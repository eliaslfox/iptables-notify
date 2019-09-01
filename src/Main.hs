{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified GHC.IO.Handle as Handle
import qualified System.Process as Process
import qualified GHC.IO.Handle as Handle
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Text.Regex as Regex

import Text.RawString.QQ (r)
import Data.Monoid ((<>))

mFork :: IO () -> IO (MVar.MVar ())
mFork m = do
    mvar <- MVar.newEmptyMVar
    _ <- Concurrent.forkIO $ do
        m
        MVar.putMVar mvar ()
    pure mvar


regex = Regex.mkRegex [r|\[[0-9]+\.[0-9]+\] refused connection\: IN=([A-z0-9]*) OUT=([A-z0-9]*) MAC=([A-z0-9\:]*) SRC=([A-z0-9\.\:]*) DST=([A-z0-9\.\:]*) LEN=[0-9]* TOS=0x[0-9]* PREC=0x[0-9]* TTL=[0-9]* ID=[0-9]* DF PROTO=(TCP|UDP) SPT=[0-9]* DPT=([0-9]*)|]

readThread :: IO ()
readThread =
    let
        process = (Process.proc "/run/current-system/sw/bin/dmesg" ["-w"])
           { Process.std_out = Process.CreatePipe
           , Process.std_err = Process.CreatePipe
           }

    in
        Process.withCreateProcess process $ \_ (Just stdout) (Just stderr) _ -> do
           handleLine stdout

    where
        handleLine stdout = do
            x <- Handle.hGetLine stdout
            processLine x
            handleLine stdout

        processLine line = do
            case Regex.matchRegex regex line of
                Just [in_, out_, mac_, src, dst, proto, port] ->
                    let
                        str = "Blocked " <> proto <> " packet from " <> src <> " on port " <> port
                    in do
                        Process.createProcess $ (Process.proc "/home/elf/.nix-profile/bin/notify-send" [str])
                            { Process.std_out = Process.Inherit
                            , Process.std_err = Process.Inherit
                            }
                        putStrLn str


                Nothing ->
                    pure ()

main :: IO ()
main = do
    putStrLn "Starting"
    mvar <- mFork readThread
    _ <- MVar.takeMVar mvar
    pure ()
