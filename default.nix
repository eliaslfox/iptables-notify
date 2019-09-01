{ pkgs, ... }:
pkgs.haskellPackages.callCabal2nix "iptables-notify" ./. {}
