let nixpkgs = import <nixpkgs> {};
in nixpkgs.haskellPackages.callCabal2nix "iptables-notify" ./. {}
