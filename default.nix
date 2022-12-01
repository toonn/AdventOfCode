{ pkgs ? import ~/src/nix-config/haskell.nix/nixpkgs.nix
, compiler-nix-name ? "ghc925"
}:
pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  src = pkgs.haskell-nix.cleanSourceHaskell {
    name = "AdventOfCode-source";
    src = ./.;
  };
}
