{ pkgs ? import ~/src/nix-config/haskell.nix/nixpkgs.nix
, compiler-nix-name ? "ghc8102"
}:
pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    name = "AdventOfCode-source";
    src = ./.;
    filter = path: type:
      (builtins.match "LICENSE|.*\.(cabal|hs)|cabal\.project.*|2020" (baseNameOf path)) != null;
  };
}
