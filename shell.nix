let
  hs-nix = builtins.fetchTarball
    ( "https://github.com/input-output-hk/haskell.nix/archive/"
    + "2e7a9925f5922ec785b0782e6b1457166dcb127c.tar.gz"
    );
  nixpkgs = builtins.fetchTarball
    ( "https://github.com/input-output-hk/nixpkgs/archive/"
    + "a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz"
    );
  pkgs = import nixpkgs (import hs-nix);
  haskell-nix = pkgs.haskell-nix;
  hackage-package = haskell-nix.hackage-package;
in
pkgs.stdenv.mkDerivation {
  name = "hs-dev";
  buildInputs =
    (with pkgs; # Packages that don't work from hsPkgs for some reason
    [ cabal-install
    ]
    ) ++ (with haskell-nix.haskellPackages; # Packages in the overlay
    [ ghcid.components.exes.ghcid
      (ghcWithPackages (ps: with ps;
        [ # containers
          megaparsec
        ]))
      pandoc.components.exes.pandoc
    ]
    ) ++ ( # Packages not in stackage-lts
    [ (hackage-package { name = "fast-tags";
                         version = "2.0.0"; }).components.exes.fast-tags
    ]);
}
