{ pkgs ? import ~/src/nix-config/haskell.nix/nixpkgs.nix
, compiler-nix-name ? "ghc925"
, shell ? import ~/src/nix-config/haskell.nix/shell.nix
}:
let hsPkgs = import ./default.nix { inherit pkgs compiler-nix-name; };
    stackage = pkgs.haskell-nix.snapshots."lts-20.2";
in shell { inherit hsPkgs;
           for = with hsPkgs; [ AoC2022 ];
           buildInputs = (map (p: stackage."${p}".components.exes."${p}")
                              [ "pandoc" ]
                         ) ++ (with (import <nixpkgs> {}); [ z3 ]);
         }
