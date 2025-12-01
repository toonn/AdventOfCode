{ pkgs ? import ~/src/nix-config/haskell.nix/nixpkgs.nix
, compiler-nix-name ? "ghc9122"
, shell ? import ~/src/nix-config/haskell.nix/shell.nix
}:
let inherit (pkgs.lib) mapAttrs;
    inherit (pkgs.haskell-nix) hackage-package;
    hsPkgs = import ./default.nix { inherit pkgs compiler-nix-name; };
    stackage = pkgs.haskell-nix.snapshots."lts-24.22";
in shell {
  inherit hsPkgs;
  for = with hsPkgs; [ AoC2025 ];
  buildInputs = let hackageFromExe =
                      mapAttrs (_: p: { inherit compiler-nix-name;
                                        index-state = "2025-11-30T00:00:00Z";
                                      } // p)
                               { pandoc = { name = "pandoc-cli";
                                            version = "3.8.2.1";
                                          };
                               };
                    hackages = map (exe:
                                      ( hackage-package hackageFromExe."${exe}"
                                      ).components.exes."${exe}"
                                   );
    in ( hackages [ "pandoc" ]
       ) ++ (with (import <nixpkgs> {}); [ z3 ]);
}
