{ pkgs ? import ~/src/nix-config/haskell.nix/nixpkgs.nix
, compiler-nix-name ? "ghc9101"
, shell ? import ~/src/nix-config/haskell.nix/shell.nix
}:
let inherit (pkgs.lib) mapAttrs;
    inherit (pkgs.haskell-nix) hackage-package;
    hsPkgs = import ./default.nix { inherit pkgs compiler-nix-name; };
    stackage = pkgs.haskell-nix.snapshots."lts-22.43";
in shell {
  inherit hsPkgs;
  for = with hsPkgs; [ AoC2024 ];
  buildInputs = let hackageFromExe =
                      mapAttrs (_: p: { inherit compiler-nix-name;
                                        index-state = "2024-11-30T00:00:00Z";
                                      } // p)
                               { pandoc = { name = "pandoc-cli";
                                            version = "3.5";
                                          };
                               };
                    hackages = map (exe:
                                      ( hackage-package hackageFromExe."${exe}"
                                      ).components.exes."${exe}"
                                   );
    in ( hackages [ "pandoc" ]
       ) ++ (with (import <nixpkgs> {}); [ z3 ]);
}
