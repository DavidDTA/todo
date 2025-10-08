{
  description = "";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.miscellaneous.url = github:DavidDTA/miscellaneous/master;

  outputs = { self, nixpkgs, miscellaneous }:
    let
      systems = ["x86_64-linux" "aarch64-linux"];
    in
    {
      devShells =
        nixpkgs.lib.attrsets.genAttrs systems (system:
          let
            nixpkgs' = nixpkgs.legacyPackages.${system};
            callPackage = nixpkgs'.newScope(miscellaneous.packages.${system} // myPkgs) ;
            myPkgs =
              nixpkgs.lib.attrsets.concatMapAttrs
                (filename: type:
                  if type == "regular" && nixpkgs.lib.hasSuffix ".nix" filename then
                   { ${nixpkgs.lib.removeSuffix ".nix" filename} = callPackage ./flake/packages/${filename} { }; }
                  else
                    {}
                )
                (builtins.readDir ./flake/packages);
          in
          {
            default =
              nixpkgs'.mkShell {
                packages =
                  builtins.attrValues myPkgs ++ [
                  nixpkgs'.deno
                  nixpkgs'.elmPackages.elm
                ];
              };
            }
          );
      };
}
