{
  description = "";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.miscellaneous.url = github:DavidDTA/miscellaneous/master;

  outputs = { self, nixpkgs, miscellaneous }:
    let
      systems = ["x86_64-linux" "aarch64-linux"];
    in
      {
        packages =
          nixpkgs.lib.attrsets.genAttrs systems (system:
            let
              callPackage = nixpkgs.legacyPackages.${system}.newScope(miscellaneous.packages.${system} // self.outputs.packages.${system} ) ;
            in
              nixpkgs.lib.concatMapAttrs
                ( filename: type:
                  if type == "regular" && nixpkgs.lib.hasSuffix ".nix" filename then
                    let
                      packagename = nixpkgs.lib.removeSuffix ".nix" filename;
                    in
                      {
                        ${packagename} = callPackage ./flake/packages/${filename} { };
                      }
                  else
                    { }
                )
                (builtins.readDir ./flake/packages)
          );
      };
}
