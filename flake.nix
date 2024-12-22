{
  description = "";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.miscellaneous.url = github:DavidDTA/miscellaneous/master;

  outputs = { self, nixpkgs, miscellaneous }:
    let
      system = "x86_64-linux";
      callPackage = nixpkgs.legacyPackages.${system}.newScope(miscellaneous.packages.${system} // self.outputs.packages.${system} ) ;
    in
      {
        packages.${system} =
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
            (builtins.readDir ./flake/packages);
      };
}
