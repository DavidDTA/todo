{
  description = "";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.miscellaneous.url = github:DavidDTA/miscellaneous/master;

  outputs = { self, nixpkgs, miscellaneous }:
    let
      system = "x86_64-linux";
      callPackage = nixpkgs.legacyPackages.${system}.newScope(miscellaneous.packages.${system} // self.outputs.packages.${system} ) ;
    in
      {
        packages.${system} = builtins.mapAttrs
        (dirname: _: callPackage ./flake/packages/${dirname}/package.nix { })
        (builtins.readDir ./flake/packages);
      };
}
