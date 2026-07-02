{
  description = "";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.miscellaneous.url = github:DavidDTA/miscellaneous/main?dir=nix;

  outputs = { self, nixpkgs, miscellaneous }:
    let
      systems = ["x86_64-linux" "aarch64-linux"];
    in
    {
      devShells =
        nixpkgs.lib.attrsets.genAttrs systems (system:
          let
            nixpkgs' = import nixpkgs {
              inherit system;
              overlays = [
                miscellaneous.overlay
                (final: prev: miscellaneous.lib.mkPackages {
                  nixpkgs = final;
                  packages = ./packages;
                })
              ];
            };
          in
          {
            default =
              nixpkgs'.mkShell {
                packages = [
                  nixpkgs'.backup
                  nixpkgs'.deploy
                  nixpkgs'.devserver
                  nixpkgs'.devwatch
                  nixpkgs'.format
                  nixpkgs'.quickfix
                  nixpkgs'.deno
                  nixpkgs'.elmPackages.lamdera
                ];
              };
            }
          );
      };
}
