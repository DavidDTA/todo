{ coreutils, nix, watch, writeShellApplication }:
  writeShellApplication {
    name = "devwatch";
    inheritPath = false;
    runtimeInputs = [
      coreutils
      nix
      watch
    ];
    text = ''
      mkdir -p build
      touch build/touchstone
      watch --paths build/touchstone -- nix develop --command devserver
    '';
  }
