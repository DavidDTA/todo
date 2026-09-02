{ elmPackages, writeShellApplication }:
  writeShellApplication {
    name = "format";
    inheritPath = false;
    runtimeInputs = [
      elmPackages.elm-format
    ];
    text = ''
      cd elm
      elm-format --yes .
    '';
  }
