{ coreutils, writeShellApplication }:
  writeShellApplication {
    name = "elm-to-esm";
    inheritPath = false;
    runtimeInputs = [
      coreutils
    ];
    text = ''
      file="''${1}"

      content="export const Elm = function(){$(<"''${file}")return this;}.call({}).Elm;"
      cat <<<"''${content}" >"''${file}"
    '';
  }
