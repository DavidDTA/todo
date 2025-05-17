{ bash, watch, writeScriptBin }:
  writeScriptBin "devwatch" ''
    #! ${bash}/bin/bash
    ${watch}/bin/watch --paths flake/packages/dev.nix flake/packages/elm-to-esm.nix backend elm/elm.json elm/src -- nix develop --command devserver
  ''
