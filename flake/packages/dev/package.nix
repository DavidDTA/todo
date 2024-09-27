{ writeScriptBin, bash, watch }:
  writeScriptBin "dev" ''
#! ${bash}/bin/bash
${watch}/bin/watch --paths flake/packages/devserver backend frontend/elm.json frontend/src -- nix run .#devserver
''
