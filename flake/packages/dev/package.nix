{ writeScriptBin, bash, watch }:
  writeScriptBin "dev" ''
#! ${bash}/bin/bash
${watch}/bin/watch --paths . -- nix run .#devserver
''
