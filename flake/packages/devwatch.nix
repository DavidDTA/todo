{ bash, watch, writeScriptBin }:
  writeScriptBin "devwatch" ''
    #! ${bash}/bin/bash
    mkdir -p build
    touch build/touchstone
    ${watch}/bin/watch --paths build/touchstone -- nix develop --command devserver
  ''
