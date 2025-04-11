
{ bash, elmPackages, writeScriptBin }:
    writeScriptBin "format" ''
      #! ${bash}/bin/bash
      set -e
      cd elm
      ${elmPackages.elm-format}/bin/elm-format .
    ''
