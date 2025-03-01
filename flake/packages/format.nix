
{ bash, elmPackages, writeScriptBin }:
    writeScriptBin "format" ''
      #! ${bash}/bin/bash
      set -e
      cd frontend
      ${elmPackages.elm-format}/bin/elm-format .
    ''
