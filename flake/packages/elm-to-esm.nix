{ bash, writeScriptBin }:
    writeScriptBin "elm-to-esm" ''
      #! ${bash}/bin/bash
      set -e
      file="''${1}"
      cat <<<"export const Elm = function(){$(<"''${file}")return this;}.call({}).Elm;" >"''${file}"
    ''
