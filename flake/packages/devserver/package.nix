{ bash, stdenv, deno, elmPackages, writeScriptBin  }:
  writeScriptBin "devserver" ''
    #! ${bash}/bin/bash
    (cd frontend && ${elmPackages.elm}/bin/elm make src/Main.elm --output=../build/frontend/index.html)
    ${deno}/bin/deno run --allow-net --allow-read backend/src/main.ts
  ''
