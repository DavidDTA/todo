{ bash, stdenv, deno, writeScriptBin  }:
  writeScriptBin "devseever" ''
    #! ${bash}/bin/bash
    ${deno}/bin/deno run --allow-net backend/src/main.ts
  ''
