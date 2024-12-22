{ bash, deno, elmPackages, watch, writeScriptBin }:
  writeScriptBin "dev" ''
    #! ${bash}/bin/bash
    ${watch}/bin/watch --paths flake/packages/dev backend frontend/elm.json frontend/src -- nix run --no-warn-dirty .#dev.server
  '' // {
    server = writeScriptBin "server" ''
      #! ${bash}/bin/bash
      set -e
      (cd frontend && ${elmPackages.elm}/bin/elm make src/Main.elm --output=../build/frontend/index.html)
      ${deno}/bin/deno run --unstable-kv --allow-net --allow-read backend/src/main.ts
    '';
  }
