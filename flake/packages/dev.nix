{ bash, deno, elmPackages, watch, writeScriptBin }:
  writeScriptBin "dev" ''
    #! ${bash}/bin/bash
    ${watch}/bin/watch --paths flake/packages/dev.nix backend frontend/elm.json frontend/src -- nix run --no-warn-dirty .#dev.server
  '' // {
    server = writeScriptBin "server" ''
      #! ${bash}/bin/bash
      set -e
      outdir="$(pwd)/build/dev"
      rm -r "''${outdir}" 2>/dev/null || true
      mkdir -p "''${outdir}"
      cp -r backend "''${outdir}/server"
      (cd frontend && ${elmPackages.elm}/bin/elm make src/Login.elm --output=''${outdir}/server/index-unauthenticated.html)
      (cd frontend && ${elmPackages.elm}/bin/elm make src/Main.elm --output=''${outdir}/server/index-authenticated.html)
      (cd "''${outdir}/server" && TOKEN=password ${deno}/bin/deno run --unstable-kv --allow-net --allow-read --allow-env=TOKEN --check src/main.ts)
    '';
  }
