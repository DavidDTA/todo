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
      (cd frontend && ${elmPackages.elm}/bin/elm make src/Main.elm --output=''${outdir}/server/index.html)
      (cd "''${outdir}/server" && ${deno}/bin/deno run --unstable-kv --allow-net --allow-read src/main.ts)
    '';
  }
