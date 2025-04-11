{ bash, deno, elmPackages, git, writeScriptBin }:
    writeScriptBin "deploy" ''
      #! ${bash}/bin/bash
      set -e
      outdir="$(pwd)/build/deploy"
      rm -r "''${outdir}" 2>/dev/null || true
      mkdir -p "''${outdir}"
      cp -r backend "''${outdir}/server"
      (cd elm && ${elmPackages.elm}/bin/elm make src/Frontend/Login.elm --optimize --output=''${outdir}/server/index-unauthenticated.html)
      (cd elm && ${elmPackages.elm}/bin/elm make src/Frontend/Main.elm --optimize --output=''${outdir}/server/index-authenticated.html)
      (cd "''${outdir}/server" && ${deno}/bin/deno run --allow-sys --allow-env --allow-read --allow-write=deno.json --allow-net jsr:@deno/deployctl deploy --save-config "''${@}")
      ${git}/bin/git diff --no-index -- backend/deno.json ''${outdir}/server/deno.json
    ''
