{ bash, deno, elmPackages, git, writeScriptBin }:
    writeScriptBin "deploy" ''
      #! ${bash}/bin/bash
      set -e
      outdir="$(pwd)/build/deploy"
      rm -r "''${outdir}" 2>/dev/null || true
      mkdir -p "''${outdir}"
      cp -r backend "''${outdir}/server"
      (cd frontend && ${elmPackages.elm}/bin/elm make src/Login.elm --optimize --output=''${outdir}/server/index-unauthenticated.html)
      (cd frontend && ${elmPackages.elm}/bin/elm make src/Main.elm --optimize --output=''${outdir}/server/indexauthenticated.html)
      (cd "''${outdir}/server" && ${deno}/bin/deno run --allow-sys --allow-env --allow-read --allow-net jsr:@deno/deployctl deploy --save-dev "''${@}")
      ${git}/bin/git diff backend/deno.json build/deploy/server/deno.json
    ''
