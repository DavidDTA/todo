{ bash, deno, elm-to-esm, elmPackages, git, writeScriptBin }:
    writeScriptBin "deploy" ''
      #! ${bash}/bin/bash
      set -Eeuo pipefail

      project="''${1:-}"
      if [ -z "''${project}" ]; then
        echo "You must specify a project. Here are your existing projects:"
        ${deno}/bin/deno run --allow-sys --allow-env --allow-read --allow-write=deno.json --allow-write=~/.deno --allow-net jsr:@deno/deployctl projects list
        exit 1
      fi
      outdir="$(pwd)/build/deploy"
      rm -r "''${outdir}" 2>/dev/null || true
      mkdir -p "''${outdir}"
      cp -r backend "''${outdir}/server"
      (cd elm && ${elmPackages.elm}/bin/elm make src/Frontend/Login.elm --optimize --output="''${outdir}/server/index-unauthenticated.html")
      (cd elm && ${elmPackages.elm}/bin/elm make src/Frontend/Main.elm --optimize --output="''${outdir}/server/index-authenticated.html")
      (cd elm && ${elmPackages.elm}/bin/elm make src/Backend/Main.elm --optimize --output="''${outdir}/server/src/elm/main.js")
      ${elm-to-esm}/bin/elm-to-esm "''${outdir}/server/src/elm/main.js"
      (cd "''${outdir}/server" && ${deno}/bin/deno run --allow-sys --allow-env --allow-read --allow-write=deno.json --allow-write=~/.deno/deployctl --allow-net jsr:@deno/deployctl deploy --save-config --project "''${project}")
      ${git}/bin/git diff --no-index -- backend/deno.json ''${outdir}/server/deno.json
    ''
