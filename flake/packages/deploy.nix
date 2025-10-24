{ bash, build, deno, git, writeScriptBin }:
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
      "${build}/bin/build" "''${outdir}" "prod"
      (cd "''${outdir}/server" && ${deno}/bin/deno run --allow-sys --allow-env --allow-read --allow-write=deno.json --allow-write=~/.deno/deployctl --allow-net jsr:@deno/deployctl deploy --save-config --project "''${project}")
      ${git}/bin/git diff --no-index -- backend/deno.json ''${outdir}/server/deno.json
    ''
