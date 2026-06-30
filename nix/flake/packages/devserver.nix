{ bash, build, deno, writeScriptBin }:
  writeScriptBin "devserver" ''
    #! ${bash}/bin/bash
    set -Eeuo pipefail
    outdir="$(pwd)/build/devserver"
    "${build}/bin/build" "''${outdir}" "dev"
    (cd "''${outdir}/server" && TOKEN=password ${deno}/bin/deno run --frozen=false --unstable-kv --allow-net --allow-read --allow-env=TOKEN --check src/scaffold.ts)
  ''
