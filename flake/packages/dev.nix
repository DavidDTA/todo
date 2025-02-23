{ bash, deno, elmPackages, jq, watch, writeScriptBin }:
  writeScriptBin "dev" ''
    #! ${bash}/bin/bash
    ${watch}/bin/watch --paths flake/packages/dev.nix backend frontend/elm.json frontend/src -- nix run --no-warn-dirty .#dev.server
  '' // {
    quickfix = writeScriptBin "server" ''
      #! ${bash}/bin/bash
      set -e
      (cd frontend && {
        ${elmPackages.elm}/bin/elm make src/Login.elm --report=json --output=/dev/null >/dev/null
        ${elmPackages.elm}/bin/elm make src/Main.elm --report=json --output=/dev/null >/dev/null
      }) 2>&1 >/dev/null |
        ${jq}/bin/jq --raw-output '
          .errors[] |
          .path as $path |
          .name as $module |
          .problems[] |
          $path +
          ":" +
          $module +
          ":" +
          (.region.start.line | tostring) +
          ":" +
          (.region.start.column | tostring) +
          ":" +
          (.region.end.line | tostring) +
          ":" +
          (.region.end.column | tostring) +
          ":" +
          .title +
          "\n" +
          (
            .message |
            map(
              if type == "string" then .
              else .string
              end
            ) |
            join("")
          )
        '
    '';
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
