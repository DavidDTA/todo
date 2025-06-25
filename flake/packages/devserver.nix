{ bash, deno, elm-to-esm, elmPackages, writeScriptBin }:
  writeScriptBin "devserver" ''
    #! ${bash}/bin/bash
    set -e
    add_devtools() {
      sed -i '/^<\/head>$/ s#^#<script src="https://cdn.jsdelivr.net/npm/eruda"></script><script>eruda.init();</script>#' "''${1}"
    }
    outdir="$(pwd)/build/dev"
    rm -r "''${outdir}" 2>/dev/null || true
    mkdir -p "''${outdir}"
    cp -r backend "''${outdir}/server"
    (cd elm && ${elmPackages.elm}/bin/elm make src/Frontend/Login.elm --output="''${outdir}/server/index-unauthenticated.html")
    add_devtools "''${outdir}/server/index-unauthenticated.html"
    (cd elm && ${elmPackages.elm}/bin/elm make src/Frontend/Main.elm --output="''${outdir}/server/index-authenticated.html")
    add_devtools "''${outdir}/server/index-authenticated.html"
    (cd elm && ${elmPackages.elm}/bin/elm make src/Backend/Main.elm --output="''${outdir}/server/src/elm/main.js")
    ${elm-to-esm}/bin/elm-to-esm "''${outdir}/server/src/elm/main.js"
    (cd "''${outdir}/server" && TOKEN=password ${deno}/bin/deno run --frozen=false --unstable-kv --allow-net --allow-read --allow-env=TOKEN --check src/scaffold.ts)
  ''
