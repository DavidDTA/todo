{ bash, elm-to-esm, elmPackages, writeScriptBin }:
  let
    write-index = writeScriptBin "write-index" ''
      #! ${bash}/bin/bash
      set -Eeuo pipefail
      entry="''${1}"
      devtools="''${2}"
      cat << EOF
      <!DOCTYPE HTML>
      <html>
      <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1,interactive-widget=resizes-content">
      <title>Working on it!</title>
      ''${devtools}
      <script src="/-/app.js"></script>
      <script>
      (function(){
      document.addEventListener("DOMContentLoaded", (event) => {
      var elm = Elm.Frontend.''${entry}.init({ node: document.body });
      });
      })();
      </script>
      </head>
      <body></body>
      </html>
      EOF
    '';
  in
  writeScriptBin "build" ''
    #! ${bash}/bin/bash
    set -Eeuo pipefail
    outdir="''${1}"
    environment="''${2}"
    case "''${environment}" in
      "prod")
        devtools=""
        elm_flags=("--optimize")
      ;;
      "dev")
        devtools='<script src="https://cdn.jsdelivr.net/npm/eruda"></script><script>eruda.init();</script>'
        elm_flags=()
      ;;
      *)
        exit 1
      ;;
    esac
    rm -r "''${outdir}" 2>/dev/null || true
    mkdir -p "''${outdir}"
    cp -r backend "''${outdir}/server"
    mkdir -p "''${outdir}/server/files"
    (cd elm && ${elmPackages.lamdera}/bin/lamdera make src/Frontend/Login.elm src/Frontend/Main.elm "''${elm_flags[@]}" --output="''${outdir}/server/files/app.js")
    "${write-index}/bin/write-index" Login "''${devtools}" > "''${outdir}/server/files/index-unauthenticated.html"
    "${write-index}/bin/write-index" Main "''${devtools}" > "''${outdir}/server/files/index-authenticated.html"
    (cd elm && ${elmPackages.lamdera}/bin/lamdera make src/Backend/Main.elm "''${elm_flags[@]}" --output="''${outdir}/server/src/elm/main.js")
    ${elm-to-esm}/bin/elm-to-esm "''${outdir}/server/src/elm/main.js"
  ''
