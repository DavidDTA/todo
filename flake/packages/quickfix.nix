{ bash, elmPackages, jq, writeScriptBin }:
  writeScriptBin "quickfix" ''
    #! ${bash}/bin/bash
    set -eEuo pipefail
    (cd elm && {
      ${elmPackages.lamdera}/bin/lamdera make --no-wire src/Frontend/Login.elm src/Frontend/Main.elm src/Backend/Main.elm --output=/dev/null >/dev/null 2>/dev/null || true
      ${elmPackages.lamdera}/bin/lamdera make src/Frontend/Login.elm --report=json --output=/dev/null >/dev/null
      ${elmPackages.lamdera}/bin/lamdera make src/Frontend/Main.elm --report=json --output=/dev/null >/dev/null
      ${elmPackages.lamdera}/bin/lamdera make src/Backend/Main.elm --report=json --output=/dev/null >/dev/null
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
  ''
