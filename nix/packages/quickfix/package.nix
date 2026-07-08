{ coreutils, elmPackages, jq, writeShellApplication }:
  writeShellApplication {
    name = "quickfix";
    inheritPath = false;
    runtimeInputs = [
      coreutils
      elmPackages.lamdera
      jq
    ];
    text = ''
      pushd elm >/dev/null
      regular_exit_code=0
      regular_output="$(lamdera make src/Frontend/Login.elm src/Frontend/Main.elm src/Backend/Main.elm --report=json --output=/dev/null 3>&1 >/dev/null 2>&3)" || regular_exit_code="$?"
      no_wire_exit_code=0
      no_wire_output="$(lamdera make --no-wire src/Frontend/Login.elm src/Frontend/Main.elm src/Backend/Main.elm --report=json --output=/dev/null 3>&1 >/dev/null 2>&3)" || no_wire_exit_code="$?"
      popd >/dev/null
      echo "$no_wire_output" "$regular_output" | jq --slurp --raw-output '
        [
          .[] |
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
        ] |
        sort_by(split("\n") | .[1] | contains("`w3_")) |
        .[]
      '
      mkdir -p build
      touch build/touchstone
      [ "$no_wire_exit_code" = "0" ] && [ "$regular_exit_code" = "0" ]
    '';
  }
