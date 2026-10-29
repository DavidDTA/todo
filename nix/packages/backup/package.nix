{ coreutils, curl, writeShellApplication, yq }:
  writeShellApplication {
    name = "backup";
    inheritPath = false;
    runtimeInputs = [
      coreutils
      curl
      yq
    ];
    text = ''
      usage() {
        echo "usage: backup <origin> <token-file>" >&2
        exit 1
      }

      if [ "$#" != "2" ]; then
        usage
      fi

      origin="''${1}"
      token_file="''${2}"
      curl "''${origin}/account/export" -L --fail --silent --config <(cat <<EOF
      cookie = "__Host-Http-a=$(cat "''${token_file}")"
      EOF
      ) |
        yq -y .
    '';
  }
