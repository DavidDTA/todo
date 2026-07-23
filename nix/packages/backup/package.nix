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
        echo "usage: backup <origin> <token>" >&2
        exit 1
      }

      if [ "$#" != "2" ]; then
        usage
      fi

      origin="''${1}"
      token="''${2}"
      curl "''${origin}/account/export" -L --cookie "__Host-Http-a=''${token}" --fail --silent |
        yq -y .
    '';
  }
