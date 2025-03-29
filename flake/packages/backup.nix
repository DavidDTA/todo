
{ bash, curl, writeScriptBin, yq }:
    writeScriptBin "backup" ''
      #! ${bash}/bin/bash
      set -Eeuo pipefail

      origin="''${1}"
      tokenfile="''${2}"
      ${curl}/bin/curl "''${origin}/-/api/priorities" -L --cookie "__Host-d=''$(cat ''${tokenfile})" --fail --silent |
        ${yq}/bin/yq -y .
      ${curl}/bin/curl "''${origin}/-/api/waypoints" -L --cookie "__Host-d=''$(cat ''${tokenfile})" --fail --silent |
        ${yq}/bin/yq -y .
    ''
