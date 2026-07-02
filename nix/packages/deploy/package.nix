{ bash, build, coreutils, deno, git, writeShellApplication }:
  writeShellApplication {
    name = "deploy";
    inheritPath = false;
    runtimeInputs = [
      build
      coreutils
      deno
    ];
    text = ''
      if [ "''${#}" != 3 ]; then
        echo "usage: deploy <token> <org> <app>"
        exit 1
      fi
      token="''${1}"
      org="''${2}"
      app="''${3}"
      outdir="$(pwd)/build/deploy"
      build "''${outdir}" "prod"
      (cd "''${outdir}/server" && deno deploy --non-interactive --token "''${token}" --org "''${org}" --app "''${app}" --prod)
    '';
  }
