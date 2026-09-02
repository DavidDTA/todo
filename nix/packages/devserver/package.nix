{ build, deno, writeShellApplication }:
  writeShellApplication {
    name = "devserver";
    inheritPath = false;
    runtimeInputs = [
      build
      deno
    ];
    text = ''
      outdir="$(pwd)/build/devserver"
      build "''${outdir}" "dev"
      (cd "''${outdir}/server" && TOKEN=password deno run --frozen=false --unstable-kv --allow-net --allow-read --allow-env=TOKEN --check src/scaffold.ts)
    '';
  }
