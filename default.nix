{ haskellPackages, writeShellScriptBin, runCommand, splitString }:

let

  shellrcSrcPath = ./.;
  shellrcModule = "ShellRC";

  shellrcSrc = shellrcSrcPath;
  shellrcModulePath = builtins.replaceStrings [ "." ] [ "/" ] shellrcModule + ".hs";

  libraries = with haskellPackages; [
    dhall
    http-conduit
    horizon-gen-nix
    horizon-spec
    horizon-spec-lens
    horizon-spec-pretty
    lens
    path
    procex
    vector
  ];

  ghc = haskellPackages.ghcWithPackages (_: libraries);

  args = builtins.concatStringsSep " " [
    "-XDataKinds"
    "-XExtendedDefaultRules"
    "-XGHC2021"
    "-XOverloadedStrings"
    "-XOverloadedLabels"
    "-Wall"
    "-Wno-type-defaults"
  ];

  shellrc = runCommand "shellrc" { } ''
    cp ${shellrcSrc} --no-preserve=all -rT $out
    ${ghc}/bin/ghc -c -dynamic --make -i"$out" ${args} $out/${shellrcModulePath}
  '';

  horizon-module-imports = runCommand "horizon-module-imports" { } ''
    grep -E '^import .*$' < ${shellrcSrc}/${shellrcModulePath} >> $out
  '';

  init = runCommand "ghci-init" { } ''
    cat > $out <<END
      :set +m -interactive-print Text.Pretty.Simple.pPrint

      :l ${shellrcModule}

      import Procex.Shell.Labels

      :set prompt-function promptFunction

      _init

      putStrLn ""

      putStrLn "  \ESC[33m\STX##     ##  #######  ########  #### ########  #######  ##    ##   \ESC[m\STX##     ##    ###     ######  ##    ## ######## ##       ##       "
      putStrLn "  \ESC[33m\STX##     ## ##     ## ##     ##  ##       ##  ##     ## ###   ##   \ESC[m\STX##     ##   ## ##   ##    ## ##   ##  ##       ##       ##       "
      putStrLn "  \ESC[33m\STX##     ## ##     ## ##     ##  ##      ##   ##     ## ####  ##   \ESC[m\STX##     ##  ##   ##  ##       ##  ##   ##       ##       ##       "
      putStrLn "  \ESC[33m\STX######### ##     ## ########   ##     ##    ##     ## ## ## ##   \ESC[m\STX######### ##     ##  ######  #####    ######   ##       ##       "
      putStrLn "  \ESC[33m\STX##     ## ##     ## ##   ##    ##    ##     ##     ## ##  ####   \ESC[m\STX##     ## #########       ## ##  ##   ##       ##       ##       "
      putStrLn "  \ESC[33m\STX##     ## ##     ## ##    ##   ##   ##      ##     ## ##   ###   \ESC[m\STX##     ## ##     ## ##    ## ##   ##  ##       ##       ##       "
      putStrLn "  \ESC[33m\STX##     ##  #######  ##     ## #### ########  #######  ##    ##   \ESC[m\STX##     ## ##     ##  ######  ##    ## ######## ######## ######## "

      putStrLn ""

      putStrLn "  \ESC[1mNOTICE: This shell supports the dhall spec at version 0.10.0 located at https://store.horizon-haskell.net/horizon-spec-0.10.0/\ESC[0m"

      putStrLn ""

      putStrLn "  The following libraries are available:"

      putStrLn ""

      ${builtins.concatStringsSep "\n" (map (x: "putStrLn \"    ${x.name}\"") libraries)}

      putStrLn ""

      putStrLn "  The following modules are loaded:"

      putStrLn ""

      ${builtins.concatStringsSep "\n" (map (x: "putStrLn \"    ${x}\"") (splitString "\n" (builtins.readFile horizon-module-imports)))}

      hz <- loadHorizon

    END
    cat ${horizon-module-imports} >> $out
  '';

in

(writeShellScriptBin "horizon-shell" ''

  home="$HOME/.local/share/ghci-shell"

  mkdir -p "$home"

  exec env GHCRTS="-c" HOME="$home" REALHOME="$HOME" ${ghc}/bin/ghci ${args} -ignore-dot-ghci -i -i${shellrc} -ghci-script ${init} "$@"

'').overrideAttrs (old: old // { passthru = { shellPath = "/bin/horizon-shell"; }; })
