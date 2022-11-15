{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [ pkgs.git pkgs.ormolu ];

  enterShell = ''
    git --version
  '';

  # https://devenv.sh/languages/
  languages.nix.enable = true;
  languages.haskell.enable = true;

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo hello from $GREET";
  scripts.test-ghcid.exec = ''
    ghcid -c "cabal repl"
  '';

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks.shellcheck.enable = true;
  pre-commit.hooks.hlint.enable = true;
  pre-commit.hooks.ormolu.enable = true;
  pre-commit.hooks.cabal-fmt.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";
}
