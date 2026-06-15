{
  description = "Terminal Emacs configuration flake for home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ (import inputs.emacs-overlay) ];
        };

        lib = pkgs.lib;

        sharedLibrarySuffix = if pkgs.stdenv.isDarwin then "dylib" else "so";

        treeSitterGrammarBundle =
          let
            grammars = with pkgs.tree-sitter-grammars; {
              bash = tree-sitter-bash;
              dockerfile = tree-sitter-dockerfile;
              go = tree-sitter-go;
              gomod = tree-sitter-gomod;
              hcl = tree-sitter-hcl;
              java = tree-sitter-java;
              json = tree-sitter-json;
              lua = tree-sitter-lua;
              markdown = tree-sitter-markdown;
              markdown-inline = tree-sitter-markdown-inline;
              nix = tree-sitter-nix;
              python = tree-sitter-python;
              toml = tree-sitter-toml;
              yaml = tree-sitter-yaml;
            };
          in
          pkgs.runCommand "emacs-tree-sitter-grammars" { } ''
            mkdir -p "$out"
            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (
                language: grammar:
                ''ln -s ${grammar}/parser "$out/libtree-sitter-${language}.${sharedLibrarySuffix}"''
              ) grammars
            )}
          '';

        languageTools =
          with pkgs;
          [
            basedpyright
            bash-language-server
            deadnix
            dockerfile-language-server
            emacs-lsp-booster
            fd
            fzf
            git
            google-java-format
            gopls
            gotools
            hadolint
            jdt-language-server
            luajitPackages.luacheck
            lua-language-server
            markdownlint-cli
            marksman
            nixd
            nixfmt
            nodejs_24
            prettier
            ripgrep
            ruff
            shellcheck
            shfmt
            statix
            stylua
            taplo
            terraform-ls
            tflint
            vscode-langservers-extracted
            yaml-language-server
          ]
          ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.wl-clipboard ];

        weztermConfig = pkgs.writeText "wezterm.lua" (builtins.readFile ./wezterm/wezterm.lua);
        windowsWeztermConfig = pkgs.writeText "windows.wezterm.lua" (
          builtins.readFile ./wezterm/windows.wezterm.lua
        );

        homeManagerModule =
          { lib, ... }:
          {
            home.packages = languageTools;

            home.sessionVariables = {
              EDITOR = lib.mkOverride 900 "emacsclient -t";
              VISUAL = lib.mkOverride 900 "emacsclient -t";
              ALTERNATE_EDITOR = lib.mkDefault "";
            };

            programs.emacs = {
              enable = true;
              package = pkgs.emacs-unstable-nox;
              extraPackages = import ./epkgs { inherit pkgs; };
            };

            programs.wezterm = {
              enable = true;
              package = pkgs.wezterm;
              extraConfig = builtins.readFile ./wezterm/wezterm.lua;
            };

            services.emacs = {
              enable = true;
              client.enable = true;
            };

            home.file = {
              ".emacs.d/init.el".source = ./init.el;
              ".emacs.d/early-init.el".source = ./early-init.el;
              ".emacs.d/tree-sitter-grammars".source = treeSitterGrammarBundle;
            };
          };
      in
      {
        packages = {
          default = pkgs.emacs-unstable-nox;
          emacs = pkgs.emacs-unstable-nox;
          tree-sitter-grammars = treeSitterGrammarBundle;
          wezterm-config = weztermConfig;
          windows-wezterm-config = windowsWeztermConfig;
        };

        homeManagerModules.default = homeManagerModule;
      }
    );
}
