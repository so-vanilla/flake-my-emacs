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
    let
      emacsOverlay = import inputs.emacs-overlay;

      mkPkgs =
        system:
        import inputs.nixpkgs {
          inherit system;
          overlays = [ emacsOverlay ];
        };

      mkTreeSitterGrammarBundle =
        pkgs:
        let
          lib = pkgs.lib;
          sharedLibrarySuffix = if pkgs.stdenv.isDarwin then "dylib" else "so";

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

      mkLanguageTools =
        pkgs:
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
        ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.wl-clipboard ];

      mkTerminalTools = pkgs: with pkgs; [ zellij ];

      mkEmacsProjectDaemon =
        pkgs: emacsPackage:
        pkgs.writeShellApplication {
          name = "emacs-project-daemon";
          runtimeInputs = [
            emacsPackage
            pkgs.coreutils
            pkgs.flock
            pkgs.git
          ];
          text = builtins.readFile ./scripts/emacs-project-daemon.sh;
        };

      mkEmacsclientSmart =
        pkgs: emacsPackage: emacsProjectDaemon:
        pkgs.writeShellApplication {
          name = "emacsclient-smart";
          runtimeInputs = [
            emacsPackage
            emacsProjectDaemon
          ];
          text = builtins.readFile ./scripts/emacsclient-smart.sh;
        };

      mkWeztermConfigText =
        pkgs:
        ''
          local MY_EMACS_ZELLIJ = "${pkgs.zellij}/bin/zellij"
        ''
        + builtins.readFile ./wezterm/wezterm.lua;
      mkWeztermConfig = pkgs: pkgs.writeText "wezterm.lua" (mkWeztermConfigText pkgs);
      mkWindowsWeztermConfig =
        pkgs: pkgs.writeText "windows.wezterm.lua" (builtins.readFile ./wezterm/windows.wezterm.lua);
      mkZellijConfig = pkgs: pkgs.writeText "config.kdl" (builtins.readFile ./zellij/config.kdl);
      mkZellijWorkspaceLayout =
        pkgs: pkgs.writeText "workspace.kdl" (builtins.readFile ./zellij/layouts/workspace.kdl);

      homeManagerModule =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          modulePkgs = mkPkgs pkgs.stdenv.hostPlatform.system;
          treeSitterGrammarBundle = mkTreeSitterGrammarBundle modulePkgs;
          emacsProjectDaemon =
            mkEmacsProjectDaemon modulePkgs config.programs.emacs.finalPackage;
          emacsclientSmart =
            mkEmacsclientSmart modulePkgs config.programs.emacs.finalPackage emacsProjectDaemon;
        in
        {
          home.packages =
            mkLanguageTools modulePkgs
            ++ mkTerminalTools modulePkgs
            ++ [
              emacsProjectDaemon
              emacsclientSmart
            ];

          home.sessionVariables = {
            EDITOR = lib.mkOverride 900 "emacsclient-smart -t";
            VISUAL = lib.mkOverride 900 "emacsclient-smart -t";
          };

          programs.emacs = {
            enable = true;
            package = modulePkgs.emacs-unstable-nox;
            extraPackages = import ./epkgs { pkgs = modulePkgs; };
          };

          programs.wezterm = {
            enable = true;
            package = modulePkgs.wezterm;
            extraConfig = mkWeztermConfigText modulePkgs;
          };

          services.emacs = {
            enable = lib.mkForce false;
          };

          home.file = {
            ".emacs.d/init.el".source = ./init.el;
            ".emacs.d/early-init.el".source = ./early-init.el;
            ".emacs.d/tree-sitter-grammars".source = treeSitterGrammarBundle;
            ".config/zellij/config.kdl".source = ./zellij/config.kdl;
            ".config/zellij/layouts/workspace.kdl".source = ./zellij/layouts/workspace.kdl;
          };
        };
      perSystemOutputs = inputs.flake-utils.lib.eachDefaultSystem (
        system:
        let
          pkgs = mkPkgs system;
          treeSitterGrammarBundle = mkTreeSitterGrammarBundle pkgs;
          configuredEmacs =
            (pkgs.emacsPackagesFor pkgs.emacs-unstable-nox).emacsWithPackages (
              import ./epkgs { inherit pkgs; }
            );
          emacsProjectDaemon = mkEmacsProjectDaemon pkgs configuredEmacs;
          emacsclientSmart = mkEmacsclientSmart pkgs configuredEmacs emacsProjectDaemon;
        in
        {
          packages = {
            default = pkgs.emacs-unstable-nox;
            emacs = pkgs.emacs-unstable-nox;
            tree-sitter-grammars = treeSitterGrammarBundle;
            wezterm-config = mkWeztermConfig pkgs;
            windows-wezterm-config = mkWindowsWeztermConfig pkgs;
            zellij-config = mkZellijConfig pkgs;
            zellij-workspace-layout = mkZellijWorkspaceLayout pkgs;
            emacs-project-daemon = emacsProjectDaemon;
            emacsclient-smart = emacsclientSmart;
          };

          homeManagerModules.default = homeManagerModule;
        }
      );
    in
    perSystemOutputs
    // {
      homeManagerModules = perSystemOutputs.homeManagerModules // {
        default = homeManagerModule;
      };
    };
}
