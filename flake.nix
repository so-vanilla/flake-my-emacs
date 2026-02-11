{
  description = "Emacs configuration flake for home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-my-claude = {
      url = "github:so-vanilla/flake-my-claude";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [(import inputs.emacs-overlay)];
        };
        extraPkgs = {
          claude-code-modeline = inputs.flake-my-claude.packages.${system}.claude-code-modeline;
        };
      in
      {
        homeManagerModules = {
          pgtk = { ... }:
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacs-unstable-pgtk;
                extraPackages = import ./epkgs { inherit pkgs extraPkgs; };
              };

              home.file = {
                ".emacs.d/init.el".source = ./init.el;
                ".emacs.d/early-init.el".source = ./early-init.el;
                ".emacs.d/templates".source = ./templates;
                ".ddskk/init".source = ./.ddskk/init;
                ".emacs.d/lisp".source = ./.emacs.d/lisp;
              };
            };
          stable = { ... }:
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacs;
                extraPackages = import ./epkgs { inherit pkgs extraPkgs; };
              };

              home.file = {
                ".emacs.d/init.el".source = ./init.el;
                ".emacs.d/early-init.el".source = ./early-init.el;
                ".emacs.d/templates".source = ./templates;
                ".ddskk/init".source = ./.ddskk/init;
                ".emacs.d/lisp".source = ./.emacs.d/lisp;
              };
            };
          macport = { ... }:
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacs-macport;
                extraPackages = import ./epkgs { inherit pkgs extraPkgs; };
              };
              
              home.file = {
                ".emacs.d/init.el".source = ./init.el;
                ".emacs.d/early-init.el".source = ./early-init.el;
                ".emacs.d/templates".source = ./templates;
                ".ddskk/init".source = ./.ddskk/init;
                ".emacs.d/lisp".source = ./.emacs.d/lisp;
              };
            };
        };
      }
    );
}
