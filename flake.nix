{
  description = "Emacs configuration flake for home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [(import inputs.emacs-overlay)];
        };

        epkgsFn = import ./epkgs { inherit pkgs; };

        mkEmacs = baseEmacs:
          (pkgs.emacsPackagesFor baseEmacs).emacsWithPackages epkgsFn;

        configFiles = {
          ".emacs.d/init.el".source = ./init.el;
          ".emacs.d/early-init.el".source = ./early-init.el;
          ".emacs.d/templates".source = ./templates;
          ".ddskk/init".source = ./.ddskk/init;
          ".emacs.d/lisp".source = ./.emacs.d/lisp;
        };

        mkHmModule = baseEmacs: { ... }: {
          programs.emacs = {
            enable = true;
            package = baseEmacs;
            extraPackages = epkgsFn;
          };
          home.file = configFiles;
        };
      in
      {
        packages = {
          emacs-pgtk = mkEmacs pkgs.emacs-unstable-pgtk;
          emacs-stable = mkEmacs pkgs.emacs;
          default = mkEmacs pkgs.emacs;
        };

        homeManagerModules = {
          pgtk = mkHmModule pkgs.emacs-unstable-pgtk;
          stable = mkHmModule pkgs.emacs;
          macport = mkHmModule pkgs.emacs-macport;
        };
      }
    );
}
