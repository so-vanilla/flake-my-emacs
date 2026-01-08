{
  description = "Emacs configuration flake for home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { ... }@inputs:
    {
      homeManagerModules.default = { system, ...}:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [(import inputs.emacs-overlay)];
          };
        in
          {
        programs.emacs = {
          enable = true;
          package = pkgs.emacs-unstable-pgtk;
          extraPackages = import ./epkgs { inherit pkgs; };
        };

        home.file = {
          ".emacs.d/init.el".source = ./init.el;
          ".emacs.d/early-init.el".source = ./early-init.el;
          ".emacs.d/templates".source = ./templates;
          ".ddskk/init".source = ./.ddskk/init;
        };
      };
    };
}
