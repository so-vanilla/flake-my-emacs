{
  description = "Emacs configuration flake for home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    aide = {
      url = "github:so-vanilla/aide";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
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
        extraPkgs = {
          aide-modeline = inputs.aide.packages.${system}.aide-modeline;
          aide-session-status = inputs.aide.packages.${system}.aide-session-status;
          aide-persp-side-bar = inputs.aide.packages.${system}.aide-persp-sidebar;
        };
        mkEmacsclientApp =
          finalPackage:
          let
            launcherScript = pkgs.writeShellScript "Emacsclient" ''
              if ! "${finalPackage}/bin/emacsclient" -e "(server-running-p)" >/dev/null 2>&1; then
                "${finalPackage}/bin/emacs" --daemon
              fi
              exec "${finalPackage}/bin/emacsclient" -c "$@"
            '';
            infoPlist = pkgs.writeText "emacsclient-info-plist" ''
              <?xml version="1.0" encoding="UTF-8"?>
              <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
              <plist version="1.0">
              <dict>
                <key>CFBundleName</key>
                <string>Emacsclient</string>
                <key>CFBundleExecutable</key>
                <string>Emacsclient</string>
                <key>CFBundleIdentifier</key>
                <string>org.gnu.Emacsclient</string>
                <key>CFBundleIconFile</key>
                <string>Emacsclient</string>
                <key>CFBundlePackageType</key>
                <string>APPL</string>
                <key>CFBundleVersion</key>
                <string>1.0</string>
                <key>CFBundleShortVersionString</key>
                <string>1.0</string>
                <key>CFBundleInfoDictionaryVersion</key>
                <string>6.0</string>
              </dict>
              </plist>
            '';
          in
          pkgs.stdenv.mkDerivation {
            pname = "emacsclient-app";
            version = finalPackage.version or "1.0";
            dontUnpack = true;
            installPhase = ''
              APP="$out/Emacsclient.app"
              mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
              cp ${finalPackage}/Applications/Emacs.app/Contents/Resources/Emacs.icns \
                 "$APP/Contents/Resources/Emacsclient.icns"
              cp ${infoPlist} "$APP/Contents/Info.plist"
              cp ${launcherScript} "$APP/Contents/MacOS/Emacsclient"
              chmod +x "$APP/Contents/MacOS/Emacsclient"
            '';
          };
      in
      {
        homeManagerModules = {
          pgtk =
            { config, lib, ... }:
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacs-unstable-pgtk;
                extraPackages = import ./epkgs { inherit pkgs extraPkgs; };
              };

              home.file =
                {
                  ".emacs.d/init.el".source = ./init.el;
                  ".emacs.d/early-init.el".source = ./early-init.el;
                  ".emacs.d/templates".source = ./templates;
                  ".ddskk/init".source = ./.ddskk/init;
                  ".emacs.d/lisp".source = ./.emacs.d/lisp;
                }
                // (lib.optionalAttrs pkgs.stdenv.isDarwin {
                  "Applications/Emacsclient.app".source =
                    "${mkEmacsclientApp config.programs.emacs.finalPackage}/Emacsclient.app";
                });
            };
          stable =
            { config, lib, ... }:
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacs;
                extraPackages = import ./epkgs { inherit pkgs extraPkgs; };
              };

              home.file =
                {
                  ".emacs.d/init.el".source = ./init.el;
                  ".emacs.d/early-init.el".source = ./early-init.el;
                  ".emacs.d/templates".source = ./templates;
                  ".ddskk/init".source = ./.ddskk/init;
                  ".emacs.d/lisp".source = ./.emacs.d/lisp;
                }
                // (lib.optionalAttrs pkgs.stdenv.isDarwin {
                  "Applications/Emacsclient.app".source =
                    "${mkEmacsclientApp config.programs.emacs.finalPackage}/Emacsclient.app";
                });
            };
          macport =
            { config, lib, ... }:
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacs-macport;
                extraPackages = import ./epkgs { inherit pkgs extraPkgs; };
              };

              home.file =
                {
                  ".emacs.d/init.el".source = ./init.el;
                  ".emacs.d/early-init.el".source = ./early-init.el;
                  ".emacs.d/templates".source = ./templates;
                  ".ddskk/init".source = ./.ddskk/init;
                  ".emacs.d/lisp".source = ./.emacs.d/lisp;
                }
                // (lib.optionalAttrs pkgs.stdenv.isDarwin {
                  "Applications/Emacsclient.app".source =
                    "${mkEmacsclientApp config.programs.emacs.finalPackage}/Emacsclient.app";
                });
            };
        };
      }
    );
}
