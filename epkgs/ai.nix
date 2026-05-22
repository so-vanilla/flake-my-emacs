{
  epkgs,
  inputs,
}:
with epkgs;
let
  claude-code = epkgs.trivialBuild {
    pname = "claude-code";
    version = "0-unstable";
    src = inputs.claude-code-el;
    packageRequires = [
      transient
      inheritenv
    ];
  };
  claude-code-ide = epkgs.trivialBuild {
    pname = "claude-code-ide";
    version = "0-unstable";
    src = inputs.claude-code-ide-el;
    packageRequires = [
      websocket
      transient
      web-server
    ];
  };
  ghostel-module = epkgs.ghostel.passthru.module.overrideAttrs (_: {
    src = inputs.ghostel-src;
  });
  ghostel-latest = epkgs.ghostel.overrideAttrs (_: {
    src = inputs.ghostel-src;
    preBuild = ''
      install ${ghostel-module}/lib/libghostel-module.so ghostel-module.so
    '';
  });
in
[
  copilot
  copilot-chat
  ghostel-latest
  inheritenv
  claude-code
  claude-code-ide
  websocket
  web-server
]
