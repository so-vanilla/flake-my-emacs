{
  epkgs,
  inputs,
}:
with epkgs;
let
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
in
[
  copilot
  copilot-chat
  claude-code-ide
  websocket
  web-server
]
