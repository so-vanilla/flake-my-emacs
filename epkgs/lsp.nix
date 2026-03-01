{
  epkgs
}:
with epkgs; [
  (lsp-bridge.overrideAttrs (_: { doCheck = false; }))
  # lsp-mode
  # lsp-pyright
  # lsp-java
  # lsp-ui
]
