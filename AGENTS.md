# Agent Guidelines for .emacs.d

## Language & Style
- **Language**: Emacs Lisp
- **Formatting**: Use standard Emacs Lisp conventions with 2-space indentation
- **Comments**: Use double semicolons (;;) for full-line comments, single (;) for inline

## Code Conventions
- **Package declarations**: Always use `use-package` for package configuration
- **Keybindings**: Define custom keymaps for logical grouping (see `my-lsp-bridge-keymap`, `my-gptel-keymap`)
- **Custom functions**: Prefix custom functions with `my-` to distinguish from built-in/package functions
- **Interactive functions**: Mark user-facing functions with `(interactive)`
- **Hooks**: Prefer `:hook` in `use-package` declarations over manual `add-hook`
- **Config**: Use `:config` for code that runs after package loads, `:init` for pre-load setup

## Configuration Structure
- **Main file**: `init.el` contains all configuration
- **Custom file**: Auto-generated customizations go to `custom.el` (line 140-143)
- **Snippets**: Located in `~/.emacs.d/snippets/` directory
- **No tests**: This is a configuration repo; manual testing in Emacs is expected

## Key Dependencies
- LSP: Uses `lsp-bridge` from `~/lsp-bridge` (external git repo)
- Completion: `vertico`, `consult`, `orderless` stack
- Project management: `projectile` with persistent caching
