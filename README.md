# Claude Code IDE MCP Tools

## Extended MCP tools for Claude Code IDE with backend-agnostic design

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-28.1+-purple.svg)](https://www.gnu.org/software/emacs/)

This package extends [Claude Code IDE](https://github.com/manzaltu/claude-code-ide.el) with 38 additional comprehensive MCP (Model Context Protocol) tools across multiple categories. It provides backend-agnostic implementations that automatically adapt to your preferred Emacs packages.

## Features

- 🎯 **38 Additional MCP Tools** across 9 categories
- 🔧 **Backend Agnostic** - works with lsp-mode/eglot, projectile/project.el, git/vc
- 🚀 **Auto-Detection** - automatically uses the best available backend
- 📦 **Zero Hard Dependencies** - works with vanilla Emacs, enhanced with optional packages
- 🎨 **Modular Design** - enable only the tool categories you need
- 🧪 **Well Tested** - comprehensive test suite included

## Relationship to Claude Code IDE

This package **extends** the base [Claude Code IDE](https://github.com/manzaltu/claude-code-ide.el) package rather than replacing it. The base package provides 5 core tools:

- **xref-find-references** - Find symbol references (we provide enhanced LSP version)
- **xref-find-apropos** - Search symbols by pattern (provided by base package)
- **project-info** - Project overview (provided by base package)
- **imenu-list-symbols** - List file symbols (provided by base package)
- **treesit-info** - Basic TreeSitter info (we provide 4 enhanced TreeSitter tools)

The package adds **38 additional tools** with enhanced functionality, better backend detection, and comprehensive development workflow support.

## Complete Additional Tool List

### Symbol Navigation & Analysis (5 tools)

Backends: lsp-mode, eglot, or xref fallback:

- **describe_symbol** - Get comprehensive symbol information at position
- **find_definition** - Find symbol definition
- **find_references** - Find all references to a symbol
- **xref_find_apropos** - Find symbols matching a pattern
- **which_function_at_point** - Get function name at position

### Project Management (5 tools)

Backends: projectile, project.el:

- **projectile_find_file** - Find files by pattern (fuzzy search)
- **projectile_grep** - Search text across project files
- **projectile_list_projects** - List all known projects
- **projectile_switch_project** - Switch to different project
- **projectile_current_project** - Get current project info

### Search & Discovery (3 tools)

Uses ripgrep when available:

- **rg_search** - Advanced ripgrep with file type filtering
- **consult_grep** - Interactive grep with live preview
- **occur_find_pattern** - Find pattern occurrences in file

### File Structure & Symbols (1 tool)

Always available:

- **goto_symbol** - Jump to specific symbol by name

### Cursor & Navigation (4 tools)

Always available:

- **goto_line_column** - Move cursor to specific position
- **get_cursor_position** - Get current cursor position
- **save_cursor_position** - Save position with name
- **restore_cursor_position** - Restore saved position

### Code Editing (2 tools)

Always available:

- **comment_region** - Toggle comments for line range
- **indent_region** - Fix indentation for line range

### Testing & Development (3 tools)

Backends: projectile with test framework detection:

- **detect_test_framework** - Detect project test configuration
- **run_file_tests** - Run tests for specific file
- **run_project_tests** - Run all project tests

### Git Integration (4 tools)

Uses direct git commands with optional vc backend:

- **git_status_summary** - Git status overview
- **git_diff_summary** - Git diff summary
- **git_blame_at_line** - Git blame for specific line
- **git_log_file** - Commit history for file

### Code Snippets (2 tools)

Requires yasnippet package:

- **yasnippet_list_snippets** - List available snippets
- **yasnippet_insert_snippet** - Insert snippet at position

### Documentation & Help (6 tools)

Always available, enhanced when helpful package is installed:

- **describe_function** - Get Emacs function documentation (enhanced with helpful when available)
- **describe_variable** - Get variable documentation (enhanced with helpful when available)
- **apropos_command** - Find commands matching pattern
- **which_key_help** - Check which-key mode status
- **occur_find_pattern** - Find pattern occurrences in file
- **which_function_at_point** - Get function name at position

### TreeSitter Analysis (4 tools)

Available when TreeSitter is supported:

- **treesit_analyze_structure** - Get complete syntax tree
- **treesit_get_node_at_point** - Get node info at position
- **treesit_find_parent_node** - Find parent node of type
- **treesit_get_children** - Get child nodes

### Buffer Management (2 tools)

Always available:

- **list_open_buffers** - List all open buffers
- **recent_files** - Show recently opened files

## Installation

### Prerequisites

- Emacs 28.1+
- [claude-code-ide](https://github.com/manzaltu/claude-code-ide.el)

### Optional Enhancements

For full functionality, consider installing:

```elisp
;; Language server support
(use-package lsp-mode)   ; OR
(use-package eglot)      ; Built-in as of Emacs 29

;; Project management
(use-package projectile) ; OR use built-in project.el

;; Git integration - uses direct git commands, no additional packages needed

;; Code snippets
(use-package yasnippet)

;; Enhanced help and documentation
(use-package helpful)
```

### Via Package Manager

```elisp
;; With use-package + elpaca
(use-package claude-code-ide-mcp-tools
  :elpaca (:host github :repo "Kaylebor/claude-code-ide-mcp-tools")
  :after claude-code-ide
  :config
  (claude-code-ide-mcp-tools-setup))

;; With straight.el
(use-package claude-code-ide-mcp-tools
  :straight (:host github :repo "Kaylebor/claude-code-ide-mcp-tools")
  :after claude-code-ide
  :config
  (claude-code-ide-mcp-tools-setup))
```

### Manual Installation

```bash
git clone https://github.com/Kaylebor/claude-code-ide-mcp-tools.git
```

```elisp
(add-to-list 'load-path "/path/to/claude-code-ide-mcp-tools")
(require 'claude-code-ide-mcp-tools)
(claude-code-ide-mcp-tools-setup)
```

## Configuration

### Backend Preferences

```elisp
;; Customize backend preferences
(setq claude-code-ide-mcp-tools-backends
      '((lsp . eglot)        ; Prefer eglot over lsp-mode
        (project . project)   ; Use built-in project.el
        (vcs . git)))        ; Use git commands directly

;; Or use auto-detection (default) - git commands used automatically
(setq claude-code-ide-mcp-tools-backends
      '((lsp . auto)
        (project . auto)
        (vcs . auto)))
```

### Selective Tool Loading

```elisp
;; Enable only specific categories
(setq claude-code-ide-mcp-tools-enable-categories
      '(lsp project nav edit))  ; Skip vcs, doc, treesit

;; Then setup
(claude-code-ide-mcp-tools-setup)
```

### Advanced Configuration

```elisp
(use-package claude-code-ide-mcp-tools
  :elpaca (:host github :repo "Kaylebor/claude-code-ide-mcp-tools")
  :after claude-code-ide
  :custom
  ;; Backend preferences
  (claude-code-ide-mcp-tools-backends
   '((lsp . lsp-mode)
     (project . projectile)
     (vcs . git)))

  ;; Tool categories to enable
  (claude-code-ide-mcp-tools-enable-categories
   '(lsp project vcs nav edit doc treesit))

  :config
  (claude-code-ide-mcp-tools-setup)

  ;; Optional: Check what was detected
  (claude-code-ide-mcp-tools-info))
```

## Usage

Once installed and configured, the tools are automatically available to Claude through the MCP server. Claude can use them to:

- Navigate your codebase intelligently
- Find definitions and references
- Manage cursor positions and bookmarks
- Edit code with proper indentation and comments
- Access Emacs documentation and help
- Analyze code structure with TreeSitter
- Manage git operations and project workflow

Example Claude interaction:

```text
Claude: I'll help you find that function definition.
> Tool: find_definition("src/main.py", 42, 10)
> Result: Found definition at src/utils.py:15:0

Claude: Let me navigate there and examine the code.
> Tool: goto_line_column("src/utils.py", 15, 0)
> Tool: describe_symbol("src/utils.py", 15, 0)
```

## Backend Compatibility

| Feature | lsp-mode | eglot | xref | projectile | project.el | git | vc |
|---------|----------|-------|------|------------|------------|-------|-----|
| Symbol Info | ✅ Full | ✅ Basic | ✅ Basic | N/A | N/A | N/A | N/A |
| Definitions | ✅ Full | 🚧 Planned | 🚧 Planned | N/A | N/A | N/A | N/A |
| References | ✅ Full | 🚧 Planned | 🚧 Planned | N/A | N/A | N/A | N/A |
| Project Files | N/A | N/A | N/A | ✅ Full | 🚧 Planned | N/A | N/A |
| Git Blame | N/A | N/A | N/A | N/A | N/A | ✅ Full | ✅ Basic |

✅ Full support &nbsp; ✅ Basic support &nbsp; 🚧 Planned &nbsp; N/A Not applicable

## Development

### Project Structure

```text
claude-code-ide-mcp-tools/
├── claude-code-ide-mcp-tools.el         # Main entry point
├── claude-code-ide-mcp-tools-core.el    # Core utilities
├── claude-code-ide-mcp-tools-lsp.el     # Language server tools
├── claude-code-ide-mcp-tools-project.el # Project management tools
├── claude-code-ide-mcp-tools-vcs.el     # Version control tools
├── claude-code-ide-mcp-tools-nav.el     # Navigation tools
├── claude-code-ide-mcp-tools-edit.el    # Editing tools
├── claude-code-ide-mcp-tools-doc.el     # Documentation tools
├── claude-code-ide-mcp-tools-treesit.el # TreeSitter tools
└── test/                                 # Test suite
```

### Running Tests

```bash
# Run all tests
emacs -batch -L . -l ert -l test/claude-code-ide-mcp-tools-test.el \
  -f ert-run-tests-batch-and-exit
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Debugging

Check your configuration:

```elisp
M-x claude-code-ide-mcp-tools-info
```

This shows:

- Detected backends for each category
- Enabled tool categories
- Registration status

## Troubleshooting

### "No tools registered"

- Ensure `claude-code-ide` is loaded first
- Check that `claude-code-ide-mcp-tools-setup` was called
- Verify `claude-code-ide-enable-mcp-server` is `t`

### "Backend not detected"

- Install the desired backend package (lsp-mode, projectile)
- Ensure the package is properly loaded and activated
- Check `claude-code-ide-mcp-tools-info` for detection status

### "Tool not working"

- Check if the required backend is active in the buffer
- Verify the tool category is enabled in `claude-code-ide-mcp-tools-enable-categories`
- Look for error messages in `*Messages*` buffer

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Claude Code IDE](https://github.com/manzaltu/claude-code-ide.el) for the MCP infrastructure
- [Anthropic](https://anthropic.com) for the Model Context Protocol specification
- The Emacs community for the excellent ecosystem of packages

---

**Note**: This package is designed specifically for Claude Code IDE and the Model Context Protocol. It enhances Claude's ability to interact with your Emacs environment through a comprehensive set of well-tested tools.

