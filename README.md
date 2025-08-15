# Claude Code IDE MCP Tools

**Extended MCP tools for Claude Code IDE with backend-agnostic design**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-28.1+-purple.svg)](https://www.gnu.org/software/emacs/)

This package extends [Claude Code IDE](https://github.com/username/claude-code-ide) with 35+ comprehensive MCP (Model Context Protocol) tools across multiple categories. It provides backend-agnostic implementations that automatically adapt to your preferred Emacs packages.

## Features

🎯 **35+ MCP Tools** across 7 categories  
🔧 **Backend Agnostic** - works with lsp-mode/eglot, projectile/project.el, magit/vc  
🚀 **Auto-Detection** - automatically uses the best available backend  
📦 **Zero Hard Dependencies** - works with vanilla Emacs, enhanced with optional packages  
🎨 **Modular Design** - enable only the tool categories you need  
🧪 **Well Tested** - comprehensive test suite included  

## Tool Categories

### Language Server Integration
- **describe_symbol** - Get comprehensive symbol information
- **find_definition** - Jump to symbol definitions  
- **find_references** - Find all symbol references
- **get_hover_info** - Get documentation at point

*Backends: lsp-mode, eglot, or xref fallback*

### Project Navigation
- **find_file** - Find files by pattern
- **grep_project** - Search across project files
- **list_project_files** - Get project file listing
- **get_project_root** - Get project root directory

*Backends: projectile, project.el*

### Version Control
- **show_status** - Get repository status
- **show_blame** - Show git blame for file/line
- **show_log** - Get file history
- **show_diff** - Show file differences

*Backends: magit, built-in vc*

### Navigation & Cursor Management
- **goto_line_column** - Move to specific position
- **get_cursor_position** - Get current cursor info
- **save_cursor_position** - Bookmark positions
- **restore_cursor_position** - Return to bookmarks
- **goto_symbol** - Jump to symbols with fuzzy matching

*Always available*

### Text Editing
- **comment_region** - Comment/uncomment text
- **indent_region** - Fix indentation
- **replace_text** - Replace text in regions
- **insert_text** - Insert text at positions

*Always available*

### Documentation & Help
- **describe_function** - Get function documentation
- **describe_variable** - Get variable information
- **list_symbols** - List file symbols via imenu
- **search_documentation** - Search help content

*Always available*

### TreeSitter Analysis
- **analyze_structure** - Get syntax tree structure
- **get_node_at_point** - Get AST node information
- **find_parent_node** - Find parent nodes by type
- **get_children** - Get child nodes

*Available when TreeSitter is supported*

## Installation

### Prerequisites

- Emacs 28.1+
- [claude-code-ide](https://github.com/username/claude-code-ide)

### Optional Enhancements

For full functionality, consider installing:

```elisp
;; Language server support
(use-package lsp-mode)   ; OR
(use-package eglot)      ; Built-in as of Emacs 29

;; Project management  
(use-package projectile) ; OR use built-in project.el

;; Git integration
(use-package magit)      ; OR use built-in vc
```

### Via Package Manager

```elisp
;; With use-package
(use-package claude-code-ide-mcp-tools
  :after claude-code-ide
  :config
  (claude-code-ide-mcp-tools-setup))

;; With straight.el
(use-package claude-code-ide-mcp-tools
  :straight (:host github :repo "enderveiga/claude-code-ide-mcp-tools")
  :after claude-code-ide
  :config
  (claude-code-ide-mcp-tools-setup))
```

### Manual Installation

```bash
git clone https://github.com/enderveiga/claude-code-ide-mcp-tools.git
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
        (vcs . magit)))       ; Use magit for git operations

;; Or use auto-detection (default)
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
  :after claude-code-ide
  :custom
  ;; Backend preferences
  (claude-code-ide-mcp-tools-backends
   '((lsp . lsp-mode)
     (project . projectile)
     (vcs . magit)))
  
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

Example Claude interaction:
```
Claude: I'll help you find that function definition.
> Tool: find_definition("src/main.py", 42, 10)
> Result: Found definition at src/utils.py:15:0

Claude: Let me navigate there and examine the code.
> Tool: goto_line_column("src/utils.py", 15, 0)
> Tool: describe_symbol("src/utils.py", 15, 0)
```

## Backend Compatibility

| Feature | lsp-mode | eglot | xref | projectile | project.el | magit | vc |
|---------|----------|-------|------|------------|------------|-------|-----|
| Symbol Info | ✅ Full | ✅ Basic | ✅ Basic | N/A | N/A | N/A | N/A |
| Definitions | ✅ Full | 🚧 Planned | ✅ Basic | N/A | N/A | N/A | N/A |
| References | ✅ Full | 🚧 Planned | 🚧 Planned | N/A | N/A | N/A | N/A |
| Project Files | N/A | N/A | N/A | ✅ Full | ✅ Basic | N/A | N/A |
| Git Blame | N/A | N/A | N/A | N/A | N/A | ✅ Full | ✅ Basic |

✅ Full support &nbsp; ✅ Basic support &nbsp; 🚧 Planned &nbsp; N/A Not applicable

## Development

### Project Structure

```
claude-code-ide-mcp-tools/
├── claude-code-ide-mcp-tools.el      # Main entry point
├── claude-code-ide-mcp-tools-core.el # Core utilities
├── tools/                             # Tool implementations
│   ├── claude-code-ide-mcp-tools-lsp.el
│   ├── claude-code-ide-mcp-tools-project.el
│   ├── claude-code-ide-mcp-tools-vcs.el
│   ├── claude-code-ide-mcp-tools-nav.el
│   ├── claude-code-ide-mcp-tools-edit.el
│   ├── claude-code-ide-mcp-tools-doc.el
│   └── claude-code-ide-mcp-tools-treesit.el
└── test/                              # Test suite
```

### Running Tests

```bash
# Run all tests
emacs -batch -L . -l ert -l test/claude-code-ide-mcp-tools-test.el \\
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
- Install the desired backend package (lsp-mode, projectile, magit)
- Ensure the package is properly loaded and activated
- Check `claude-code-ide-mcp-tools-info` for detection status

### "Tool not working"
- Check if the required backend is active in the buffer
- Verify the tool category is enabled in `claude-code-ide-mcp-tools-enable-categories`
- Look for error messages in `*Messages*` buffer

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Claude Code IDE](https://github.com/username/claude-code-ide) for the MCP infrastructure
- [Anthropic](https://anthropic.com) for the Model Context Protocol specification
- The Emacs community for the excellent ecosystem of packages

---

**Note**: This package is designed specifically for Claude Code IDE and the Model Context Protocol. It enhances Claude's ability to interact with your Emacs environment through a comprehensive set of well-tested tools.