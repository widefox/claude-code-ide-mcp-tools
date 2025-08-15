;;; claude-code-ide-mcp-tools.el --- Extended MCP tools for Claude Code IDE -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno <letstalk@ender.codes>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (claude-code-ide "0.1"))
;; Keywords: ai, claude, mcp, tools, convenience
;; URL: https://github.com/Kaylebor/claude-code-ide-mcp-tools

;;; Commentary:

;; This package extends Claude Code IDE with 38 additional MCP tools
;; across multiple categories. It provides backend-agnostic implementations
;; that work with user's preferred packages:
;;
;; - Language Servers: lsp-mode, eglot, or built-in xref
;; - Project Management: projectile, project.el
;; - Version Control: git commands, vc
;; - Plus navigation, editing, documentation, and TreeSitter tools
;;
;; Installation:
;;   (use-package claude-code-ide-mcp-tools
;;     :after claude-code-ide
;;     :config
;;     (claude-code-ide-mcp-tools-setup))
;;
;; The package automatically detects available backends and adapts accordingly.
;; Tools are registered with claude-code-ide's MCP server for use by Claude.

;;; Code:

(require 'claude-code-ide-mcp-server)
(require 'cl-lib)

;; Load core utilities
(require 'claude-code-ide-mcp-tools-core)

;;; Customization

(defgroup claude-code-ide-mcp-tools nil
  "Extended MCP tools for Claude Code IDE."
  :group 'claude-code-ide
  :prefix "claude-code-ide-mcp-tools-")

(defcustom claude-code-ide-mcp-tools-backends
  '((lsp . auto)      ; auto-detect: lsp-mode, eglot, or xref fallback
    (project . auto)  ; auto-detect: projectile, project.el
    (vcs . auto))     ; auto-detect: git, vc
  "Backend preferences for each tool category.
Each entry is (CATEGORY . BACKEND) where BACKEND can be:
- auto: Auto-detect best available backend
- Specific backend: lsp-mode, eglot, projectile, project, git, vc"
  :type '(alist :key-type (choice (const lsp)
                                  (const project)
                                  (const vcs))
                :value-type (choice (const auto)
                                   (const lsp-mode)
                                   (const eglot)
                                   (const projectile)
                                   (const project)
                                   (const git)
                                   (const vc)))
  :group 'claude-code-ide-mcp-tools)

(defcustom claude-code-ide-mcp-tools-enable-categories
  '(lsp project vcs nav edit doc treesit)
  "Categories of tools to enable.
Disable categories you don't want to reduce the number of registered tools."
  :type '(set (const :tag "Language Server Tools" lsp)
              (const :tag "Project Navigation Tools" project)
              (const :tag "Version Control Tools" vcs)
              (const :tag "Navigation Tools" nav)
              (const :tag "Editing Tools" edit)
              (const :tag "Documentation Tools" doc)
              (const :tag "TreeSitter Tools" treesit))
  :group 'claude-code-ide-mcp-tools)

;;; Public API

;;;###autoload
(defun claude-code-ide-mcp-tools-setup ()
  "Set up and register all MCP tools with Claude Code IDE.
This function should be called after claude-code-ide is loaded."
  (interactive)
  
  ;; Ensure MCP server is enabled
  (setq claude-code-ide-enable-mcp-server t)
  
  ;; Load tool modules explicitly (autoload cookies don't work for programmatic calls)
  (require 'claude-code-ide-mcp-tools-lsp)
  (require 'claude-code-ide-mcp-tools-project) 
  (require 'claude-code-ide-mcp-tools-vcs)
  (require 'claude-code-ide-mcp-tools-nav)
  (require 'claude-code-ide-mcp-tools-edit)
  (require 'claude-code-ide-mcp-tools-doc)
  (when (treesit-available-p)
    (require 'claude-code-ide-mcp-tools-treesit))
  
  ;; Register all tools once (all tools are already added to the list by requiring modules)
  (let ((registered-count (claude-code-ide-mcp-tools-register-pending)))
    (message "claude-code-ide-mcp-tools-setup: Registered %d MCP tools with Claude Code IDE" registered-count)))

;;;###autoload
(defun claude-code-ide-mcp-tools-info ()
  "Display information about available tools and backends."
  (interactive)
  (with-output-to-temp-buffer "*MCP Tools Info*"
    (princ "Claude Code IDE MCP Tools\n")
    (princ "===========================\n\n")
    
    (princ "Detected Backends:\n")
    (princ (format "  LSP: %s\n" (claude-code-ide-mcp-tools-detect-backend 'lsp)))
    (princ (format "  Project: %s\n" (claude-code-ide-mcp-tools-detect-backend 'project)))
    (princ (format "  VCS: %s\n" (claude-code-ide-mcp-tools-detect-backend 'vcs)))
    (princ (format "  TreeSitter: %s\n" (if (treesit-available-p) "available" "not available")))
    
    (princ "\nEnabled Categories:\n")
    (dolist (category claude-code-ide-mcp-tools-enable-categories)
      (princ (format "  - %s\n" category)))
    
    (princ "\nTo register tools, run: M-x claude-code-ide-mcp-tools-setup")))

(provide 'claude-code-ide-mcp-tools)
;;; claude-code-ide-mcp-tools.el ends here