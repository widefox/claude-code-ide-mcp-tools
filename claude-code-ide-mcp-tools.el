;;; claude-code-ide-mcp-tools.el --- Extended MCP tools for Claude Code IDE -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (claude-code-ide "0.1"))
;; Keywords: ai, claude, mcp, tools, convenience
;; URL: https://github.com/yourusername/claude-code-ide-mcp-tools

;;; Commentary:

;; This package extends Claude Code IDE with 35+ comprehensive MCP tools
;; across multiple categories. It provides backend-agnostic implementations
;; that work with user's preferred packages:
;;
;; - Language Servers: lsp-mode, eglot, or built-in xref
;; - Project Management: projectile, project.el
;; - Version Control: magit, vc
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

;; Add tools directory to load path for submodules first
(add-to-list 'load-path (expand-file-name "tools" (file-name-directory (or load-file-name buffer-file-name))))

;; Load core utilities first - but defer tool loading until setup
(require 'claude-code-ide-mcp-tools-core)

;; Function declarations for tool registration functions (loaded at runtime)
(declare-function claude-code-ide-mcp-tools-register-lsp-tools "claude-code-ide-mcp-tools-lsp")
(declare-function claude-code-ide-mcp-tools-register-nav-tools "claude-code-ide-mcp-tools-nav")
(declare-function claude-code-ide-mcp-tools-register-project-tools "claude-code-ide-mcp-tools-project")
(declare-function claude-code-ide-mcp-tools-register-vcs-tools "claude-code-ide-mcp-tools-vcs")
(declare-function claude-code-ide-mcp-tools-register-edit-tools "claude-code-ide-mcp-tools-edit")
(declare-function claude-code-ide-mcp-tools-register-doc-tools "claude-code-ide-mcp-tools-doc")
(declare-function claude-code-ide-mcp-tools-register-treesit-tools "claude-code-ide-mcp-tools-treesit")

;;; Customization

(defgroup claude-code-ide-mcp-tools nil
  "Extended MCP tools for Claude Code IDE."
  :group 'claude-code-ide
  :prefix "claude-code-ide-mcp-tools-")

(defcustom claude-code-ide-mcp-tools-backends
  '((lsp . auto)      ; auto-detect: lsp-mode, eglot, or xref fallback
    (project . auto)  ; auto-detect: projectile, project.el
    (vcs . auto))     ; auto-detect: magit, vc
  "Backend preferences for each tool category.
Each entry is (CATEGORY . BACKEND) where BACKEND can be:
- auto: Auto-detect best available backend
- Specific backend: lsp-mode, eglot, projectile, project, magit, vc"
  :type '(alist :key-type (choice (const lsp)
                                  (const project)
                                  (const vcs))
                :value-type (choice (const auto)
                                   (const lsp-mode)
                                   (const eglot)
                                   (const projectile)
                                   (const project)
                                   (const magit)
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
  
  ;; Load all tool categories now that dependencies are ready
  (require 'claude-code-ide-mcp-tools-lsp)
  (require 'claude-code-ide-mcp-tools-nav)
  (require 'claude-code-ide-mcp-tools-project)
  (require 'claude-code-ide-mcp-tools-vcs)
  (require 'claude-code-ide-mcp-tools-edit)
  (require 'claude-code-ide-mcp-tools-doc)
  (require 'claude-code-ide-mcp-tools-treesit)
  
  ;; Register tools from enabled categories
  (let ((registered-count 0))
    (when (memq 'lsp claude-code-ide-mcp-tools-enable-categories)
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-lsp-tools))))
    
    (when (memq 'project claude-code-ide-mcp-tools-enable-categories)
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-project-tools))))
    
    (when (memq 'vcs claude-code-ide-mcp-tools-enable-categories)
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-vcs-tools))))
    
    (when (memq 'nav claude-code-ide-mcp-tools-enable-categories)
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-nav-tools))))
    
    (when (memq 'edit claude-code-ide-mcp-tools-enable-categories)
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-edit-tools))))
    
    (when (memq 'doc claude-code-ide-mcp-tools-enable-categories)
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-doc-tools))))
    
    (when (and (memq 'treesit claude-code-ide-mcp-tools-enable-categories)
               (treesit-available-p))
      (setq registered-count (+ registered-count (claude-code-ide-mcp-tools-register-treesit-tools))))
    
    (message "Registered %d MCP tools with Claude Code IDE" registered-count)))

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