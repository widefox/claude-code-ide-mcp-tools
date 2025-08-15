;;; claude-code-ide-mcp-tools-lsp.el --- Language server integration tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, lsp

;;; Commentary:

;; Language server integration tools for claude-code-ide-mcp-tools.
;; Provides backend-agnostic LSP functionality that works with lsp-mode, eglot, or xref fallbacks.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; LSP backend function declarations
(declare-function lsp-describe-thing-at-point "lsp-mode")
(declare-function lsp-find-definition "lsp-mode")
(declare-function lsp-request "lsp-mode")
(declare-function lsp--text-document-position-params "lsp-mode")
(declare-function lsp--uri-to-path "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")

;; Eglot backend function declarations
(declare-function eglot-current-server "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot--uri-to-path "eglot")

;; Xref fallback declarations
(declare-function xref-find-definitions "xref")
(declare-function xref-find-references "xref")
(declare-function xref-backend-identifier-at-point "xref")

;;; Backend-specific implementations

(defun claude-code-ide-mcp-tools-lsp--lsp-mode-describe (file-path line column)
  "Get symbol information using lsp-mode backend."
  (claude-code-ide-mcp-tools-with-file-buffer file-path
    (lambda ()
      (when (bound-and-true-p lsp-mode)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column (or column 0))
          (let ((info (lsp-describe-thing-at-point)))
            (format "LSP info at %s:%d:%d:\n%s" file-path line column info)))))))

(defun claude-code-ide-mcp-tools-lsp--eglot-describe (file-path line column)
  "Get symbol information using eglot backend."
  (claude-code-ide-mcp-tools-with-file-buffer file-path
    (lambda ()
      (when (bound-and-true-p eglot--managed-mode)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column (or column 0))
          ;; Eglot doesn't have a direct equivalent to lsp-describe-thing-at-point
          ;; but we can get hover information
          (when-let ((server (eglot-current-server)))
            (format "Eglot info at %s:%d:%d:\nSymbol at point: %s"
                    file-path line column
                    (or (thing-at-point 'symbol) "No symbol"))))))))

(defun claude-code-ide-mcp-tools-lsp--xref-describe (file-path line column)
  "Get symbol information using xref fallback."
  (claude-code-ide-mcp-tools-with-file-buffer file-path
    (lambda ()
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column (or column 0))
        (let ((symbol (thing-at-point 'symbol)))
          (if symbol
              (format "Symbol info at %s:%d:%d:\nSymbol: %s\nNote: Using xref fallback (no LSP server active)"
                      file-path line column symbol)
            (format "No symbol found at %s:%d:%d" file-path line column)))))))

(defun claude-code-ide-mcp-tools-lsp--lsp-mode-find-definition (file-path line column)
  "Find definition using lsp-mode backend."
  (claude-code-ide-mcp-tools-with-file-buffer file-path
    (lambda ()
      (when (bound-and-true-p lsp-mode)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column (or column 0))
          (condition-case err
              (let ((definitions (lsp-request "textDocument/definition"
                                            (lsp--text-document-position-params))))
                (if definitions
                    (let ((def-list (cond
                                    ((vectorp definitions) (append definitions nil))
                                    ((listp definitions) definitions)
                                    (t (list definitions)))))
                      (mapcar (lambda (def)
                                (let* ((uri (or (plist-get def :targetUri)
                                              (plist-get def :uri)))
                                       (range (or (plist-get def :targetSelectionRange)
                                                (plist-get def :targetRange)
                                                (plist-get def :range)))
                                       (start (when range (plist-get range :start))))
                                  (if (and uri range start)
                                      (format "%s:%d:%d"
                                              (lsp--uri-to-path uri)
                                              (1+ (plist-get start :line))
                                              (plist-get start :character))
                                    (format "Invalid definition data: %S" def))))
                              def-list))
                  "No definition found"))
            (error (format "LSP error finding definition: %s" (error-message-string err)))))))))

(defun claude-code-ide-mcp-tools-lsp--lsp-mode-find-references (file-path line column)
  "Find references using lsp-mode backend."
  (claude-code-ide-mcp-tools-with-file-buffer file-path
    (lambda ()
      (when (bound-and-true-p lsp-mode)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column (or column 0))
          (let* ((params (lsp--text-document-position-params))
                 (refs-response (lsp-request "textDocument/references" 
                                           (plist-put params :context '(:includeDeclaration t))))
                 (refs (when refs-response
                         (mapcar (lambda (ref)
                                   (let* ((uri (plist-get ref :uri))
                                          (range (plist-get ref :range))
                                          (start (plist-get range :start))
                                          (ref-line (1+ (plist-get start :line)))
                                          (ref-char (plist-get start :character))
                                          (ref-file (lsp--uri-to-path uri)))
                                     (format "%s:%d:%d" ref-file ref-line ref-char)))
                                 refs-response))))
            (if refs
                (format "Found %d references:\n%s" 
                        (length refs) 
                        (mapconcat #'identity refs "\n"))
              "No references found")))))))

;;; Public tool definitions

(claude-code-ide-mcp-tools-define-tool describe-symbol (file-path line column)
  "Get comprehensive symbol information at a specific position.
  
  FILE-PATH - Path to the file to analyze
  LINE - Line number (1-based)  
  COLUMN - Column number (0-based)
  
  Uses the best available language server backend (lsp-mode, eglot, or xref fallback)."
  (if (not file-path)
      (error "file_path parameter is required")
    (let ((backend (claude-code-ide-mcp-tools-get-backend 'lsp)))
      (claude-code-ide-mcp-tools-safe-call
       (cond
        ((eq backend 'lsp-mode)
         (claude-code-ide-mcp-tools-lsp--lsp-mode-describe file-path line column))
        ((eq backend 'eglot)
         (claude-code-ide-mcp-tools-lsp--eglot-describe file-path line column))
        (t
         (claude-code-ide-mcp-tools-lsp--xref-describe file-path line column)))))))

(claude-code-ide-mcp-tools-define-tool find-definition (file-path line column)
  "Find symbol definition at a specific position.
  
  FILE-PATH - Path to the file containing the symbol
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  Returns the exact file and position where the symbol is defined."
  (if (not file-path)
      (error "file_path parameter is required")
    (let ((backend (claude-code-ide-mcp-tools-get-backend 'lsp)))
      (claude-code-ide-mcp-tools-safe-call
       (cond
        ((eq backend 'lsp-mode)
         (claude-code-ide-mcp-tools-lsp--lsp-mode-find-definition file-path line column))
        ((eq backend 'eglot)
         ;; TODO: Implement eglot-specific version
         "Eglot backend not yet implemented for find-definition")
        (t
         ;; TODO: Implement xref fallback
         "Xref fallback not yet implemented for find-definition"))))))

(claude-code-ide-mcp-tools-define-tool find-references (file-path line column)
  "Find all references to a symbol throughout the project.
  
  FILE-PATH - Path to the file containing the symbol
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  Returns a comprehensive list of everywhere the symbol is used."
  (if (not file-path)
      (error "file_path parameter is required")
    (let ((backend (claude-code-ide-mcp-tools-get-backend 'lsp)))
      (claude-code-ide-mcp-tools-safe-call
       (cond
        ((eq backend 'lsp-mode)
         (claude-code-ide-mcp-tools-lsp--lsp-mode-find-references file-path line column))
        ((eq backend 'eglot)
         ;; TODO: Implement eglot-specific version
         "Eglot backend not yet implemented for find-references")
        (t
         ;; TODO: Implement xref fallback
         "Xref fallback not yet implemented for find-references"))))))

;;; Registration function

(defun claude-code-ide-mcp-tools-register-lsp-tools ()
  "Register all LSP tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-lsp)
;;; claude-code-ide-mcp-tools-lsp.el ends here