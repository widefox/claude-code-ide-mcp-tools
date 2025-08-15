;;; claude-code-ide-mcp-tools-doc.el --- Documentation and help tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, documentation, help

;;; Commentary:

;; Documentation and help tools for claude-code-ide-mcp-tools.
;; Provides access to Emacs function/variable documentation and help systems.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; Documentation & help declarations
(declare-function describe-function "help-fns")
(declare-function describe-variable "help-fns")
(declare-function apropos-command "apropos")
(declare-function which-key-show-keymap "which-key")

;; Code intelligence declarations
(declare-function imenu--make-index-alist "imenu")
(declare-function occur "replace")
(declare-function xref-find-apropos "xref")
(declare-function which-function "which-func")

(claude-code-ide-mcp-tools-define-tool describe-function (function-name)
  "Get comprehensive documentation for an Emacs Lisp function.
  
  FUNCTION-NAME - Name of the function to describe
  
  Includes usage examples, parameters, and detailed documentation."
  (progn
    (condition-case err
        (if (fboundp (intern function-name))
            (let* ((func-symbol (intern function-name))
                   (doc (documentation func-symbol))
                   (type (cond ((commandp func-symbol) "Command")
                              ((functionp func-symbol) "Function")
                              (t "Symbol"))))
              (format "%s: %s\n\nDocumentation:\n%s" 
                      type function-name 
                      (or doc "No documentation available.")))
          (format "Function '%s' not found" function-name))
      (error
       (format "Error describing function '%s': %s"
               function-name (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool describe-variable (variable-name)
  "Get documentation for an Emacs Lisp variable.
  
  VARIABLE-NAME - Name of the variable to describe
  
  Shows current value, purpose, and configuration options."
  (progn
    (condition-case err
        (if (boundp (intern variable-name))
            (let* ((var-symbol (intern variable-name))
                   (doc (documentation-property var-symbol 'variable-documentation))
                   (value (symbol-value var-symbol))
                   (type (cond ((custom-variable-p var-symbol) "Custom Variable")
                              (t "Variable"))))
              (format "%s: %s\n\nCurrent Value:\n%S\n\nDocumentation:\n%s" 
                      type variable-name value
                      (or doc "No documentation available.")))
          (format "Variable '%s' not found" variable-name))
      (error
       (format "Error describing variable '%s': %s"
               variable-name (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool apropos-command (pattern)
  "Find all commands (interactive functions) matching a pattern.
  
  PATTERN - Pattern to search for in command names
  
  Useful for discovering available functionality."
  (progn
    (condition-case err
        (let ((commands (apropos-internal pattern 'commandp)))
          (if commands
              (mapcar #'symbol-name commands)
            (format "No commands found matching pattern '%s'" pattern)))
      (error
       (format "Error finding commands matching '%s': %s"
               pattern (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool which-key-help ()
  "Check if which-key mode is active for automatic keybinding help.
  
  Which-key provides contextual keybinding assistance."
  (progn
    (condition-case err
        (if (bound-and-true-p which-key-mode)
            "Which-key mode is active. Available keybindings are shown automatically."
          "Which-key mode is not active")
      (error
       (format "Error checking which-key: %s" (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool occur-find-pattern (file-path pattern)
  "Find all occurrences of a pattern within a specific file.
  
  FILE-PATH - Path to the file to search in
  PATTERN - Pattern to search for (supports regex)
  
  Shows line numbers and context for each match."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (let ((matches '())
                      (line-num 1))
                  (while (re-search-forward pattern nil t)
                    (let ((line-start (line-beginning-position))
                          (line-end (line-end-position)))
                      (push (format "%s:%d: %s"
                                    file-path
                                    line-num
                                    (buffer-substring-no-properties line-start line-end))
                            matches))
                    (forward-line 1)
                    (setq line-num (line-number-at-pos)))
                  (if matches
                      (nreverse matches)
                    (format "No occurrences of pattern '%s' found in %s" pattern file-path))))))
        (error
         (format "Error finding pattern '%s' in %s: %s"
                 pattern file-path (error-message-string err)))))))


(claude-code-ide-mcp-tools-define-tool which-function-at-point (file-path line column)
  "Get the name of the function or method at a specific position.
  
  FILE-PATH - Path to the file to analyze
  LINE - Line number (1-based)
  COLUMN - Column number (0-based, optional)
  
  Useful for understanding code context."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- line))
                (move-to-column (or column 0))
                (require 'which-func)
                (let ((func-name (which-function)))
                  (if func-name
                      (format "Current function: %s" func-name)
                    "Not inside a function")))))
        (error
         (format "Error getting function name at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

;;; Registration function

(defun claude-code-ide-mcp-tools-register-doc-tools ()
  "Register all documentation tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-doc)
;;; claude-code-ide-mcp-tools-doc.el ends here
