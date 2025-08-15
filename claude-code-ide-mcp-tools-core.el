;;; claude-code-ide-mcp-tools-core.el --- Core utilities for MCP tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, tools

;;; Commentary:

;; Core utility functions for claude-code-ide-mcp-tools package.
;; Provides backend detection, tool registration helpers, and common utilities.

;;; Code:

(require 'claude-code-ide-mcp-server)
(require 'cl-lib)

;; Forward declaration of customization variable
(defvar claude-code-ide-mcp-tools-backends)

;; Optional backend function declarations
(declare-function projectile-project-root "projectile")

;;; Backend Detection

(defun claude-code-ide-mcp-tools-detect-backend (category)
  "Auto-detect best available backend for CATEGORY.
Returns the detected backend symbol or nil if none available."
  (pcase category
    ('lsp
     (cond ((featurep 'lsp-mode) 'lsp-mode)
           ((featurep 'eglot) 'eglot)
           (t 'xref)))  ; Always available fallback
    ('project
     (cond ((bound-and-true-p projectile-mode) 'projectile)
           ((featurep 'project) 'project)  ; Built-in since Emacs 25
           (t nil)))
    ('vcs
     (cond (t 'git)))  ; git commands always available
    (_ nil)))

(defun claude-code-ide-mcp-tools-get-backend (category)
  "Get the configured backend for CATEGORY, respecting user preferences."
  (let ((preference (alist-get category claude-code-ide-mcp-tools-backends)))
    (if (eq preference 'auto)
        (claude-code-ide-mcp-tools-detect-backend category)
      preference)))

;;; Buffer Management Utilities

(defun claude-code-ide-mcp-tools-with-file-buffer (file-path func)
  "Execute FUNC with buffer for FILE-PATH, managing buffer lifecycle.
Tries to use existing buffer first, creates temporary one if needed,
and cleans up temporary buffers afterward."
  (let* ((existing-buffer (find-buffer-visiting file-path))
         (target-buffer (or existing-buffer (find-file-noselect file-path)))
         (was-temp (not existing-buffer)))
    (unwind-protect
        (with-current-buffer target-buffer
          (funcall func))
      ;; Clean up temporary buffer if we created it
      (when (and was-temp (buffer-live-p target-buffer))
        (kill-buffer target-buffer)))))

;;; Tool Registration - Copy of working macro from original config

(defvar claude-code-ide-mcp-tools--tool-list nil
  "List of MCP tools to be registered.
Each entry is a plist with :name, :function, :description, and :args.")

(defmacro claude-code-ide-mcp-tools-define-tool (name args docstring &rest body)
  "Define an MCP tool function and automatically register it.

NAME is the tool name (symbol).
ARGS is the argument list for the function.
DOCSTRING includes the tool description (first paragraph used for MCP).
BODY is the function implementation.

The macro creates a function and automatically
registers it as an MCP tool with name NAME (hyphens converted to underscores).
Automatically handles type conversion for common numeric parameters."
  (declare (indent 3) (doc-string 3))
  (let* ((func-name (intern (format "claude-code-ide-mcp-tools-%s" name)))
         (tool-name (replace-regexp-in-string "-" "_" (symbol-name name)))
         ;; Extract first paragraph of docstring as description
         (description (car (split-string docstring "\n\n" t)))
         ;; Parse args to create MCP argument specifications
         (mcp-args (claude-code-ide-mcp-tools--parse-mcp-args args docstring))
         ;; Detect numeric parameters that need automatic conversion
         (has-line (memq 'line args))
         (has-column (memq 'column args))
         (has-max-count (memq 'max-count args))
         (has-limit (memq 'limit args))
         (has-start-line (memq 'start-line args))
         (has-end-line (memq 'end-line args))
         ;; Build conversion bindings for numeric parameters
         (conversions
          (append
           (when has-line 
             '((line (if (stringp line) (string-to-number line) line))))
           (when has-column 
             '((column (and column (if (stringp column) (string-to-number column) column)))))
           (when has-max-count
             '((max-count (and max-count (if (stringp max-count) (string-to-number max-count) max-count)))))
           (when has-limit
             '((limit (and limit (if (stringp limit) (string-to-number limit) limit)))))
           (when has-start-line
             '((start-line (if (stringp start-line) (string-to-number start-line) start-line))))
           (when has-end-line
             '((end-line (if (stringp end-line) (string-to-number end-line) end-line))))))
         ;; Wrap body with conversions if needed
         (wrapped-body
          (if conversions
              `((let ,conversions ,@body))
            body)))
    `(progn
       ;; Define the function with automatic type conversions
       (defun ,func-name ,args
         ,docstring
         ,@wrapped-body)
       ;; Add to registration list
       (add-to-list 'claude-code-ide-mcp-tools--tool-list
                    (list :name ,tool-name
                          :function #',func-name
                          :description ,description
                          :args ',mcp-args)))))

(defun claude-code-ide-mcp-tools--parse-mcp-args (args docstring)
  "Parse function ARGS and DOCSTRING to create MCP argument specifications."
  (let ((mcp-args '()))
    (dolist (arg args)
      (when (and (symbolp arg) 
                 (not (memq arg '(&optional &rest))))
        (let ((arg-name (symbol-name arg))
              (arg-desc (claude-code-ide-mcp-tools--extract-arg-description arg docstring))
              (optional (claude-code-ide-mcp-tools--arg-is-optional-p arg args)))
          (push `(:name ,arg-name
                  :type string
                  ,@(when optional '(:optional t))
                  :description ,arg-desc)
                mcp-args))))
    (nreverse mcp-args)))

(defun claude-code-ide-mcp-tools--extract-arg-description (arg docstring)
  "Extract description for ARG from DOCSTRING."
  (let ((arg-name (upcase (symbol-name arg))))
    (cond
     ;; Look for "ARG - description" pattern
     ((string-match (format "%s[[:space:]]*-[[:space:]]*\\([^\n.]+\\)" arg-name) docstring)
      (match-string 1 docstring))
     ;; Look for "ARG: description" pattern  
     ((string-match (format "%s[[:space:]]*:[[:space:]]*\\([^\n.]+\\)" arg-name) docstring)
      (match-string 1 docstring))
     ;; Default description
     (t (format "The %s parameter" (symbol-name arg))))))

(defun claude-code-ide-mcp-tools--arg-is-optional-p (arg args)
  "Check if ARG appears after &optional in ARGS list."
  (let ((found-optional nil)
        (is-optional nil))
    (dolist (a args)
      (cond
       ((eq a '&optional) (setq found-optional t))
       ((eq a '&rest) (setq found-optional nil))
       ((and found-optional (eq a arg)) (setq is-optional t))))
    is-optional))

(defun claude-code-ide-mcp-tools-register-pending ()
  "Register all pending tools with claude-code-ide.
This should only be called once to avoid duplicate registrations."
  (let ((count 0))
    (dolist (tool-info claude-code-ide-mcp-tools--tool-list)
      (apply #'claude-code-ide-make-tool tool-info)
      (cl-incf count))
    ;; Clear the list to prevent duplicate registrations
    (setq claude-code-ide-mcp-tools--tool-list nil)
    count))

;;; Error Handling

(defun claude-code-ide-mcp-tools-safe-call (func &rest args)
  "Safely call FUNC with ARGS, returning error message on failure."
  (condition-case err
      (apply func args)
    (error (format "Error: %s" (error-message-string err)))))

;;; Common Predicates

(defun claude-code-ide-mcp-tools-lsp-active-p ()
  "Check if any LSP backend is active in current buffer."
  (or (bound-and-true-p lsp-mode)
      (bound-and-true-p eglot--managed-mode)))

(defun claude-code-ide-mcp-tools-project-root ()
  "Get project root using best available backend."
  (let ((backend (claude-code-ide-mcp-tools-get-backend 'project)))
    (cond
     ((eq backend 'projectile)
      (and (bound-and-true-p projectile-mode)
           (projectile-project-root)))
     ((eq backend 'project)
      (when-let ((project (project-current)))
        (project-root project)))
     (t default-directory))))

(provide 'claude-code-ide-mcp-tools-core)
;;; claude-code-ide-mcp-tools-core.el ends here