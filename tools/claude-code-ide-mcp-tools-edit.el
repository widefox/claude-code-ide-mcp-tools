;;; claude-code-ide-mcp-tools-edit.el --- Code editing and manipulation tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, editing, manipulation, yasnippet

;;; Commentary:

;; Code editing and manipulation tools for claude-code-ide-mcp-tools.
;; Provides safe code modification operations and yasnippet integration.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; Code manipulation declarations
(declare-function comment-or-uncomment-region "newcomment")
(declare-function indent-region "indent")
(declare-function sort-lines "sort")
(declare-function query-replace-regexp "replace")

;; Yasnippet declarations
(declare-function yas-describe-tables "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-insert-snippet "yasnippet")

(claude-code-ide-mcp-tools-define-tool comment-region (file-path start-line end-line)
  "Toggle comments for lines in a specific range.
  
  FILE-PATH - Path to the file
  START-LINE - Starting line number (1-based)
  END-LINE - Ending line number (1-based)
  
  Safe operation that can be easily undone."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- start-line))
                (let ((start-pos (line-beginning-position)))
                  (goto-char (point-min))
                  (forward-line (1- end-line))
                  (let ((end-pos (line-end-position)))
                    ;; Make sure we have proper region
                    (when (> end-pos start-pos)
                      (comment-or-uncomment-region start-pos end-pos)
                      ;; Save the buffer if it was already visited
                      (when (buffer-file-name)
                        (save-buffer)))
                    (format "Toggled comments for lines %d-%d in %s"
                            start-line end-line file-path))))))
        (error
         (format "Error commenting region in %s: %s"
                 file-path (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool indent-region (file-path start-line end-line)
  "Fix indentation for lines in a specific range.
  
  FILE-PATH - Path to the file
  START-LINE - Starting line number (1-based)
  END-LINE - Ending line number (1-based)
  
  Applies indentation according to the current mode's rules."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((target-buffer (or (find-buffer-visiting file-path)
                                   (find-file-noselect file-path))))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- start-line))
                (let ((start-pos (line-beginning-position)))
                  (forward-line (- end-line start-line))
                  (let ((end-pos (line-end-position)))
                    (indent-region start-pos end-pos)
                    (format "Fixed indentation for lines %d-%d in %s"
                            start-line end-line file-path))))))
        (error
         (format "Error indenting region in %s: %s"
                 file-path (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool yasnippet-list-snippets (&optional mode-name)
  "List available yasnippet snippets for a specific mode.
  
  MODE-NAME - Major mode name (e.g., 'ruby-mode', 'js-mode', optional)
  
  Shows snippet keys and descriptions. If mode-name is omitted, uses current 
  buffer's mode."
  (progn
    (condition-case err
        (if (bound-and-true-p yas-global-mode)
            (let* ((mode-symbol (if mode-name (intern mode-name) major-mode))
                   (tables (yas--get-snippet-tables mode-symbol)))
              (if tables
                  (let ((snippets '()))
                    (dolist (table tables)
                      (maphash (lambda (key template-list)
                                 (dolist (template template-list)
                                   (push (format "%s - %s" 
                                                 key
                                                 (or (yas--template-name template) ""))
                                         snippets)))
                               (yas--table-hash table)))
                    (if snippets
                        (nreverse snippets)
                      (format "No snippets found for mode '%s'" (or mode-name major-mode))))
                (format "No snippet tables found for mode '%s'" (or mode-name major-mode))))
          "Yasnippet is not active")
      (error
       (format "Error listing snippets for mode '%s': %s"
               (or mode-name "current") (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool yasnippet-insert-snippet (file-path line column snippet-key)
  "Insert a yasnippet at a specific position using its trigger key.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based, optional)
  SNIPPET-KEY - Snippet trigger key (use yasnippet_list_snippets to see keys)
  
  Expands the snippet template at the specified location."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (bound-and-true-p yas-global-mode)
              (let ((target-buffer (or (find-buffer-visiting file-path)
                                       (find-file-noselect file-path))))
                (with-current-buffer target-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    ;; Try to expand snippet by key
                    (let ((templates (yas--all-templates (yas--get-snippet-tables))))
                      (if-let ((template (cl-find snippet-key templates 
                                                  :key #'yas--template-key 
                                                  :test #'string=)))
                          (progn
                            (yas-expand-snippet (yas--template-content template))
                            (format "Inserted snippet '%s' at %s:%d:%d"
                                    snippet-key file-path line (or column 0)))
                        (format "Snippet '%s' not found" snippet-key))))))
            "Yasnippet is not active")
        (error
         (format "Error inserting snippet '%s' at %s:%d:%d: %s"
                 snippet-key file-path line (or column 0) (error-message-string err)))))))

;;; Registration function

(defun claude-code-ide-mcp-tools-register-edit-tools ()
  "Register all editing tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-edit)
;;; claude-code-ide-mcp-tools-edit.el ends here