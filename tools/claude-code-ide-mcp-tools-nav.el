;;; claude-code-ide-mcp-tools-nav.el --- Navigation and cursor management tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, navigation

;;; Commentary:

;; Navigation and cursor management tools for claude-code-ide-mcp-tools.
;; These tools don't require external packages and work with any Emacs setup.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; Navigation declarations
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu "imenu")
(defvar imenu--index-alist)

;;; Saved positions management

(defvar claude-code-ide-mcp-tools-saved-positions nil
  "Alist of saved cursor positions per file.")

(defun claude-code-ide-mcp-tools-find-imenu-item (symbol-name imenu-alist)
  "Search for SYMBOL-NAME in IMENU-ALIST with multiple strategies.
Returns the item if found, nil otherwise.
Tries exact match first, then partial match, then substring match."
  (or 
   ;; Strategy 1: Exact match
   (claude-code-ide-mcp-tools-find-imenu-item-exact symbol-name imenu-alist)
   ;; Strategy 2: Partial match (symbol contains the name)
   (claude-code-ide-mcp-tools-find-imenu-item-partial symbol-name imenu-alist)
   ;; Strategy 3: Look for qualified names ending with symbol-name
   (claude-code-ide-mcp-tools-find-imenu-item-suffix symbol-name imenu-alist)))

(defun claude-code-ide-mcp-tools-find-imenu-item-exact (symbol-name imenu-alist)
  "Find exact match for SYMBOL-NAME in IMENU-ALIST."
  (catch 'found
    (dolist (item imenu-alist)
      (cond
       ;; Direct match at top level
       ((and (consp item) (string= (car item) symbol-name))
        (throw 'found item))
       ;; Search in subcategories
       ((and (consp item) (listp (cdr item)))
        (let ((subitem (claude-code-ide-mcp-tools-find-imenu-item-exact symbol-name (cdr item))))
          (when subitem
            (throw 'found subitem))))))
    nil))

(defun claude-code-ide-mcp-tools-find-imenu-item-partial (symbol-name imenu-alist)
  "Find partial match for SYMBOL-NAME in IMENU-ALIST."
  (catch 'found
    (dolist (item imenu-alist)
      (cond
       ;; Partial match at top level (item name contains symbol-name)
       ((and (consp item) (string-match-p (regexp-quote symbol-name) (car item)))
        (throw 'found item))
       ;; Search in subcategories
       ((and (consp item) (listp (cdr item)))
        (let ((subitem (claude-code-ide-mcp-tools-find-imenu-item-partial symbol-name (cdr item))))
          (when subitem
            (throw 'found subitem))))))
    nil))

(defun claude-code-ide-mcp-tools-find-imenu-item-suffix (symbol-name imenu-alist)
  "Find qualified name ending with SYMBOL-NAME in IMENU-ALIST."
  (catch 'found
    (dolist (item imenu-alist)
      (cond
       ;; Suffix match at top level (item name ends with #symbol-name or similar)
       ((and (consp item) 
             (string-match-p (concat "[#\\.]" (regexp-quote symbol-name) "$") (car item)))
        (throw 'found item))
       ;; Search in subcategories
       ((and (consp item) (listp (cdr item)))
        (let ((subitem (claude-code-ide-mcp-tools-find-imenu-item-suffix symbol-name (cdr item))))
          (when subitem
            (throw 'found subitem))))))
    nil))

;;; Tool definitions

(claude-code-ide-mcp-tools-define-tool goto-line-column (file-path line column)
  "Move cursor to a specific line and column in a file.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based, optional)
  
  Essential for precise navigation before using at-point tools."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-tools-with-file-buffer file-path
      (lambda ()
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column (or column 0))
        (format "Moved to %s:%d:%d (point: %d)" 
                file-path line (or column 0) (point))))))

(claude-code-ide-mcp-tools-define-tool get-cursor-position (file-path)
  "Get the current cursor position in a file.
  
  FILE-PATH - Path to the file to check
  
  Returns line, column, and point information."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-tools-with-file-buffer file-path
      (lambda ()
        (let ((line (line-number-at-pos))
              (column (current-column))
              (point (point)))
          (format "Position in %s: line %d, column %d (point: %d)"
                  file-path line column point))))))

(claude-code-ide-mcp-tools-define-tool save-cursor-position (file-path name)
  "Save the current cursor position with a name for later restoration.
  
  FILE-PATH - Path to the file
  NAME - Name to save the position under
  
  Use this to create a bookmark before navigating elsewhere."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-tools-with-file-buffer file-path
      (lambda ()
        (let ((position (point))
              (line (line-number-at-pos))
              (column (current-column)))
          (setq claude-code-ide-mcp-tools-saved-positions
                (cons (list name file-path position line column)
                      (assq-delete-all name claude-code-ide-mcp-tools-saved-positions)))
          (format "Saved position '%s' in %s at line %d, column %d"
                  name file-path line column))))))

(claude-code-ide-mcp-tools-define-tool restore-cursor-position (name)
  "Restore a previously saved cursor position by name.
  
  NAME - Name of the saved position to restore"
  (let ((saved-pos (assoc name claude-code-ide-mcp-tools-saved-positions)))
    (if saved-pos
        (let ((file-path (nth 1 saved-pos))
              (position (nth 2 saved-pos))
              (line (nth 3 saved-pos))
              (column (nth 4 saved-pos)))
          (claude-code-ide-mcp-tools-with-file-buffer file-path
            (lambda ()
              (goto-char position)
              (format "Restored position '%s' in %s to line %d, column %d"
                      name file-path line column))))
      (format "No saved position found with name '%s'" name))))

(claude-code-ide-mcp-tools-define-tool goto-symbol (file-path symbol-name)
  "Jump to a specific symbol in a file using imenu.
  
  FILE-PATH - Path to the file
  SYMBOL-NAME - Name of the symbol to jump to
  
  Supports exact matches, partial matches, and qualified names."
  (if (not file-path)
      (error "file_path parameter is required")
    (claude-code-ide-mcp-tools-with-file-buffer file-path
      (lambda ()
        (imenu--make-index-alist)
        (if imenu--index-alist
            (let ((item (claude-code-ide-mcp-tools-find-imenu-item symbol-name imenu--index-alist)))
              (if item
                  (progn
                    (imenu item)
                    (format "Jumped to symbol '%s' in %s at line %d"
                            symbol-name file-path (line-number-at-pos)))
                (format "Symbol '%s' not found in %s" symbol-name file-path)))
          (format "No imenu support for %s" file-path))))))

;;; Registration function

(defun claude-code-ide-mcp-tools-register-nav-tools ()
  "Register all navigation tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-nav)
;;; claude-code-ide-mcp-tools-nav.el ends here
