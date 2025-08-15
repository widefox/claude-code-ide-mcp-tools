;;; claude-code-ide-mcp-tools-treesit.el --- TreeSitter integration tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, treesitter, ast

;;; Commentary:

;; TreeSitter integration tools for claude-code-ide-mcp-tools.
;; Provides syntax tree analysis and AST navigation capabilities.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; TreeSitter declarations  
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-children "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-available-p "treesit")

(claude-code-ide-mcp-tools-define-tool treesit-analyze-structure (file-path)
  "Get the complete syntax tree structure for a file using TreeSitter.
  
  FILE-PATH - Path to the file to analyze
  
  Provides deep insights into code structure and AST analysis."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (treesit-available-p)
              (claude-code-ide-mcp-tools-with-file-buffer file-path
                (lambda ()
                  (if (treesit-parser-list)
                      (let ((root-node (treesit-buffer-root-node)))
                        (format "TreeSitter structure for %s:\n%s"
                                file-path
                                (treesit-node-text root-node)))
                    (format "No TreeSitter parser available for %s" file-path))))
            "TreeSitter is not available")
        (error
         (format "Error analyzing structure of %s: %s"
                 file-path (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool treesit-get-node-at-point (file-path line column)
  "Get detailed TreeSitter node information at a specific position.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  Shows node type, range, and text for precise syntax analysis."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (treesit-available-p)
              (let ((target-buffer (or (find-buffer-visiting file-path)
                                       (find-file-noselect file-path))))
                (with-current-buffer target-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (if (treesit-parser-list)
                        (let ((node (treesit-node-at (point))))
                          (if node
                              (format "Node at %s:%d:%d:\nType: %s\nText: %s\nRange: %d-%d"
                                      file-path line column
                                      (treesit-node-type node)
                                      (treesit-node-text node)
                                      (treesit-node-start node)
                                      (treesit-node-end node))
                            "No node found at position"))
                      (format "No TreeSitter parser available for %s" file-path)))))
            "TreeSitter is not available")
        (error
         (format "Error getting node at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool treesit-find-parent-node (file-path line column node-type)
  "Find the nearest parent node of a specific type in the syntax tree.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  NODE-TYPE - Type of parent node to find (e.g., 'function', 'class')
  
  Useful for understanding code context and structure."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (treesit-available-p)
              (let ((target-buffer (or (find-buffer-visiting file-path)
                                       (find-file-noselect file-path))))
                (with-current-buffer target-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (if (treesit-parser-list)
                        (let ((node (treesit-node-at (point)))
                              (parent nil))
                          (while (and node (not parent))
                            (setq node (treesit-node-parent node))
                            (when (and node (string= (treesit-node-type node) node-type))
                              (setq parent node)))
                          (if parent
                              (format "Found %s parent at %s:%d:%d:\nText: %s\nRange: %d-%d"
                                      node-type file-path line column
                                      (treesit-node-text parent)
                                      (treesit-node-start parent)
                                      (treesit-node-end parent))
                            (format "No %s parent found at %s:%d:%d" node-type file-path line column)))
                      (format "No TreeSitter parser available for %s" file-path)))))
            "TreeSitter is not available")
        (error
         (format "Error finding parent node at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool treesit-get-children (file-path line column)
  "Get all child nodes at a specific position in the syntax tree.
  
  FILE-PATH - Path to the file
  LINE - Line number (1-based)
  COLUMN - Column number (0-based)
  
  Shows the immediate descendants of a syntax node."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (treesit-available-p)
              (let ((target-buffer (or (find-buffer-visiting file-path)
                                       (find-file-noselect file-path))))
                (with-current-buffer target-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or column 0))
                    (if (treesit-parser-list)
                        (let ((node (treesit-node-at (point))))
                          (if node
                              (let ((children (treesit-node-children node)))
                                (if children
                                    (format "Children of node at %s:%d:%d:\n%s"
                                            file-path line column
                                            (mapconcat (lambda (child)
                                                         (format "- %s: %s"
                                                                 (treesit-node-type child)
                                                                 (treesit-node-text child)))
                                                       children "\n"))
                                  "Node has no children"))
                            "No node found at position"))
                      (format "No TreeSitter parser available for %s" file-path)))))
            "TreeSitter is not available")
        (error
         (format "Error getting children at %s:%d:%d: %s"
                 file-path line column (error-message-string err)))))))

;;; Registration function

(defun claude-code-ide-mcp-tools-register-treesit-tools ()
  "Register all TreeSitter tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-treesit)
;;; claude-code-ide-mcp-tools-treesit.el ends here
