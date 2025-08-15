;;; claude-code-ide-mcp-tools-vcs.el --- Git and version control tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, git, magit

;;; Commentary:

;; Git and version control tools for claude-code-ide-mcp-tools.
;; Integrates with Magit for comprehensive Git operations.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; Git/Magit declarations
(declare-function magit-toplevel "magit")
(declare-function magit-status "magit")
(declare-function magit-diff-staged "magit")
(declare-function magit-diff-unstaged "magit")
(declare-function magit-blame-addition "magit-blame")
(declare-function magit-log-buffer-file "magit-log")

(claude-code-ide-mcp-tools-define-tool magit-status-summary ()
  "Get a summary of the current git status for the project.
  
  Shows modified, staged, and untracked files."
  (progn
    (condition-case err
        (if (magit-toplevel)
            (let ((status-info (magit-status)))
              (format "Git status summary for %s:\n%s" 
                      (magit-toplevel) status-info))
          "Not in a git repository")
      (error
       (format "Error getting git status: %s" (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool magit-diff-summary (&optional staged)
  "Get git diff summary showing changes in the working directory or staging area.
  
  STAGED - Show staged changes if true, unstaged if false (default: false)
  
  Helps review modifications before committing."
  (progn
    (condition-case err
        (if (magit-toplevel)
            (let ((diff-info (if staged
                                 (magit-diff-staged)
                               (magit-diff-unstaged))))
              (format "%s changes:\n%s"
                      (if staged "Staged" "Unstaged") 
                      diff-info))
          "Not in a git repository")
      (error
       (format "Error getting git diff: %s" (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool git-blame-at-line (file-path line)
  "Get git blame information for a specific line.
  
  FILE-PATH - Path to the file
  LINE - Line number to blame
  
  Shows who last modified the line and when, with commit information."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (magit-toplevel)
              (let ((target-buffer (or (find-buffer-visiting file-path)
                                       (find-file-noselect file-path))))
                (with-current-buffer target-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    ;; Use git command directly for reliable blame info
                    (let* ((git-root (magit-toplevel))
                           (default-directory git-root)
                           (relative-path (file-relative-name file-path git-root))
                           (blame-output (shell-command-to-string 
                                         (format "git blame -L %d,%d '%s'" 
                                                line line relative-path))))
                      (if (string-empty-p blame-output)
                          (format "No blame information for %s:%d" file-path line)
                        (format "Blame info for %s:%d:\n%s" 
                                file-path line (string-trim blame-output)))))))
            "Not in a git repository")
        (error
         (format "Error getting blame info for %s:%d: %s"
                 file-path line (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool magit-log-file (file-path &optional max-count)
  "Get commit history for a specific file.
  
  FILE-PATH - Path to the file
  MAX-COUNT - Maximum number of commits to show (default: 10)
  
  Shows the chronological list of changes made to the file."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (if (magit-toplevel)
              (let* ((git-root (magit-toplevel))
                     (default-directory git-root)
                     (relative-path (file-relative-name file-path git-root))
                     (limit (or max-count 10))
                     (log-output (shell-command-to-string 
                                 (format "git log --oneline -n %d '%s'" 
                                        limit relative-path))))
                (if (string-empty-p (string-trim log-output))
                    (format "No git history found for %s" file-path)
                  (format "Git log for %s (last %d commits):\n%s" 
                          file-path limit log-output)))
            "Not in a git repository")
        (error
         (format "Error getting git log for %s: %s"
                 file-path (error-message-string err)))))))

;;; Registration function

;;;###autoload
(defun claude-code-ide-mcp-tools-register-vcs-tools ()
  "Register all VCS tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-vcs)
;;; claude-code-ide-mcp-tools-vcs.el ends here
