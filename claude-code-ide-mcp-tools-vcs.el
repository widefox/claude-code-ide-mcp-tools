;;; claude-code-ide-mcp-tools-vcs.el --- Git and version control tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, git, vc

;;; Commentary:

;; Git and version control tools for claude-code-ide-mcp-tools.
;; Uses direct git commands for reliable, dependency-free operation.
;; Works with or without vc package.

;;; Code:

(require 'claude-code-ide-mcp-tools-core)

;; VC function declarations
(declare-function vc-root-dir "vc")

;;; Helper functions

(defun claude-code-ide-mcp-tools--find-git-root (&optional dir)
  "Find the git repository root starting from DIR (or current directory).
  
  Returns the path to the git root directory, or nil if not in a git repository.
  Uses vc-root-dir if available, otherwise falls back to direct git command."
  (let ((start-dir (or dir default-directory)))
    (or
     ;; Try vc-root-dir if available
     (and (fboundp 'vc-root-dir)
          (vc-root-dir start-dir))
     ;; Fallback to git command
     (let ((default-directory start-dir))
       (condition-case nil
           (let ((git-root (string-trim 
                           (shell-command-to-string "git rev-parse --show-toplevel"))))
             (and (not (string-empty-p git-root))
                  (file-directory-p git-root)
                  git-root))
         (error nil))))))

(claude-code-ide-mcp-tools-define-tool git-status-summary ()
  "Get a summary of the current git status for the project.
  
  Shows modified, staged, and untracked files."
  (progn
    (condition-case err
        (let ((git-root (claude-code-ide-mcp-tools--find-git-root)))
          (if git-root
              (let ((default-directory git-root)
                    (status-output (shell-command-to-string "git status --short")))
                (if (string-empty-p (string-trim status-output))
                    (format "Git repository at %s is clean (no changes)" git-root)
                  (format "Git status for %s:\n%s" git-root status-output)))
            "Not in a git repository"))
      (error
       (format "Error getting git status: %s" (error-message-string err))))))

(claude-code-ide-mcp-tools-define-tool git-diff-summary (&optional staged)
  "Get git diff summary showing changes in the working directory or staging area.
  
  STAGED - Show staged changes if true, unstaged if false (default: false)
  
  Helps review modifications before committing."
  (progn
    (condition-case err
        (let ((git-root (claude-code-ide-mcp-tools--find-git-root)))
          (if git-root
              (let* ((default-directory git-root)
                     (diff-command (if staged "git diff --cached" "git diff"))
                     (diff-output (shell-command-to-string diff-command)))
                (if (string-empty-p (string-trim diff-output))
                    (format "No %s changes" (if staged "staged" "unstaged"))
                  (format "%s changes:\n%s"
                          (if staged "Staged" "Unstaged") 
                          diff-output)))
            "Not in a git repository"))
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
          (let ((git-root (claude-code-ide-mcp-tools--find-git-root)))
            (if git-root
                (let* ((default-directory git-root)
                       (relative-path (file-relative-name file-path git-root))
                       (blame-output (shell-command-to-string 
                                     (format "git blame -L %d,%d '%s'" 
                                            line line relative-path))))
                  (if (string-empty-p (string-trim blame-output))
                      (format "No blame information for %s:%d" file-path line)
                    (format "Blame info for %s:%d:\n%s" 
                            file-path line (string-trim blame-output))))
              "Not in a git repository"))
        (error
         (format "Error getting blame info for %s:%d: %s"
                 file-path line (error-message-string err)))))))

(claude-code-ide-mcp-tools-define-tool git-log-file (file-path &optional max-count)
  "Get commit history for a specific file.
  
  FILE-PATH - Path to the file
  MAX-COUNT - Maximum number of commits to show (default: 10)
  
  Shows the chronological list of changes made to the file."
  (if (not file-path)
      (error "file_path parameter is required")
    (progn
      (condition-case err
          (let ((git-root (claude-code-ide-mcp-tools--find-git-root)))
            (if git-root
                (let* ((default-directory git-root)
                       (relative-path (file-relative-name file-path git-root))
                       (limit (or max-count 10))
                       (log-output (shell-command-to-string 
                                   (format "git log --oneline -n %d '%s'" 
                                          limit relative-path))))
                  (if (string-empty-p (string-trim log-output))
                      (format "No git history found for %s" file-path)
                    (format "Git log for %s (last %d commits):\n%s" 
                            file-path limit log-output)))
              "Not in a git repository"))
        (error
         (format "Error getting git log for %s: %s"
                 file-path (error-message-string err)))))))

;;; Registration function

(defun claude-code-ide-mcp-tools-register-vcs-tools ()
  "Register all VCS tools with claude-code-ide."
  (claude-code-ide-mcp-tools-register-pending))

(provide 'claude-code-ide-mcp-tools-vcs)
;;; claude-code-ide-mcp-tools-vcs.el ends here
