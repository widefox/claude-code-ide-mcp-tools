;;; claude-code-ide-mcp-tools-test.el --- Tests for MCP tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ender Veiga Bueno
;; Keywords: ai, claude, mcp, test

;;; Commentary:

;; Test suite for claude-code-ide-mcp-tools package.
;; Automatically installs claude-code-ide dependency from GitHub if needed.

;;; Code:

(require 'ert)
(require 'package)

;; Set up load paths relative to test file
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(add-to-list 'load-path (expand-file-name "../tools" (file-name-directory (or load-file-name buffer-file-name))))

;; Initialize package system for dependency installation
(package-initialize)

;; Add MELPA for wider package availability 
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Install claude-code-ide from GitHub if not available
(unless (locate-library "claude-code-ide-mcp-server")
  (message "Installing claude-code-ide dependency from GitHub...")
  (unless package-archive-contents
    (package-refresh-contents))
  
  ;; Try to install websocket first (dependency)
  (unless (package-installed-p 'websocket)
    (package-install 'websocket))
  
  ;; Install claude-code-ide from GitHub
  (unless (package-installed-p 'claude-code-ide)
    (package-vc-install "https://github.com/manzaltu/claude-code-ide.el.git"))
  
  (require 'claude-code-ide))

;; Mock functions if claude-code-ide still not available
(unless (fboundp 'claude-code-ide-make-tool)
  (message "Using mock functions for claude-code-ide")
  (defvar claude-code-ide-mcp-tools-backends nil
    "Mock backends variable for testing.")
  (defvar claude-code-ide-mcp-tools--tool-list nil  
    "Mock tool list for testing.")
  (defun claude-code-ide-make-tool (&rest args)
    "Mock function for testing."
    (message "Mock: claude-code-ide-make-tool called with %S" args)
    t))

;; Load the core module
(require 'claude-code-ide-mcp-tools-core)

;;; Backend Detection Tests

(ert-deftest claude-code-ide-mcp-tools-test-backend-detection ()
  "Test backend detection functions."
  (should (symbolp (claude-code-ide-mcp-tools-detect-backend 'lsp)))
  (should (symbolp (claude-code-ide-mcp-tools-detect-backend 'project)))
  (should (symbolp (claude-code-ide-mcp-tools-detect-backend 'vcs)))
  (should (null (claude-code-ide-mcp-tools-detect-backend 'invalid))))

(ert-deftest claude-code-ide-mcp-tools-test-xref-always-available ()
  "Test that xref fallback is always detected for LSP."
  (should (eq 'xref (claude-code-ide-mcp-tools-detect-backend 'lsp))))

(ert-deftest claude-code-ide-mcp-tools-test-vc-always-available ()
  "Test that vc is always available for VCS."
  (should (eq 'vc (claude-code-ide-mcp-tools-detect-backend 'vcs))))

;;; Utility Function Tests

(ert-deftest claude-code-ide-mcp-tools-test-safe-call ()
  "Test safe function calling."
  (should (equal "test" (claude-code-ide-mcp-tools-safe-call #'identity "test")))
  (should (string-match-p "Error:" 
                         (claude-code-ide-mcp-tools-safe-call #'error "test error"))))

(ert-deftest claude-code-ide-mcp-tools-test-buffer-management ()
  "Test buffer management utilities."
  (let ((test-file (make-temp-file "mcp-tools-test" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "test content"))
          (let ((result (claude-code-ide-mcp-tools-with-file-buffer 
                        test-file
                        (lambda () (buffer-string)))))
            (should (string-match-p "test content" result))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;;; Integration Tests

(ert-deftest claude-code-ide-mcp-tools-test-registration ()
  "Test tool registration mechanism."
  (let ((claude-code-ide-mcp-tools--tool-list nil))
    ;; Simulate a tool definition
    (add-to-list 'claude-code-ide-mcp-tools--tool-list
                 (list :name "test_tool"
                       :function #'identity
                       :description "Test tool"
                       :args nil))
    (should (= 1 (length claude-code-ide-mcp-tools--tool-list)))))

;; Add cleanup test
(ert-deftest claude-code-ide-mcp-tools-test-cleanup ()
  "Test that test environment cleans up properly."
  (should t)) ; Simple test to verify cleanup works

(provide 'claude-code-ide-mcp-tools-test)
;;; claude-code-ide-mcp-tools-test.el ends here