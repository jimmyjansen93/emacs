;; Ensure these are available at compile time for native-comp warnings
(eval-when-compile
  (require 'use-package)
  (require 'general)
  (require 'lsp-mode nil t)
  (require 'eglot nil t)
  (require 'flyspell nil t))

;; Ensure Homebrew binaries are in PATH and exec-path (for native compilation, etc.)
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/homebrew/bin")

;; Basic package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Disable backup and auto-save files
(setq make-backup-files nil)      ; Don't create backup~ files
(setq auto-save-default nil)      ; Don't create #autosave# files
(setq create-lockfiles nil)       ; Don't create .#lock files

;; Sensible defaults
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)

;; Font (optional, comment out if you want system default)
;; (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130)

;; Evil mode and evil-collection
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; which-key for keybinding hints
(use-package which-key
  :config
  (which-key-mode))

;; Company for autocompletion
(use-package company
  :hook (after-init . global-company-mode))

;; LSP Mode and UI
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((bash-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (kotlin-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (markdown-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (zig-mode . lsp-deferred))
  :config
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

;; Language-specific major modes (ensure they're installed)
(use-package go-mode)
(use-package kotlin-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package php-mode)
(use-package rust-mode)
(use-package typescript-mode)
(use-package yaml-mode)
(use-package zig-mode)

;; Tokyo Night theme (doom-themes version)
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

;; Modeline (optional, but nice)
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Icons (for modeline and file trees)
(use-package nerd-icons)

;; Final touches
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Projectile for project management
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Ivy for completion
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :after ivy)

;; Treemacs for file tree sidebar
(use-package treemacs
  :defer t)
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Magit for git integration
(use-package magit)

;; Flycheck for syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; vterm for terminal integration
(use-package vterm
  :commands vterm)

;; format-all for auto-formatting
(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter))

;; general.el for keybinding management and space leader
(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (my/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open-files :which-key "recent files")
    "p"  '(:ignore t :which-key "project")
    "pf" '(projectile-find-file :which-key "project find file")
    "pp" '(projectile-switch-project :which-key "switch project")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(ivy-switch-buffer :which-key "switch buffer")
    "w"  '(:ignore t :which-key "windows")
    "q"  '(:ignore t :which-key "quit")
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")
    "t"  '(:ignore t :which-key "toggle")
    "tt" '(treemacs :which-key "treemacs")
    "v"  '(:ignore t :which-key "vterm")
    "vv" '(vterm :which-key "vterm")
    ))

;; recentf for recent files
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))

;; undo-tree for visual undo history
(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode 1))

;; Org-mode enhancements
(use-package org
  :config
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  ;; Use 8 parallel jobs for native compilation
  (setq native-comp-async-jobs-number 8)
  ;; Compile this init.el file to native code if not already compiled
  (native-compile (or load-file-name (buffer-file-name))))

(provide 'init)
