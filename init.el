;;; init.el --- my config -*- lexical-binding: t -*-
;;; Commentary: Personal Emacs configuration

(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/homebrew/bin")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defvar bootstrap-version 6)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(require 'use-package)

(setq use-package-expand-minimally t
      use-package-enable-imenu-support t
      warning-minimum-level :error)

(setq inhibit-startup-message t
      ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      warning-minimum-level :error
      split-width-threshold 0
      split-height-threshold nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130)

(use-package doom-themes
  :demand t
  :config (load-theme 'doom-tokyo-night t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package no-littering
  :demand t)

(use-package super-save
  :diminish
  :config (super-save-mode 1))

(when (fboundp 'global-tree-sitter-mode)
  (use-package tree-sitter
    :demand t
    :config (global-tree-sitter-mode))
  (use-package tree-sitter-langs
    :after tree-sitter)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package vertico
  :demand t
  :init (vertico-mode 1))

(use-package orderless
  :demand t
  :init (setq completion-styles '(orderless basic)))

(use-package marginalia
  :demand t
  :init (marginalia-mode 1))

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)))

(use-package consult-project-extra
  :after consult
  :defer t
  :commands consult-project-extra-find
  :bind (("C-c p f" . consult-project-extra-find)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(use-package evil
  :preface (setq evil-want-C-u-scroll t
                 evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-window-map))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package undo-fu
  :after evil
  :bind (:map evil-normal-state-map
              ("u" . undo-fu-only-undo)
              ("C-r" . undo-fu-only-redo)))

(use-package evil-mc
  :after evil
  :defer t
  :hook (evil-mode . global-evil-mc-mode))

(use-package avy
  :bind (("C-'" . avy-goto-char-2)))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))

(use-package imenu-list
  :commands imenu-list-smart-toggle)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package popper
  :defer t
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '(" *Messages*"
          " *Warnings*"
          " *Help*"
          "^\\*Flycheck.*"
          "^\\*Embark.*"
          "^\\*Compilation.*"
          "^\\*xref.*"))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package magit
  :defer t
  :commands magit-status)

(use-package forge
  :after magit
  :defer t)

(use-package magit-todos
  :after magit
  :defer t
  :hook (magit-mode . magit-todos-mode))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-mode-map
              ("C-SPC" . company-complete)
         :map company-active-map
              ("C-SPC" . company-complete)
              ("C-y" . company-complete-selection)
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (setq company-idle-delay nil
        company-minimum-prefix-length 1))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode kotlin-mode lua-mode php-mode rust-mode typescript-mode yaml-mode zig-mode) . lsp-deferred)
  :config
  (setq lsp-enable-snippet t)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-json)
    (require 'lsp-yaml)))

(use-package lsp-ui
  :after lsp-mode
  :defer t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp-mode
  :defer t)

(use-package consult-lsp
  :after (consult lsp-mode)
  :defer t
  :commands consult-lsp-symbols)

(use-package go-mode :defer t)
(use-package kotlin-mode :defer t)
(use-package lua-mode :defer t)
(use-package markdown-mode :defer t)
(use-package php-mode :defer t)
(use-package rust-mode
  :defer t
  :hook (rust-mode . (lambda () (setq-local rust-format-on-save t))))
(use-package cargo
  :after rust-mode
  :defer t
  :hook (rust-mode . cargo-minor-mode))
(use-package typescript-mode :defer t)
(use-package yaml-mode :defer t)
(use-package zig-mode :defer t)
(use-package yasnippet
  :defer t
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets
  :after yasnippet
  :defer t)

(use-package projectile
  :diminish
  :demand t
  :config
  (setq projectile-project-search-path
        '("~/.config/" "~/projects/work" "~/projects/private"))
  (projectile-discover-projects-in-search-path)
  (projectile-mode 1))

(use-package perspective
  :demand t
  :init (setq persp-suppress-no-prefix-key-warning t)
  :config (persp-mode))

(use-package nerd-icons :if (display-graphic-p) :defer t)
(use-package all-the-icons :if (display-graphic-p) :defer t)
(use-package all-the-icons-dired :if (display-graphic-p) :defer t :hook (dired-mode . all-the-icons-dired-mode))
(use-package which-key :demand t :diminish :config (which-key-mode 1))

(use-package vterm :defer t :commands vterm)
(use-package format-all :defer t :hook (prog-mode . format-all-ensure-formatter))

(use-package general
  :demand t
  :config
  (general-create-definer my/leader :keymaps '(normal visual) :prefix "SPC")
  (my/leader
    ":" '(execute-extended-command :which-key "M-x")
    "b" '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch")
    "bsh" '(bsh :which-key "hsplit")
    "bsv" '(bsv :which-key "vsplit")
    "c" '(:ignore t :which-key "code")
    "ca" '(lsp-execute-code-action :which-key "code action")
    "cb" '(compile :which-key "compile")
    "cc" '(lsp-rename :which-key "rename")
    "cd" '(lsp-ui-flycheck-list :which-key "diagnostics")
    "cD" '(lsp-treemacs-errors-list :which-key "proj diag")
    "ce" '(flycheck-list-errors :which-key "buffer diag")
    "cf" '(lsp-format-buffer :which-key "lsp format")
    "cF" '(format-all-buffer :which-key "fmt all")
    "ch" '(lsp-describe-thing-at-point :which-key "describe")
    "ci" '(lsp-find-implementation :which-key "impl")
    "cl" '(lsp-ui-flycheck-list :which-key "diag list")
    "cr" '(recompile :which-key "recompile")
    "cs" '(lsp-signature-help :which-key "signature")
    "ct" '(projectile-test-project :which-key "test")
    "f" '(:ignore t :which-key "files")
    "fd" '(my/dired-maximize :which-key "dired")
    "ff" '(find-file :which-key "find")
    "fr" '(consult-recent-file :which-key "recent")
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "status")
    "h" '(:ignore t :which-key "help")
    "hrr" '(my/reload-config :which-key "reload")
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch")
    "pf" '(projectile-find-file :which-key "find file")
    "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
    "pK" '(projectile-kill-buffers :which-key "kill buffers")
    "pd" '(my/projectile-dired-maximize :which-key "dired max")
    "s" '(:ignore t :which-key "search")
    "sr" '(consult-ripgrep :which-key "ripgrep")
    "si" '(consult-imenu :which-key "imenu")
    "t" '(:ignore t :which-key "toggles")
    "tt" '(treemacs :which-key "treemacs")
    "w" '(:ignore t :which-key "window")
    "wo" '(ace-window :which-key "ace")
    "wd" '(delete-window :which-key "delete")
    "v" '(:ignore t :which-key "vterm")
    "vv" '(vterm :which-key "vterm")))

(use-package nerd-icons-which-key
  :straight (nerd-icons-which-key
             :type git :host github :repo "jdtsmith/nerd-icons-which-key")
  :after (which-key nerd-icons)
  :config (nerd-icons-which-key-mode))

(defvar my/dired-window-config nil)

(defun my/dired-maximize ()
  "Dired maximize."
  (interactive)
  (setq my/dired-window-config (current-window-configuration))
  (dired default-directory)
  (delete-other-windows)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "q") #'my/dired-restore-windows)
    (use-local-map map)))

(defun my/dired-restore-windows ()
  "Dired restore windows."
  (interactive)
  (when my/dired-window-config
    (set-window-configuration my/dired-window-config)
    (setq my/dired-window-config nil)))

(defun my/reload-config ()
  "Reload config."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun bsh ()
  "Buffer split horizontal."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun bsv ()
  "Buffer split vertical."
  (interactive)
  (split-window-right)
  (other-window 1))

;;; init.el ends here
