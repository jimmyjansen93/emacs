;;; init.el --- An enhanced configuration for Rust and JS/TS development -*- lexical-binding: t -*-
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
      use-package-enable-imenu-support t)

(setq inhibit-startup-message t
      ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      warning-minimum-level :error
      split-width-threshold 0
      split-height-threshold nil
      scroll-margin 20)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-mode-lighter "")
(show-paren-mode 1)

(use-package doom-themes
  :demand t
  :config (load-theme 'doom-tokyo-night t))

(use-package no-littering
  :demand t)

(use-package super-save
  :diminish
  :config (super-save-mode 1))

(use-package recentf
  :straight (:type built-in)
  :init
  (setq recentf-max-saved-items 200
        recentf-save-file (locate-user-emacs-file "recentf")
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon nil
	doom-modeline-height 25
	doom-modeline-position-line-format '("")
	doom-modeline-workspace-name nil
	doom-modeline-project-name nil
	doom-modeline-display-default-persp-name nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-encoding nil
        doom-modeline-percent-position nil
        doom-modeline-env-version nil
        doom-modeline-lsp nil
        doom-modeline-enable-word-count nil
        doom-modeline-project-detection 'projectile))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package evil
  :preface
  (setq evil-want-C-u-scroll t
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
  :hook (evil-mode . global-evil-mc-mode))

(use-package vertico
  :demand t
  :init (vertico-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :demand t
  :init (marginalia-mode 1))

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g" . consult-goto-line)
         ("M-y" . consult-yank-pop)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package consult-todo
  :after consult
  :bind ("C-c t" . consult-todo))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 2
        corfu-auto-delay 0.0
        corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key)
  :config
  (general-define-key
    :keymaps 'help-map
    "f" #'helpful-callable
    "v" #'helpful-variable
    "k" #'helpful-key))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy)

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))

(use-package imenu-list
  :commands imenu-list-smart-toggle)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package popper
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle))
  :init
  (setq popper-reference-buffers
        '(" *Messages*"
          " *Warnings*"
          " *Help*"
          "vterm"
          "^\\*Flycheck.*"
          "^\\*Embark.*"
          "^\\*Compilation.*"
          "^\\*xref.*"
          "^\\*helpful"
          "^\\*lsp-help.*"))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter))

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
  :config
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-mode-hook-actions nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (persp-mode))

(use-package persp-projectile
  :hook (persp-mode . persp-projectile-mode))

(use-package magit
  :defer t
  :commands magit-status)

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable t
        lsp-eldoc-render-all nil
        lsp-json-schemas-update-on-startup t
        lsp-enable-snippet t
        lsp-lua-language-server-command '("/opt/homebrew/bin/lua-language-server"))
  :hook ((go-mode
           kotlin-mode
           lua-mode
           php-mode
           js-mode
           typescript-mode
           rust-mode
           yaml-mode
           zig-mode) . lsp-deferred))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t))

(use-package consult-lsp
  :after (consult lsp-mode)
  :commands consult-lsp-symbols)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package anzu
  :config (global-anzu-mode 1))

(use-package evil-anzu
  :after (anzu evil))

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (general-define-key
    :keymaps 'rust-mode-map
    :prefix "SPC"
    "cb" '(cargo-build :which-key "Cargo Build")
    "ct" '(cargo-test :which-key "Cargo Test")
    "cr" '(cargo-run :which-key "Cargo Run")
    "cc" '(cargo-check :which-key "Cargo Check")
    "cC" '(cargo-process-clippy :which-key "Cargo Clippy")))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package typescript-mode
  :mode "\\.ts[x]?\\'")

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (general-define-key
    :keymaps 'go-mode-map
    :prefix "SPC"
    "cb" '(go-build :which-key "Go Build")
    "ct" '(go-test :which-key "Go Test")
    "cr" '(go-run :which-key "Go Run")
    "cc" '(go-vet :which-key "Go Vet")))

(use-package kotlin-mode :mode "\\.kt\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package php-mode :mode "\\.php\\'")
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package zig-mode :mode "\\.zig\\'")
(use-package json-mode :mode "\\.json\\'")

(use-package vterm
  :defer t
  :commands vterm)

(use-package which-key
  :demand t
  :diminish
  :config (which-key-mode 1))

(use-package general
  :demand t
  :config
  (general-create-definer my/leader
    :keymaps '(normal visual)
    :prefix "SPC")

  (my/leader
    "SPC" '(consult-find :which-key "Find File in Project")
    ","   '(consult-recent-file :which-key "Find Recent File")
    "."   '(find-file :which-key "Find File")
    ":"   '(execute-extended-command :which-key "M-x")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(consult-buffer :which-key "Switch Buffer")
    "bk" '(kill-this-buffer :which-key "Kill Current Buffer")
    "bK" '(projectile-kill-buffers :which-key "Kill Project Buffers")

    "c" '(:ignore t :which-key "Code")
    "ca" '(lsp-execute-code-action :which-key "Code Action")
    "cb" '(projectile-compile-project :which-key "Compile Project")
    "cd" '(consult-lsp-diagnostics :which-key "Workspace Diagnostics")
    "cD" '(flycheck-list-errors :which-key "Buffer Diagnostics")
    "cf" '(lsp-format-buffer :which-key "Format Buffer")
    "ch" '(lsp-describe-thing-at-point :which-key "Describe Symbol")
    "ci" '(lsp-find-implementation :which-key "Find Implementation")
    "cR" '(lsp-rename :which-key "Rename Symbol")
    "cs" '(lsp-signature-help :which-key "Signature Help")
    "ct" '(projectile-test-project :which-key "Test Project")

    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find File")
    "fr" '(consult-recent-file :which-key "Recent Files")
    "fs" '(save-buffer :which-key "Save File")

    "g" '(:ignore t :which-key "Git")
    "gg" '(magit-status :which-key "Magit Status")
    "gb" '(magit-blame-addition :which-key "Blame")

    "h" '(:ignore t :which-key "Help")
    "hrr" '(my/reload-config :which-key "Reload Config")

    "j" '(:ignore t :which-key "Jump")
    "jj" '(avy-goto-char-2 :which-key "Avy to Char")

    "p" '(:ignore t :which-key "Project")
    "pf" '(projectile-find-file :which-key "Find File")
    "pp" '(projectile-persp-switch-project :which-key "Switch Project Workspace")
    "ps" '(persp-switch :which-key "Switch Perspective")

    "s" '(:ignore t :which-key "Search")
    "ss" '(consult-line :which-key "Search in Buffer")
    "sp" '(consult-ripgrep :which-key "Search in Project (Ripgrep)")
    "si" '(consult-imenu :which-key "Search Symbols (imenu)")

    "t" '(:ignore t :which-key "Toggles")
    "tv" '(vterm :which-key "Toggle Vterm")

    "w" '(:ignore t :which-key "Window")
    "wo" '(ace-window :which-key "Ace (Other) Window")
    "wd" '(delete-window :which-key "Delete Window")))

(define-key evil-ex-map "q" 'kill-this-buffer)

(defun my/reload-config ()
  "Reload the emacs configuration."
  (interactive)
  (load-file user-init-file))
