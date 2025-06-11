(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/homebrew/bin")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer 1)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130)

(use-package evil
  :preface
  (setq evil-want-C-u-scroll t)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :if (package-installed-p 'evil)
  :config
  (evil-collection-init))

(use-package which-key
  :config
  (which-key-mode))

(use-package company
  :hook (after-init . global-company-mode))

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
  :if (package-installed-p 'lsp-mode)
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package go-mode
  :if (package-installed-p 'lsp-mode))
(use-package kotlin-mode
  :if (package-installed-p 'lsp-mode))
(use-package lua-mode
  :if (package-installed-p 'lsp-mode))
(use-package markdown-mode
  :if (package-installed-p 'lsp-mode))
(use-package php-mode
  :if (package-installed-p 'lsp-mode))
(use-package rust-mode
  :if (package-installed-p 'lsp-mode))
(use-package typescript-mode
  :if (package-installed-p 'lsp-mode))
(use-package yaml-mode
  :if (package-installed-p 'lsp-mode))
(use-package zig-mode
  :if (package-installed-p 'lsp-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package nerd-icons)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-project-search-path
        '("~/.config"
          "~/projects/work"
          "~/projects/private"))
  (projectile-mode 1)
  (projectile-discover-projects-in-search-path)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-count 40))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :if (package-installed-p 'vertico)
  :after vertico
  :init
  (marginalia-mode 1))

(use-package magit)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package vterm
  :commands vterm)

(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter))

(use-package ivy)

(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (my/leader-keys
    ":"  '(execute-extended-command :which-key "M-x")
    "," '(recentf-open-files :which-key "recent files")
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open-files :which-key "recent files")
    "fd" '(my/dired-maximize :which-key "dired (maximize, q to restore)")
    "p"  '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "Projects list")
    "pd" '(my/projectile-dired-maximize :which-key "projectile-dired (maximize, q to restore)")
    "h"  '(:ignore t :which-key "help")
    "hr" '(:ignore t :which-key "reload")
    "hrr" '(my/reload-config :which-key "reload config")
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

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))
(defvar my/dired-window-config nil
  "Window configuration before opening full-frame Dired.")

(defun my/dired-maximize ()
  "Open Dired in the whole frame, but allow easy restoration."
  (interactive)
  (setq my/dired-window-config (current-window-configuration))
  (dired default-directory)
  (delete-other-windows)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "q") #'my/dired-restore-windows)
    (use-local-map map)))

(defun my/dired-restore-windows ()
  "Restore the previous window configuration and kill Dired buffer."
  (interactive)
  (when my/dired-window-config
    (set-window-configuration my/dired-window-config)
    (setq my/dired-window-config nil)))

(defun my/projectile-dired-maximize ()
  "Open projectile-dired in the whole frame, but allow easy restoration."
  (interactive)
  (setq my/dired-window-config (current-window-configuration))
  (projectile-dired)
  (delete-other-windows)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "q") #'my/dired-restore-windows)
    (use-local-map map)))

(with-eval-after-load 'evil
  (require 'evil))
(with-eval-after-load 'projectile
  (require 'projectile))

(defun my/reload-config ()
  "Reload the Emacs config (init.el) without restarting."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'init)
