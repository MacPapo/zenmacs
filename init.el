;;; init.el --- The Zen Config (Emacs 30.2) -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

(defun zen/setup-utf8 ()
  "Forza UTF-8 ovunque. Niente più indovinelli da parte di Emacs."
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))

(zen/setup-utf8)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :init
  (fido-mode 1)

  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)

  (windmove-default-keybindings)

  (setq-default
   line-number-mode t
   column-number-mode t
   size-indication-mode t)

  (setq completion-styles '(flex substring partial-completion basic)
	completion-category-overrides
	'((file (styles partial-completion substring))
          (buffer (styles flex))
          (command (styles flex))
	  (symbol (styles flex substring))))

  (setq completion-ignore-case t
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t)

  (setq use-short-answers t
	echo-keystrokes 0.1
	visible-bell nil
	ring-bell-function 'ignore
	use-dialog-box nil)

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))

  (when (eq system-type 'darwin)

    (defun my/load-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
        ('light (load-theme 'tango t))
        ('dark (load-theme 'tango-dark t))))

    (add-hook 'ns-system-appearance-change-functions #'my/load-theme)
    (set-frame-parameter nil 'ns-transparent-titlebar t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

    (let ((gls (executable-find "gls")))
      (when gls (progn
		  (setq insert-directory-program gls))))

    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  ns-use-native-fullscreen t)

    (set-face-attribute 'default nil :family "Atkinson Hyperlegible Mono" :height 125)
    )

  (unless (eq system-type 'darwin)
    (load-theme 'modus-vivendi t nil)
    (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs nil))

  (setq max-lisp-eval-depth 10000)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (flymake-mode nil "flymake"))))

(use-package completion-preview
  :init
  (global-completion-preview-mode 1)
  :bind (:map completion-preview-active-mode-map
              ("M-n"   . completion-preview-next-candidate)
              ("M-p"   . completion-preview-prev-candidate)))

(use-package autorevert
  :init
  (global-auto-revert-mode 1)
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (setq auto-revert-use-notify nil))

(use-package subword
  :init
  (global-subword-mode 1))

(use-package so-long
  :init
  (global-so-long-mode 1))

(use-package winner
  :config
  (winner-mode 1))

(use-package whitespace
  :hook (prog-mode)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom
  (indicate-buffer-boundaries 'left)
  (display-fill-column-indicator-character ?¦))

(use-package copyright
  :hook (before-save . copyright-update))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package isearch
  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (search-ring-max 26)
  (regexp-search-ring-max 26)
  (isearch-lax-whitespace t)
  (isearch-repeat-on-direct-change t)
  (isearch-wrap-pause nil)
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-no-delay-length 4)
  (lazy-count-prefix-format "[%s of %s] ")
  (isearch-forward-thing-at-point '(region url email symbol sexp))
  (isearch-allow-prefix t))

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers "go.mod"))

(use-package vc
  :bind (("C-x g" . vc-dir))
  :config
  (setq vc-follow-symlinks t)
  (add-hook 'vc-dir-mode-hook 'hl-line-mode))

(use-package eglot
  :config
  (setq eglot-events-buffer-config '(:size 0)
	eglot-send-changes-idle-time 0.1
	eglot-extend-to-xref t)
  (add-to-list 'eglot-stay-out-of 'font-lock)

  (defun zen/eglot-on-connect ()
    "Attiva feature UI aggiuntive quando Eglot si connette."
    (eglot-inlay-hints-mode 1))

  (defun zen/eglot-enable-save-actions ()
    "Attiva Organize Imports (se supportato) e Formattazione al salvataggio."
    (add-hook 'before-save-hook
              (lambda ()
                (when (eglot-managed-p)
                  (ignore-errors (call-interactively #'eglot-code-action-organize-imports))
                  (eglot-format-buffer)))
              nil t)))

(use-package treesit
  :mode (("\\.go\\'" . go-ts-mode)
	 ("/go\\.mod\\'"    . go-mod-ts-mode)
         ("\\.y[a]?ml\\'" . yaml-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.c\\'"   . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.h\\'"   . c-or-c++-ts-mode)
	 ("/Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-ts-mode)
         ("\\.dockerfile\\'"             . dockerfile-ts-mode))

  :init
  (setq major-mode-remap-alist
	'((yaml-mode . yaml-ts-mode)
	  (js-mode   . js-ts-mode)
	  (json-mode . json-ts-mode)
	  (ruby-mode . ruby-ts-mode)
	  (c-mode    . c-ts-mode)
          (c++-mode  . c++-ts-mode)))
  :config
  (setq treesit-language-source-alist
	'((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

  (defun zen/install-all-treesit-langs ()
    "Install all Tree-sitter languages defined in `treesit-language-source-alist`."
    (interactive)
    (dolist (lang treesit-language-source-alist)
      (let ((lang-symbol (car lang)))
	(if (treesit-language-available-p lang-symbol)
            (message "Tree-sitter grammar for %s is already installed." lang-symbol)
          (message "Installing Tree-sitter grammar for %s..." lang-symbol)
          (treesit-install-language-grammar lang-symbol)))))
  )

(use-package c-ts-mode
  :defer t
  :hook ((c-ts-mode . eglot-ensure)
         (c-ts-mode . zen/c-setup-buffer))
  :config
  (setq c-ts-mode-indent-style 'linux)
  (setq c-ts-mode-indent-offset 8)

  (defun zen/clang-format-buffer ()
    "Formatta il buffer usando clang-format esterno e applica diff safe."
    (interactive)
    (unless (executable-find "clang-format")
      (user-error "clang-format non trovato"))

    (let ((buf (current-buffer))
          (binary "clang-format"))

      (with-temp-buffer
        (let ((temp-buf (current-buffer)))
          ;; 1. Eseguiamo clang-format su tutto il contenuto
          (with-current-buffer buf
            (call-process-region (point-min) (point-max) binary nil temp-buf nil "-style=file"))

          ;; 2. Se c'è output valido, applichiamo le differenze
          (if (> (point-max) 1)
              (let ((formatted-content (buffer-string)))
                (with-current-buffer buf
                  (replace-region-contents
                   (point-min) (point-max)
                   (lambda () formatted-content))
                  ;; Messaggio discreto (appare solo se lo chiami manualmente)
                  (message "Zen: Buffer formattato.")))
            (message "Zen: Errore clang-format (output vuoto)."))))))

  (defun zen/eglot-format-dispatcher (orig-fun &rest args)
    "Devia la richiesta di formattazione alla nostra funzione su C/C++."
    (if (derived-mode-p 'c-ts-mode 'c++-ts-mode 'c-or-c++-ts-mode)
        (zen/clang-format-buffer)
      (apply orig-fun args)))

  (defun zen/c-setup-buffer ()
    (setq indent-tabs-mode t)
    (setq tab-width 8)
    (setq-local eglot-ignored-server-capabilities
                '(:documentOnTypeFormattingProvider))
    (zen/eglot-enable-save-actions))

  (with-eval-after-load 'eglot
    (advice-add 'eglot-format :around #'zen/eglot-format-dispatcher)
    (add-to-list 'eglot-server-programs
                 '((c-ts-mode c++-ts-mode c-or-c++-ts-mode)
                   . ("clangd"
                      "-j=8"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0")))))

(use-package go-ts-mode
  :defer t
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . zen/go-setup-buffer)
         (eglot-managed-mode . zen/eglot-on-connect))
  :config
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (completeUnimported . t)
                           (usePlaceholders . t)
                           (analyses . ((unusedparams . t)
                                        (shadow . t)
                                        (nilness . t)
                                        (unusedwrite . t)))
                           (hints . ((compositeLiteralFields . t)
                                     (constantValues . t)
                                     (functionTypeParameters . t)
                                     (parameterNames . t)))
                           (gofumpt . t)))))

  (defun zen/go-setup-buffer ()
    (setq indent-tabs-mode t) ; Tab reali per Go
    (zen/eglot-enable-save-actions)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(safe-local-variable-values '((checkdoc-minor-mode . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
