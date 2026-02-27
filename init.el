;;; init.el --- The Zen Config (Emacs 30.2) -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

(prefer-coding-system 'utf-8)
(setq read-process-output-max (* 2 1024 1024)) ;; 2MB

;; Reset del Garbage Collector a boot finito (16MB è il sweet spot moderno)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(when (memq window-system '(mac ns))
  (let ((my-paths '("/opt/homebrew/bin"
                    "/opt/homebrew/sbin"
                    "~/.local/share/mise/shims"
                    "/usr/local/bin"
                    "/usr/bin"
                    "/bin")))
    (setq exec-path (append (mapcar #'expand-file-name my-paths) exec-path))
    (setenv "PATH" (mapconcat #'identity exec-path ":"))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

(use-package emacs
  :init
  ;; --- Identità e Sicurezza ---
  ;; Carica il file dei segreti se esiste (ignorando gli errori se manca)
  (let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
    (when (file-exists-p secrets-file)
      (load secrets-file nil t)))

  ;; --- Moduli Custom (Stile Purcell) ---
  ;; Aggiungi la cartella 'lisp' ai percorsi in cui Emacs cerca i file sorgente
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  ;; --- UI Pulita ---
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        initial-scratch-message ";; Happy hacking\n\n"
        use-short-answers t
        echo-keystrokes 0.1
        visible-bell nil
        ring-bell-function 'ignore
        use-dialog-box nil
	indicate-buffer-boundaries 'left
        inhibit-startup-echo-area-message (user-login-name))

  ;; (require 'zen-editing)

  ;; --- Comportamenti e Default Moderni (Loot di Bedrock) ---
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq sentence-end-double-space nil)
  (setq switch-to-buffer-obey-display-actions t)

  ;; --- Scorrimento Fluido al Pixel ---
  (pixel-scroll-precision-mode 1)
  (setq mouse-wheel-tilt-scroll t)
  (setq mouse-wheel-flip-direction t)

  ;; --- Editing Base ---
  (delete-selection-mode 1)
  (electric-pair-mode 1)

  ;; Sblocco comandi avanzati
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; --- Minibuffer e Autocompletamento (Fase 2) ---
  (fido-vertical-mode 1)

  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-extended-command-predicate #'command-completion-default-include-p)

  ;; --- Memoria e Navigazione ---
  (savehist-mode 1)
  (setq history-length 100)

  (recentf-mode 1)
  (setq recentf-max-saved-items 100)
  (setq recentf-keep '(file-remote-p file-readable-p)) ;; Evita freeze con file di rete

  (windmove-default-keybindings)

  ;; --- Filesystem e Backup ---
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))

  ;; --- Tema e Font ---
  (load-theme 'tango t nil)
  (setq font-lock-maximum-decoration 1)

  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'none
          ns-use-native-fullscreen t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light))

    (set-face-attribute 'default nil :family "Atkinson Hyperlegible Mono" :height 125)
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))))

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh")
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  :config
  (setq tramp-auto-save-directory (concat user-emacs-directory "tramp-auto-save/")))

(use-package completion-preview
  :init
  (global-completion-preview-mode 1)
  :bind (:map completion-preview-active-mode-map
              ("M-n"   . completion-preview-next-candidate)
              ("M-p"   . completion-preview-prev-candidate)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

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

(use-package repeat
  :init
  (repeat-mode 1))

(use-package winner
  :config
  (winner-mode 1))

(use-package window
  :custom
  (display-buffer-alist
   '(
     ;; 1. BUFFER DI AIUTO E INFORMAZIONI
     ;; Nome del buffer inizia con *Help*, *Apropos*, *info*, *Messages* ecc.
     ("\\*\\(Help\\|Apropos\\|info\\|Messages\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.3)        ; Occupa il 30% dell'altezza dello schermo
      (reusable-frames . visible)) ; Usa la finestra se esiste già

     ;; 2. COMPILAZIONE E TEST
     ;; I buffer *compilation* (es. quando lanci 'M-x compile' per Go o C)
     ("\\*compilation\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.3)
      (reusable-frames . visible))

     ;; 3. COMPLETAMENTO (Completions)
     ;; Se decidi di usare finestre di completamento standard in futuro
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.2)))))

(use-package whitespace
  :init
  (setq whitespace-style '(face empty trailing lines-tail))
  :hook ((prog-mode . whitespace-mode)
	 (text-mode . whitespace-mode)))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom
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
  (isearch-allow-prefix t)
  (isearch-allow-motion t)
  (isearch-yank-on-move 'shift))

(use-package dired
  :custom
  ;; -l: formato lungo, -h: human readable, -v: sort naturale (numeri), --group-directories-first: cartelle in alto
  (dired-listing-switches "-lhv --group-directories-first")
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package which-key
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.5))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-always-kill t)
  ;; Quando premi M-g n (next-error) per saltare tra gli errori nel codice,
  ;; ignora i semplici "info" o "warning" e fermati solo sugli errori critici (livello 2).
  (compilation-skip-threshold 2)
  (compilation-ask-about-save nil)
  :bind
  (("C-c c" . compile)
   ("C-c C" . project-compile)))

(use-package project
  :custom
  (project-mode-line t)
  :config
  (add-to-list 'project-vc-extra-root-markers "go.mod"))

(use-package vc
  :bind (("C-x g" . vc-dir))
  :config
  (setq vc-follow-symlinks t)
  (add-hook 'vc-dir-mode-hook 'hl-line-mode))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0))
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-stay-out-of 'font-lock)

  (defun zen/eglot-setup-buffer ()
    "Attiva inlay hints, formattazione e organize imports per questo buffer."
    (eglot-inlay-hints-mode 1)
    (add-hook 'before-save-hook
              (lambda ()
                (when (eglot-managed-p)
                  (ignore-errors (call-interactively #'eglot-code-action-organize-imports))
                  (ignore-errors (eglot-format-buffer))))
              nil t)) ; 't' finale = locale al buffer, vitale!
  (add-hook 'eglot-managed-mode-hook #'zen/eglot-setup-buffer))

(use-package c-mode
  :defer t
  :hook (c-mode . eglot-ensure))

(use-package js
  :defer t
  :hook (js-mode . eglot-ensure))

(use-package go-mode
  :ensure t
  :defer t
  :hook ((go-mode . eglot-ensure)
	 (go-mode . (lambda () (setq indent-tabs-mode t)))))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

;; --- Customizzazioni Autogenerate ---
;; Carica il file custom alla fine per garantirgli la priorità
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;;; init.el ends here
