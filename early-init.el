;;; early-init.el --- Early Startup Optimization -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; PERCHÉ: Disabilitiamo il package manager all'avvio.
;; Lo gestiremo noi esplicitamente in init.el solo quando serve.
;; Questo velocizza il boot
(setq package-enable-at-startup nil)

;; PERCHÉ: Tuning del Garbage Collector durante l'avvio.
;; Alziamo la soglia al massimo per evitare che il GC parta mentre Emacs
;; sta caricando la configurazione. Lo resetteremo in init.el.
(setq gc-cons-threshold most-positive-fixnum)

;; PERCHÉ: Disabilitiamo gli elementi della GUI *prima* che vengano disegnati.
;; Evita il "salto" grafico all'avvio e risparmia cicli di rendering.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "user")

(setq initial-scratch-message ";; Happy hacking\n\n")

;; PERCHÉ: Miglioriamo le performance di IO sui processi (utile per LSP/Eglot).
(setq read-process-output-max (* 2 1024 1024)) ;; 2MB

;;; early-init.el ends here
