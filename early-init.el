;;; early-init.el --- Early Startup Optimization -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; 1. PACKAGE MANAGER
;; Disabilitiamo il caricamento automatico per gestirlo in init.el
(setq package-enable-at-startup nil)

;; 2. GARBAGE COLLECTOR
;; Lo disattiviamo completamente durante il boot per massima velocità
(setq gc-cons-threshold most-positive-fixnum)

;; 3. PRE-RENDERING GUI
;; Spegniamo i fronzoli prima che la finestra venga disegnata (evita il flash)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Utile per i Tiling Window Manager (i3, sway, o su macOS) per evitare gap strani
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;;; early-init.el ends here
