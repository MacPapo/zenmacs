;;; zen-editing.el --- Le mie funzioni custom per il testo -*- lexical-binding: t; -*-

;;; Commentary:
;; Sandbox per testare le funzioni di manipolazione del testo.

;;; Code:

(defun zen/duplicate-dwim ()
  "Duplica la regione attiva se presente, altrimenti duplica la riga corrente.
Non inquina il `kill-ring'."
  (interactive)
  (let (beg end col text)
    (if (use-region-p)
        ;; ==========================================
        ;; CASO 1: C'è una selezione attiva
        ;; ==========================================
        (progn
          ;; 1. Trova le coordinate
          (setq beg (region-beginning))
          (setq end (region-end))

          ;; 2. Estrai il testo puro
          (setq text (buffer-substring-no-properties beg end))

          ;; 3. Vai alla fine della selezione e togli l'evidenziazione
          (goto-char end)
          (deactivate-mark)

          ;; 4. Incolla il testo (verrà affiancato)
          (insert text))

      ;; ==========================================
      ;; CASO 2: Nessuna selezione (Duplica riga)
      ;; ==========================================
      (progn
        ;; 1. Trova le coordinate della riga
        (setq beg (line-beginning-position))
        (setq end (line-end-position))
        (setq col (current-column))

        ;; 2. Estrai il testo puro
        (setq text (buffer-substring-no-properties beg end))

        ;; 3. Vai alla fine della riga
        (goto-char end)

        ;; 4. Vai a capo
        (newline)

        ;; 5. Incolla il testo
        (insert text)
        (move-to-column col)))))

(defun zen/move-up-dwim ()
  "Sposta la riga o le righe selezionate verso l'alto come nei moderni editor.
Utilizza la matematica degli offset per una precisione chirurgica."
  (interactive)
  (let* ((is-region-active (use-region-p))
         beg end text insert-pos pt-offset mk-offset)

    ;; ==========================================
    ;; 1. Espansione ai confini reali
    ;; ==========================================
    (if is-region-active
        (progn
          (setq beg (save-excursion
                      (goto-char (region-beginning))
                      (line-beginning-position)))
          (setq end (save-excursion
                      (goto-char (region-end))
                      (if (and (bolp) (> (point) beg))
                          (point)
                        (line-beginning-position 2)))))
      (progn
        (setq beg (line-beginning-position))
        (setq end (line-beginning-position 2))))

    ;; ==========================================
    ;; 2. GUARD CLAUSE (Matematica Pura)
    ;; ==========================================
    (if (= beg (point-min))
        (message "Zen: Sei già alla prima riga!")

      (progn
        ;; ==========================================
        ;; 3. Calcolo degli Offset
        ;; ==========================================
        (setq pt-offset (- (point) beg))
        (when is-region-active
          (setq mk-offset (- (mark) beg)))

        ;; ==========================================
        ;; 4. Estrazione Pulita
        ;; ==========================================
        (setq text (delete-and-extract-region beg end))

        ;; ==========================================
        ;; 5. Salto
        ;; ==========================================
        (forward-line -1)
        (setq insert-pos (point))

        ;; ==========================================
        ;; 6. Iniezione e Ripristino
        ;; ==========================================
        (insert text)
        (if is-region-active
            (progn
              (push-mark (+ insert-pos mk-offset) t t)
              (goto-char (+ insert-pos pt-offset))
              (setq deactivate-mark nil))
          (goto-char (+ insert-pos pt-offset)))))))

(defun zen/move-down-dwim ()
  "Sposta la riga o le righe selezionate verso il basso come nei moderni editor.
Utilizza la matematica degli offset per una precisione chirurgica."
  (interactive)
  (let* ((is-region-active (use-region-p))
         beg end text insert-pos pt-offset mk-offset)

    ;; ==========================================
    ;; 1. Espansione ai confini reali
    ;; ==========================================
    (if is-region-active
        (progn
          (setq beg (save-excursion
                      (goto-char (region-beginning))
                      (line-beginning-position)))
          (setq end (save-excursion
                      (goto-char (region-end))
                      (if (and (bolp) (> (point) beg))
                          (point)
                        (line-beginning-position 2)))))
      (progn
        (setq beg (line-beginning-position))
        (setq end (line-beginning-position 2))))

    ;; ==========================================
    ;; 2. GUARD CLAUSE (Matematica Pura)
    ;; ==========================================
    (if (= end (point-max))
        (message "Zen: Sei già all'ultima riga!")

      (progn
        ;; ==========================================
        ;; 3. Calcolo degli Offset
        ;; ==========================================
        (setq pt-offset (- (point) beg))
        (when is-region-active
          (setq mk-offset (- (mark) beg)))

        ;; ==========================================
        ;; 4. Estrazione Pulita
        ;; ==========================================
        (setq text (delete-and-extract-region beg end))

        ;; ==========================================
        ;; 5. Salto Quantico
        ;; ==========================================
        (forward-line 1)
        (setq insert-pos (point))

        ;; ==========================================
        ;; 6. Iniezione e Ripristino
        ;; ==========================================
        (insert text)
        (if is-region-active
            (progn
              (push-mark (+ insert-pos mk-offset) t t)
              (goto-char (+ insert-pos pt-offset))
              (setq deactivate-mark nil))
          (goto-char (+ insert-pos pt-offset)))))))

;; --- Binding per la Duplicazione (VSCode style: Alt+Shift+Giù) ---
(global-set-key (kbd "C-c d") 'zen/duplicate-dwim)
(global-set-key (kbd "M-S-<down>") 'zen/duplicate-dwim)

;; --- Binding per lo Spostamento (VSCode style: Alt+Su / Alt+Giù) ---
(global-set-key (kbd "M-<up>") 'zen/move-up-dwim)
(global-set-key (kbd "M-<down>") 'zen/move-down-dwim)

(provide 'zen-editing)
;;; zen-editing.el ends here
