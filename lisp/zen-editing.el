;;; zen-editing.el --- Le mie funzioni custom per il testo -*- lexical-binding: t; -*-

;;; Commentary:
;; Un modulo di editing chirurgico, ultra-veloce e senza dipendenze.
;; Combina il determinismo di Vim (Inner selections) con l'ergonomia
;; dei moderni editor (DWIM, Line moving, Surround istantaneo).

;;; Code:

;; ==========================================
;; SEZIONE 1: MANIPOLAZIONE RIGHE
;; ==========================================

(defun zen/duplicate-dwim ()
  "Duplica la regione attiva se presente, altrimenti duplica la riga corrente.
Non inquina il `kill-ring'."
  (interactive)
  (let (beg end col text)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end))
          (setq text (buffer-substring-no-properties beg end))
          (goto-char end)
          (deactivate-mark)
          (insert text))
      (progn
        (setq beg (line-beginning-position))
        (setq end (line-end-position))
        (setq col (current-column))
        (setq text (buffer-substring-no-properties beg end))
        (goto-char end)
        (newline)
        (insert text)
        (move-to-column col)))))

(defun zen/move-up-dwim ()
  "Sposta la riga o le righe selezionate verso l'alto con precisione matematica."
  (interactive)
  (let* ((is-region-active (use-region-p))
         beg end text insert-pos pt-offset mk-offset)
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
    (if (= beg (point-min))
        (message "Zen: Sei già alla prima riga!")
      (progn
        (setq pt-offset (- (point) beg))
        (when is-region-active
          (setq mk-offset (- (mark) beg)))
        (setq text (delete-and-extract-region beg end))
        (forward-line -1)
        (setq insert-pos (point))
        (insert text)
        (if is-region-active
            (progn
              (push-mark (+ insert-pos mk-offset) t t)
              (goto-char (+ insert-pos pt-offset))
              (setq deactivate-mark nil))
          (goto-char (+ insert-pos pt-offset)))))))

(defun zen/move-down-dwim ()
  "Sposta la riga o le righe selezionate verso il basso con precisione matematica."
  (interactive)
  (let* ((is-region-active (use-region-p))
         beg end text insert-pos pt-offset mk-offset)
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
    (if (= end (point-max))
        (message "Zen: Sei già all'ultima riga!")
      (progn
        (setq pt-offset (- (point) beg))
        (when is-region-active
          (setq mk-offset (- (mark) beg)))
        (setq text (delete-and-extract-region beg end))
        (forward-line 1)
        (setq insert-pos (point))
        (insert text)
        (if is-region-active
            (progn
              (push-mark (+ insert-pos mk-offset) t t)
              (goto-char (+ insert-pos pt-offset))
              (setq deactivate-mark nil))
          (goto-char (+ insert-pos pt-offset)))))))

(defun zen/open-line-below ()
  "Apre una nuova riga sotto quella corrente e la indenta (Vim: o)."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun zen/open-line-above ()
  "Apre una nuova riga sopra quella corrente e la indenta (Vim: O)."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun zen/join-line-below ()
  "Unisce la riga sottostante a quella corrente (Vim: J)."
  (interactive)
  (delete-indentation t))


;; ==========================================
;; SEZIONE 2: SELEZIONI CHIRURGICHE (I "Sostantivi")
;; ==========================================

(defun zen/select-inner-symbol ()
  "Seleziona il simbolo o la parola sotto il cursore (Vim: viw)."
  (interactive)
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                    (bounds-of-thing-at-point 'word))))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (point) t t)
          (goto-char (cdr bounds)))
      (message "Zen: Nessun simbolo sotto il cursore."))))

(defun zen/select-inner-quotes ()
  "Seleziona il contenuto testuale dentro le virgolette correnti (Vim: vi\")."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (start-quote (nth 8 ppss)))
    (if in-string
        (progn
          (goto-char start-quote)
          (forward-sexp)
          (let ((end-quote (point)))
            (goto-char (1+ start-quote))
            (push-mark (point) t t)
            (goto-char (1- end-quote))))
      (message "Zen: Non sei all'interno di una stringa."))))

(defun zen/select-inner-parens ()
  "Seleziona il contenuto all'interno del blocco di parentesi (Vim: vi()."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (start-paren (nth 1 ppss)))
    (if start-paren
        (progn
          (goto-char start-paren)
          (forward-sexp)
          (let ((end-paren (point)))
            (goto-char (1+ start-paren))
            (push-mark (point) t t)
            (goto-char (1- end-paren))))
      (message "Zen: Non sei all'interno di alcuna parentesi."))))


;; ==========================================
;; SEZIONE 3: MANIPOLAZIONI AVANZATE (I "Verbi")
;; ==========================================

(defun zen/change-dwim ()
  "Vaporizza la selezione attiva. Se nessuna selezione, vaporizza il simbolo.
Sostituisce il comportamento 'c' di Vim per le selezioni e 'ciw' per le parole."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                      (bounds-of-thing-at-point 'word))))
      (if bounds
          (delete-region (car bounds) (cdr bounds))
        (message "Zen: Nessun bersaglio da vaporizzare qui.")))))

(defun zen/surround-dwim ()
  "Avvolge la selezione attiva o il simbolo sotto il cursore.
Richiede UN SOLO tasto per indicare l'involucro (es. (, [, {, \", ')."
  (interactive)
  (let (beg end bounds)
    (if (use-region-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq bounds (or (bounds-of-thing-at-point 'symbol)
                       (bounds-of-thing-at-point 'word)))
      (if bounds
          (setq beg (car bounds)
                end (cdr bounds))
        (error "Zen: Nessun testo da avvolgere qui")))

    (let* ((char (read-char "Avvolgi con: "))
           (char-str (char-to-string char))
           (pairs '((?\( . ?\))
                    (?\[ . ?\])
                    (?\{ . ?\})
                    (?<  . ?>)))
           (pair (assoc char pairs))
           (open-char char-str)
           (close-char (if pair (char-to-string (cdr pair)) char-str)))
      (save-excursion
        ;; Inserisci prima alla fine per non sfalsare le coordinate
        (goto-char end)
        (insert close-char)
        (goto-char beg)
        (insert open-char))
      (deactivate-mark)
      (message "Zen: Avvolto con %s e %s" open-char close-char))))


;; ==========================================
;; SEZIONE 4: LA TASTIERA (I Keybindings)
;; ==========================================

;; --- Righe ---
(global-set-key (kbd "C-c d") 'zen/duplicate-dwim)
(global-set-key (kbd "M-S-<down>") 'zen/duplicate-dwim)

(global-set-key (kbd "M-<up>") 'zen/move-up-dwim)
(global-set-key (kbd "M-<down>") 'zen/move-down-dwim)

(global-set-key (kbd "<C-return>") 'zen/open-line-below)
(global-set-key (kbd "<C-S-return>") 'zen/open-line-above)

(global-set-key (kbd "M-J") 'zen/join-line-below)

;; --- Azioni (Verbi) ---
(global-set-key (kbd "M-k") 'zen/change-dwim)
(global-set-key (kbd "M-'") 'zen/surround-dwim)

;; --- Selezioni (Sostantivi) ---
(define-prefix-command 'zen-inner-map)
(global-set-key (kbd "M-i") 'zen-inner-map)

(define-key zen-inner-map (kbd "s") 'zen/select-inner-symbol)  ; Parola
(define-key zen-inner-map (kbd "q") 'zen/select-inner-quotes)  ; Virgol. (q)
(define-key zen-inner-map (kbd "p") 'zen/select-inner-parens)  ; Paren. (p)

(provide 'zen-editing)
;;; zen-editing.el ends here
