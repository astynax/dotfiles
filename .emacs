;; -*- lexical-binding: t-*-
;;; Initialization
;; increase GC-limit up to 100M for boot speedup
(setq gc-cons-threshold 100000000)

;; just a shortcut :)
(defun my/configure ()
  "Opens user-init-file"
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "M-<f12>") 'my/configure)

(setq debug-on-error t)

;;; Package menagement
(require 'package)
(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :ensure nil

  :custom
  (use-package-always-ensure t)

  :config
  (put 'use-package 'lisp-indent-function 1))

(use-package diminish)
(use-package bind-key)

;;; Customization
(use-package custom
  :ensure nil
  :custom
  (custom-safe-themes t "Trust all custom themes..."))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file null-device "Don't store customizations"))

;;; Emacs itself
(use-package emacs
  :ensure nil

  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)

  :custom
  (inhibit-startup-screen t "No startup screen")
  (initial-scratch-message "")
  (indicate-empty-lines t)
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (x-select-enable-clipboard t "Use clipboard")
  (x-select-enable-primary t "Use primary buffer")
  (save-interprogram-paste-before-kill t)
  (apropos-do-all t)
  (mouse-yank-at-point t "Yank at point using mouse")
  (resize-mini-windows t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  ;; Window
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode nil)
  (frame-title-format "Emacs: %b")
  ;; Cursor
  (line-number-mode t)
  (column-number-mode t)
  (blink-cursor-mode nil)
  (x-stretch-cursor t)
  (mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
  (shift-select-mode nil "No selection with <shift>")
  ;; Exit confirmation
  (kill-emacs-query-functions
   (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
         kill-emacs-query-functions))

  :config
  (global-prettify-symbols-mode)
  (prefer-coding-system 'utf-8))

(use-package frame
  :ensure nil
  :bind
  ("C-z" . nil))

(use-package faces
  :ensure nil
  :defer t
  :config
  (defun my/set-font (font)
    (set-face-attribute 'default nil :font font)
    (setq after-make-frame-functions
          (lambda (&optional ARGS)
            (set-face-attribute 'default nil :font font))))
  (my/set-font
   (font-spec
    :family "Anonymous Pro"
    :width 'normal
    :size 20)))

;;; Date/Time
(use-package time
  :ensure nil

  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (calendar-week-start-day 1)
  (calendar-date-style 'european))

;;; Files
(use-package files
  :ensure nil

  :preface
  (setq
   my/backup-directory-per-session
   (format "%sbackups/per-session" user-emacs-directory)

   my/backup-directory-per-save
   (format "%sbackups/per-save" user-emacs-directory))

  (defun my/force-backup-of-buffer ()
    (when (not buffer-backed-up)
      (let ((backup-directory-alist
             `((".*" . ,my/backup-directory-per-session)))
            (kept-new-versions 3))
        (backup-buffer)))
    (let ((buffer-backed-up nil))
      (backup-buffer)))

  :hook
  (before-save . my/force-backup-of-buffer)

  :custom
  (require-final-newline t)
  ;; backup settings
  (backup-directory-alist `((".*" . ,my/backup-directory-per-save)))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (version-control t)
  (vc-make-backup-files t)
  ;; autosave
  (auto-save-default nil)
  ;; no TABs in source
  (indent-tabs-mode nil)

  :config
  (put 'create-lockfiles 'safe-local-variable #'booleanp))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package backup-walker
  :commands (backup-walker-start))

;;; Dired
(use-package dired
  :ensure nil

  :custom
  (dired-omit-files "^\\..*$")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-al --group-directories-first")

  :hook
  (dired . dired-omit-mode)

  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil)

(use-package dired-single)

(use-package dired-details+
  :custom
  (dired-details-initially-hide nil)
  (dired-details-propagate-flag nil))

(use-package dired-subtree
  :bind
  (:map
   dired-mode-map
   ("]" . dired-subtree-insert)
   ("[" . dired-subtree-remove)
   ("}" . dired-subtree-only-this-file)
   ("{" . dired-subtree-only-this-directory)
   ("M-p" . dired-subtree-up)
   ("M-n" . dired-subtree-down)
   ("<tab>" . dired-subtree-cycle)))
;;; UI
;;;; Highlights
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package volatile-highlights
  :diminish

  :config
  (volatile-highlights-mode 1))

;;;; Hydra
(use-package hydra)

;;;; Uniquify
(use-package uniquify
  :ensure nil

  :custom
  (uniquify-buffer-name-style 'forward))

;;;; Which key
(use-package which-key
  :diminish

  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)

  :config
  (which-key-mode))

;;;; Global text scale
;; source: https://www.emacswiki.org/emacs/GlobalTextScaleMode
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(defun global-text-scale/zoom-in ()
  (interactive)
  (global-text-scale-adjust 1))

(defun global-text-scale/zoom-out ()
  (interactive)
  (global-text-scale-adjust -1))

(defun global-text-scale/reset ()
  (interactive)
  (global-text-scale-adjust (- text-scale-mode-amount))
  (global-text-scale-mode -1))

(defhydra hydra-global-text-scale (mode-specific-map "z")
  "Zoom"
  ("=" global-text-scale/zoom-in "Zoom in")
  ("-" global-text-scale/zoom-out "Zoom out")
  ("0" global-text-scale/reset "Reset zoom"))

;;;; Theme
(use-package solarized-theme
  :custom
  (solarized-distinct-fringe-background t "Make the fringe to look distinct")

  :config
  (load-theme 'solarized-light))

;;;; Window switching
(use-package ace-window
  :bind
  ("M-o" . ace-window))

;;;; Fullframe
(use-package fullframe
  :config
  (fullframe list-packages quit-window)
  (fullframe package-list-packages quit-window)
  (fullframe magit-log-all quit-window)
  (fullframe magit-log-current quit-window))

;;;; Popup windows manupulation
(use-package popwin
  :config
  (popwin-mode))

;;;; Beacon
;; (visualizes cursor position)
(use-package beacon
  :diminish

  :bind
  ("<f11>" . beacon-blink)

  :custom
  (beacon-color "#A06600")

  :config
  (beacon-mode))

;;;; iBuffer
(use-package ibuffer
  :ensure nil
  :bind
  (:map
   mode-specific-map
   ("b" . ibuffer)))

;; h/l some actions like a killing, yanking, pasting e.t.c.

;;;; Ivy
(use-package ivy
  :demand

  :diminish ivy-mode

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist
   '((swiper           . ivy--regex-plus)
     (counsel-git-grep . ivy--regex-plus)
     (counsel-grep     . ivy--regex-plus)
     (counsel-ag       . ivy--regex-plus)
     (counsel-rg       . ivy--regex-plus)
     (t                . ivy--regex-fuzzy)))

  :bind
  (:map
   ivy-minibuffer-map
   ("TAB" . ivy-partial))

  (:map
   mode-specific-map
   (:prefix
    "i"
    :prefix-map my/ivy-map
    ("r" . ivy-resume)))

  :config
  (ivy-mode 1))

(use-package counsel
  :demand

  :diminish

  :config
  (counsel-mode 1))

(use-package swiper
  :bind
  ("M-s s" . swiper)

  (:map
   isearch-mode-map
   ("M-s s" . swiper-from-isearch)))

(use-package ivy-rich
  :custom
  (ivy-rich-path-style 'abbrev)

  :config
  (ivy-rich-mode 1))

(use-package smex)
(use-package flx)
(use-package ivy-hydra)

(use-package ivy-xref
  :custom
  (xref-show-xrefs-function 'ivy-xref-show-xrefs))

;;;; TODO/FIXME/etc keyword highlight
(use-package hl-todo
  :demand

  :bind
  ("M-s t" . hl-todo-occur)

  :custom-face
  (hl-todo ((t (:bold t :background "#073642"))))

  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#6c71c4")
     ("FIXME" . "#dc322f")
     ("NOTE" . "#2aa198")))

  :config
  (global-hl-todo-mode))

;;; Behaviour
;;;; reverse-im
(use-package reverse-im
  :diminish

  :custom
  (reverse-im-input-methods '("russian-computer"))
  (default-input-method "russian-computer")

  :config
  (reverse-im-activate "russian-computer"))

;;;; Scratch
(use-package unkillable-scratch
  :preface
  (defun my/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  :bind
  ("M-<f11>" . my/switch-to-scratch)

  :config
  (unkillable-scratch))

;;;; Client/server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;; Editing
;;;; Subwords
(use-package subword
  :ensure nil
  :diminish subword-mode)

;;;; Electric
(use-package electric
  :hook
  (prog-mode . electric-pair-local-mode))

;;;; Whole line or region
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode

  :config
  (whole-line-or-region-mode))

;;;; Indirect region editing
(use-package edit-indirect
  :ensure t

  :bind
  (:map
   mode-specific-map
   ("'" . edit-indirect-region)))

;;;; Whitespaces
(use-package whitespace
  :ensure nil

  :diminish

  :preface
  (defun my/whitespace-mode ()
    "whitespace mode for prog buffers"
    (setq-local require-final-newline t)
    (setq-local next-line-add-newlines nil)
    (whitespace-mode t)
    (toggle-truncate-lines t)
    ;; trim triling spaces on save
    (add-hook 'before-save-hook 'delete-trailing-whitespace t t))

  :hook
  (prog-mode . my/whitespace-mode)

  :custom-face
  (whitespace-line ((t (:background "moccasin" :underline (:color foreground-color :style wave)))))
  (whitespace-tab ((t (:foreground "brown" :inverse-video nil :underline t))))

  :custom
  (whitespace-style '(face lines-tail trailing tab-mark))
  (whitespace-line-column 80)
  (default-tab-width 4))

(use-package shrink-whitespace
  :bind
  ("M-\\" . shrink-whitespace))

;;;; Smart commenting
(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2))

;;;; Indentation
(use-package aggressive-indent
  :defer t)

;;;; Expand Region
(use-package expand-region
  :bind
  ("M-]" . er/expand-region)
  ("M-[" . er/contract-region))

;;;; Case formatting
(use-package caseformat
  :bind
  ("M-L" . caseformat-backward))

;;;; Undo tree
(use-package undo-tree
  :diminish

  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(format "%sundo-tree-history" user-emacs-directory))))

  :config
  (global-undo-tree-mode))

;;;; SmartParens
(use-package smartparens
  :diminish (smartparens-mode . "🄢")

  :preface
  (defun my/no-electric-with-startparens ()
    "Disables electric parens with smartparens enabled"
    (electric-pair-local-mode -1))

  :hook
  (smartparens-mode . my/no-electric-with-startparens)
  (emacs-lisp-mode . smartparens-strict-mode)
  (ielm-mode . smartparens-strict-mode)
  (lisp-mode . smartparens-strict-mode)
  (lisp-interaction-mode . smartparens-strict-mode)
  (scheme-mode . smartparens-strict-mode)
  (clojure-mode . smartparens-strict-mode)
  (cider-repl-mode . smartparens-strict-mode)
  (cider-mode . smartparens-strict-mode)
  (eval-expression-minibuffer-setup . smartparens-strict-mode)

  :bind
  (:map
   sp-keymap
   ;; split/join/unwrap
   ("C-c j" . sp-join-sexp)
   ("C-c J" . sp-split-sexp)
   ("C-c M-j" . sp-splice-sexp)
   ;; barf/slurp
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ;; closing brackets of any kinds
   (")" . sp-up-sexp)
   ("}" . sp-up-sexp)
   ("]" . sp-up-sexp))

  :config
  (require 'smartparens-config)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

;;;; Multiple Cursors
(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/mark-next-word-like-this)
  ("C-M-<" . mc/mark-previous-word-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)

  (:prefix
   "C-c m"
   :prefix-map my/mc-map
   ("+" . mc/mark-all-like-this)
   ("r" . set-rectangular-region-anchor)
   ("c" . mc/edit-lines)
   ("e" . mc/edit-ends-of-lines)
   ("a" . mc/edit-beginnings-of-lines)
   ("SPC" . ace-mc-add-multiple-cursors)))

(use-package ace-mc
  :after (multiple-cursors))

;;;; Spell Checking
(when (executable-find "hunspell")
  (progn
    (setq ispell-program-name "hunspell")
    (eval-after-load "ispell"
      '(progn (defun ispell-get-coding-system () 'utf-8)))))

;;;; Vim'ish folding
(use-package vimish-fold
  :bind
  (:prefix
   "C-c f"
   :prefix-map my/vimish-fold-map
   ("l" . vimish-fold-avy)
   ("f" . vimish-fold)
   ("F" . vimish-fold-refold)
   ("u" . vimish-fold-unfold)
   ("U" . vimish-fold-unfold-all)
   ("d" . vimish-fold-delete)
   ("SPC" . vimish-fold-toggle))

  :custom-face
  (vimish-fold-mouse-face ((t (:box (:line-width 1 :color "yellow")))))
  (vimish-fold-overlay ((t (:box (:line-width 1 :color "dim gray")))))

  :config
  (setq vimish-fold-blank-fold-header "<...>"
        vimish-fold-indication-mode 'left-fringe))

;;;; Smart BOL
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line."
  (interactive "^p")
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;;;; ElDoc
(use-package eldoc
  :ensure nil
  :diminish)

;;; Navigation
;;;; Avy
(use-package avy
  :init
  (define-prefix-command 'my/avy-map 'my/avy-map "Avy")

  :bind-keymap
  ("M-g SPC" . my/avy-map)

  :bind
  ("M-SPC" . avy-goto-char)

  (:map
   isearch-mode-map
   ("M-SPC" . avy-isearch))

  (:map
   search-map
   ("M-SPC" . avy-isearch))

  (:map
   my/avy-map
   ("w" . avy-goto-word-or-subword-1)
   ("l" . avy-goto-line)))

(use-package avy-zap
  :after (avy)

  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim)

  (:map
   my/avy-map
   ("z" . avy-zap-to-char)
   ("Z" . avy-zap-up-to-char)))

(use-package link-hint
  :bind
  ("M-g l" . link-hint-open-link))

;;;; Bookmarks
(put 'bookmark-default-file 'safe-local-variable #'stringp)
(put 'bookmark-save-flag 'safe-local-variable #'numberp)

;;;; Misc
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually moves"
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; C-u C-SPC C-SPC instead  C-u C-SPC C-u C-SPC
(setq set-mark-command-repeat-pop t)

;; xref tweaks
(defun my/do-then-quit (&rest args)
  (let ((win (selected-window)))
    (apply (car args) (rest args))
    (quit-window nil win)))

(advice-add #'xref-goto-xref :around #'my/do-then-quit)

;;; Languages
;;;; ELisp
(use-package elisp-mode
  :ensure nil

  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;;;; Racket (Geiser)
(use-package geiser)

;;;; Clojure
(use-package clojure-mode
  :hook
  (clojure-mode . aggressive-indent-mode)
  (clojure-mode . rainbow-delimiters-mode)

  :config
  ;; some unicode
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))
                   ("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "ƒ")
                              nil)))
                   ("\\(#\\){"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "∈")
                              nil)))))

  (define-clojure-indent
    ;; compojure indentation tweaks
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package clojure-mode-extra-font-locking
  :after (clojure-mode))

(use-package cider
  :after (clojure-mode)

  :hook
  (cider-repl-mode . rainbow-delimiters-mode)
  (cider-mode . rainbow-delimiters-mode)

  :custom
  (cider-repl-result-prefix ";; => "))

;;;; Haskell
(use-package haskell-mode
  :diminish haskell-mode

  :init
  (add-to-list 'magic-mode-alist '(".* stack" . haskell-mode))

  :mode
  ("\\.l?hs\\'" . haskell-mode)
  ("\\.cabal\\'" . haskell-cabal-mode)
  ("\\.hamlet\\'" . shakespeare-hamlet-mode)
  ("\\.julius\\'" . shakespeare-julius-mode)
  ("routes\\'" . haskell-yesod-parse-routes-mode)

  :bind
  (:map
   haskell-mode-map
   :prefix "C-c SPC"
   :prefix-map my/haskell-map
   ("v" . haskell-cabal-visit-file)
   ("m" . haskell-auto-insert-module-template)
   ("I" . haskell-sort-imports)
   ("A" . haskell-align-imports)
   ("S" . haskell-mode-stylish-buffer)
   ("y" . haskell-hayoo)
   ("SPC" . haskell-hide-toggle))

  (:map
   haskell-mode-map
   ("C-c s" . my/snakify-region)
   ("<f9>" . my/haskell-jump-to-loc))

  :hook
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . subword-mode)
  (haskell-mode . hi2-mode)
  (haskell-mode . eldoc-mode)
  (haskell-mode . flycheck-mode)
  (haskell-mode . my/boot-haskell)
  (haskell-yesod-parse-routes-mode
   . my/haskell-yesod-parse-routes-mode-hook)

  :config
  (defun my/haskell-yesod-parse-routes-mode-hook ()
    "Disables the line wrapping and auto-fill-mode"
    (toggle-truncate-lines t))

  (defun my/haskell-jump-to-loc ()
    "Opens the location of error from primary buffer"
    (interactive)
    (-let (((path line col ...)
            (split-string (gui-get-primary-selection) ":")))
      (find-file-existing path)
      (goto-line (string-to-number line))
      (move-to-column (max 0 (- (string-to-number col) 1)))))

  (defun my/hack-locals-for-haskell ()
    (when my/use-intero
      (intero-mode)))

  (defun my/boot-haskell ()
    "Initialize haskell stuff"
    (interactive)
    (setq tags-case-fold-search nil)
    (add-hook 'hack-local-variables-hook
              #'my/hack-locals-for-haskell
              nil t))

  ;; snakify
  (defun my/snakify-region (&optional b e)
    (interactive "r")
    (call-process-region
     b e "snakify" t (current-buffer) t))

  ;; hemmet
  (defun my/hemmet-expand-region (&optional b e)
    (interactive "r")
    (shell-command-on-region
     b e "hemmet bem react-flux" (current-buffer) t "*hemmet error*" t))

  ;; yesod handlers scaffolding
  (defun my/haskell-scaffold-yesod-handlers ()
    "Kills a current line (that containins an Yesod route) and
     scaffolds yesod handler for each of methods"
    (interactive)
    (let* ((p1 (line-beginning-position))
           (p2 (line-end-position))
           (lval (buffer-substring-no-properties p1 p2))
           (tokens (cdr (split-string lval))))
      (if (> 2 (length tokens))
          (message "%s" "Not enough tokens")
        (let ((rname (car tokens))
              (methods (cdr tokens)))
          (progn
            (kill-whole-line)
            (previous-line)
            (loop
             for m in methods do
             (let* ((name (concat (downcase m) rname))
                    (l1 (concat name " :: Handler TypedContent"))
                    (l2 (concat name " = error \"" name " not implemented\"")))
               (end-of-line)
               (newline)
               (insert l1) (newline)
               (insert l2) (newline)))))))))

(use-package hi2
  :ensure t

  :after (haskell-mode)

  :diminish hi2-mode

  :bind
  (:map
   hi2-mode-map
   ("<tab>" . hi2-indent-line))

  :config
  (setq hi2-layout-offset 2
        hi2-left-offset 2
        hi2-where-post-offset 2)

  (put 'hi2-where-post-offset 'safe-local-variable #'numberp)
  (put 'hi2-left-offset 'safe-local-variable #'numberp)
  (put 'hi2-layout-offset 'safe-local-variable #'numberp))

(use-package intero
  :ensure t

  :after (haskell-mode)

  :diminish (intero-mode . "ⓘ")

  :bind
  (:map
   my/haskell-map
   ("i r" . intero-restart)
   ("i t" . intero-targets))

  :config
  (unbind-key "M-." intero-mode-map)
  (unbind-key "C-c <tab>" intero-mode-map)

  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))

(defvar my/use-intero nil "'t' = use 'intero-mode'")
(put 'my/use-intero 'safe-local-variable #'booleanp)
(put 'intero-targets 'safe-local-variable #'listp)

(use-package hindent
  :ensure t

  :after (haskell-mode)

  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)

  :bind
  (:map
   my/haskell-map
   ("f b" . hindent-reformat-buffer)
   ("f r" . hindent-reformat-region)
   ("f d" . hindent-reformat-decl))

  :config
  (setq-default hindent-reformat-buffer-on-save nil))

(put 'hindent-reformat-buffer-on-save 'safe-local-variable #'booleanp)

(use-package company-cabal
  :ensure t

  :after (haskell-mode)

  :config
  (add-to-list 'company-backends 'company-cabal))

(use-package shakespeare-mode
  :ensure t

  :after (haskell-mode))

(put 'flycheck-ghc-language-extensions   'safe-local-variable #'listp)
(put 'flycheck-hlint-language-extensions 'safe-local-variable #'listp)
(put 'haskell-stylish-on-save            'safe-local-variable #'booleanp)
(put 'haskell-hayoo-url                  'safe-local-variable #'stringp)

;;;; Python
(use-package python
  :ensure t

  :mode ("\\.py\\'" . python-mode)

  :hook
  (python-mode . my/python-mode-hook)

  :config
  (defun my/python-enforce-indentation ()
    "Enforces python indentation to 4 spaces"
    (interactive)
    (auto-indent-mode -1)
    (setq indent-tabs-mode nil
          python-indent-offset 4
          python-indent-guess-indent-offset nil))

  (defun my/python-mode-hook ()
    "Initialize python stuff"
    (interactive)
    (my/python-enforce-indentation)
    (flycheck-mode)
    (flycheck-select-checker 'python-pylint)))

(use-package elpy
  :ensure t

  :after (python)

  :diminish elpy-mode

  :bind
  (:map
   python-mode-map
   ("M-g j" . elpy-menu))

  :config
  (setq
   elpy-modules
   '(elpy-module-company
     elpy-module-eldoc
     elpy-module-pyvenv
     elpy-module-yasnippet
     elpy-module-sane-defaults))

  (elpy-enable))

;;;; Rust
(use-package rust-mode
  :ensure t

  :mode "\\.rs\\'"

  :commands (rust-mode)

  :config
  (add-to-list 'company-dabbrev-code-modes 'rust-mode)
  (add-to-list 'company-keywords-alist (cons 'rust-mode rust-mode-keywords)))

(use-package flycheck-rust
  :ensure t

  :after (rust-mode)

  :hook
  (rust-mode . my/rust-mode-hook)

  :config
  (defun my/rust-mode-hook ()
    (flycheck-rust-setup)
    (flycheck-mode)))

;;;; Markdown
(use-package markdown-mode
  :ensure t

  :mode "\\.md\\'")

;;;; Elm
(use-package elm-mode
  :ensure t

  :mode "\\.elm\\'"

  :diminish elm-indent-mode

  :bind
  (:map
   elm-mode-map
   ("TAB" . elm-indent-cycle))

  :hook
  (elm-mode . my/elm-mode-hook)

  :config
  (defun my/elm-mode-hook ()
    (elm--find-dependency-file-path)
    (flycheck-mode)
    (elm-indent-mode -1))

  (when (executable-find "elm-oracle")
    (add-hook 'elm-mode-hook
              #'elm-oracle-setup-completion))

  (setq elm-indent-look-past-empty-line nil))

(put 'elm-format-on-save 'safe-local-variable #'booleanp)
(put 'elm-compile-arguments 'safe-local-variable #'listp)

(use-package flycheck-elm
  :ensure t

  :after (elm-mode)

  :hook
  (flycheck-mode . flycheck-elm-setup))

;;;; Go
(use-package go-mode
  :if (executable-find "go")

  :ensure t

  :mode "\\.go\\'"

  :commands (go-mode)

  :hook
  (go-mode . my/go-mode-hook)

  :config
  (defun my/go-mode-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save)
    (setq-local whitespace-style '(face lines-tail trailing))
    (flycheck-mode)))

;;;; PureScript
(use-package purescript-mode
  :if (executable-find "purs")

  :ensure t

  :mode "\\.purs\\'")

(use-package psc-ide
  :ensure t

  :after (purescript-mode)

  :diminish psc-ide-mode

  :hook
  (purescript-mode . my/purescript-mode-hook)

  :config
  (defun my/purescript-mode-hook ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

;;;; Web
(use-package web-mode
  :ensure t

  :commands (web-mode)

  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

  :config
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-script-padding 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 2))

;;;; PlantUML
;; (use-package puml-mode
;;   :config
;;   (add-to-list 'auto-mode-alist
;;                '("\\.puml\\'" . puml-mode)
;;                '("\\.plantuml\\'" . puml-mode)))

;;;; YAML
(use-package yaml-mode
  :ensure t

  :mode "\\.yaml\\'")

;;;; TOML
(use-package toml-mode
  :ensure t

  :mode "\\.toml\\'")

;;;; CSV
(use-package csv-mode
  :ensure t

  :mode "\\.[Cc][Ss][Vv]\\'")

;;;; Nix
(use-package nix-mode
  :ensure t

  :mode "\\.nix\\'")

;;;; SQL
(setq-default sql-dialect 'sql-postgres)

;;; IDE
;;;; Autocompletion
(use-package company
  :ensure t

  :diminish

  :bind
  ("C-c /" . company-files)

  (:map
   company-mode-map
   ("M-<tab>" . company-complete))

  (:map
   company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))

  :custom
  (company-tooltip-limit 20)
  (company-begin-commands '(self-insert-command))
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)

  :config
  (put 'company-backends 'safe-local-variable #'listp)
  (global-company-mode t))

(use-package hippie
  :ensure nil

  :bind
  ([remap dabbrev-expand] . hippie-expand)

  :config
  (ert--remove-from-list
   'hippie-expand-try-functions-list
   'try-expand-line))

(use-package company-try-hard
  :ensure t

  :after (company)

  :bind
  ("C-c M-/" . company-try-hard)

  (:map
   company-active-map
   ("C-c M-/" . company-try-hard)))

(use-package company-flx
  :ensure t

  :after (company)

  :hook
  (company-mode . company-flx-mode))

(use-package company-quickhelp
  :ensure t

  :after (company)

  :diminish company-quickhelp-mode

  :bind
  (:map
   company-active-map
   ("C-h" . company-quickhelp-manual-begin))

  :config
  (company-quickhelp-mode t))

;;;; Flycheck
(use-package flycheck
  :ensure t

  :diminish "Ⓕ"

  :preface
  (put 'flycheck-checker 'safe-local-variable #'symbolp)
  (add-to-list 'safe-local-variable-values
               '(flycheck-check-syntax-automatically 'nil))

  :custom
  (flycheck-check-syntax-automatically
   '(save mode-enabled) "Only check on save")

  :bind
  ("<f5>" . flycheck-buffer)
  ("<f7>" . flycheck-previous-error)
  ("<f8>" . flycheck-next-error))

(use-package flycheck-color-mode-line
  :ensure t

  :after (flycheck)

  :hook
  (flycheck-mode . flycheck-color-mode-line-mode))

;;;; Yasnippet
(bind-keys
 :prefix "C-c y"
 :prefix-map my/yas-map)

(use-package yasnippet
  :ensure t

  :diminish (yas-minor-mode . "Ⓨ")

  :bind
  (:map
   my/yas-map
   ("<tab>" . company-yasnippet))

  (:map
   yas-minor-mode-map
   ("C-c <tab>" . yas-expand))

  :hook
  (prog-mode . yas-minor-mode)
  (html-mode . yas-minor-mode)

  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (unbind-key "TAB" yas-minor-mode-map)

  (yas-reload-all)
  (when (file-exists-p "~/.emacs.d/snippets")
    (add-to-list 'yas/snippet-dirs "~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t

  :after (yasnippet))

;;;; Grep'likes
;; sudo apt-get install silversearcher-ag
(use-package ag
  :if (executable-find "ag")
  :custom
  (ag-highlight-search t))

;; install from github
(use-package ripgrep
  :if (executable-find "rg"))

;;;; Projectile
(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-mode-line-function
   (lambda () (format "[%s]" (projectile-project-name))))

  :config
  (put 'projectile-tags-file-name 'safe-local-variable #'stringp)
  (put 'projectile-globally-ignored-files 'safe-local-variable #'listp)
  (put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
  (projectile-mode))

(use-package counsel-projectile
  :after (projectile counsel ivy)

  :custom
  (projectile-completion-system 'ivy)

  :config
  (counsel-projectile-mode))

(use-package projectile-ripgrep
  :if (package-installed-p 'ripgrep)

  :after (projectile)

  :bind
  (:map
   projectile-command-map
   ("s r" . projectile-ripgrep)))

;;;; Git/Magit
(use-package magit
  :ensure t

  :custom
  (magit-log-arguments '("--graph" "--color" "--decorate"))

  :bind
  ("C-x g" . magit-status))

(use-package git-timemachine
  :ensure t

  :commands (git-timemachine))

;;;; YankPad
(use-package yankpad
  :custom
  (yankpad-file "~/Dropbox/org/yankpad.org")

  :bind
  (:map
   my/yas-map
   ("m" . yankpad-map)
   ("y" . yankpad-insert)))

;;;; Terminal here
(use-package terminal-here
  :bind
  (:prefix
   "C-c t"
   :prefix-map my/terminal-here-map
   ("t" . terminal-here-launch)
   ("p" . terminal-here-project-launch)))

;;;; RESTclient
(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t

  :after (restclient)

  :hook
  (restclient-mode . my/restclient-mode-hook)

  :config
  (defun my/restclient-mode-hook ()
    (add-to-list 'company-backends
                 'company-restclient)))

;;; Org-mode/Outline
;;;; Org
(use-package org
  :ensure org
  :pin org

  :mode ("\\.org\\'" . org-mode)

  :commands (org-mode org-capture)

  :bind
  ("C-c c" . org-capture)
  ("<f12>" . my/org-open-notes-file)

  (:map
   org-mode-map
   ("C-c M-RET" . org-insert-heading-after-current))

  :hook
  (org-mode . yas-minor-mode)

  :config
  (setq
   org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE"))
   org-enforce-todo-dependencies t
   org-hide-leading-stars t
   ;; org-completion-use-ido t ;; use ido for completion
   org-outline-path-complete-in-steps nil
   org-html-postamble nil
   org-edit-src-content-indentation 0
   org-ellipsis "…"
   org-src-window-setup (quote current-window)
   org-src-preserve-indentation t)

  (require 'ob-shell)
  (require 'ob-python)

  (defvar my/org-babel-langs
    '((shell . t)
      (emacs-lisp . t)
      (python . t)))

  (defun my/org-babel-load-langs ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     my/org-babel-langs))

  (defun my/org-open-notes-file ()
    (interactive)
    (find-file org-default-notes-file))

  (setq
   org-default-notes-file "~/Dropbox/org/buffer.org"
   org-capture-templates
   '(("n" "Buffer note" entry
      (file+headline "" "Notes")
      "* %U\n%?"
      )

     ("c" "Buffer clip" entry
      (file+headline "" "Clips")
      "* %U\n%x"
      :immediate-finish
      :kill-buffer
      )

     ("b" "Bookmark a Haskell module" entry
      (file+function
       ""
       (lambda ()
         ;; TODO: propose "file+headline-function"
         (unless (derived-mode-p 'org-mode)
           (error
            "Target buffer \"%s\" for should be in Org mode"
            (current-buffer)))
         (let* ((file (buffer-file-name (org-capture-get :original-buffer)))
                (rev (vc-working-revision file))
                (branch (or (vc-git--symbolic-ref file)
                            (substring rev 0 7))))
           (goto-char (point-min))
           (if (re-search-forward
                (format org-complex-heading-regexp-format (regexp-quote branch))
                nil t)
               (goto-char (point-at-bol))
             (goto-char (point-max))
             (or (bolp) (insert "\n"))
             (insert "* " branch "\n")
             (insert ":PROPERTIES:\n")
             (insert ":VISIBILITY: children\n")
             (insert ":END:\n")
             (beginning-of-line 0))
           )))
      "* [[file:/%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%(haskell-guess-module-name-from-file-name (buffer-file-name (org-capture-get :original-buffer)))]]"
      :immediate-finish
      :kill-buffer)))

  :custom-face
  (org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.0))))
  (org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 1.0))))
  (org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.0))))
  (org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :height 1.0))))
  (org-tag ((t (:weight normal :height 0.8)))))

(put 'org-default-notes-file           'safe-local-variable #'stringp)

(use-package htmlize
  :ensure t

  :after (org))

(use-package ox-pandoc
  :if (executable-find "pandoc")

  :ensure t

  :after (org))

(use-package org-cliplink
  :ensure t

  :bind
  (:map
   org-mode-map
   ("C-c l" . org-cliplink)))

(use-package org-brain
  :ensure t

  :after (org)

  :commands (org-brain-visualize)

  :config
  (setq org-brain-path "~/Dropbox/org/brain"))

(use-package ob-restclient
  :ensure t

  :after '(org restclient)

  :commands (org-babel-execute:restclient)

  :config
  (add-to-list 'my/org-babel-langs '(restclient . t))
  (my/org-babel-load-langs))

;;;; Outshine
(use-package outshine
  :ensure t

  :diminish outline-minor-mode

  ;; :bind
  ;; (:map
  ;;  outline-minor-mode-map
  ;;  ("M-i" . outline-cycle))

  :hook
  (outline-minor-mode . outshine-hook-function)
  (prog-mode . outline-minor-mode)

  :custom
  (outshine-preserve-delimiter-whitespace t)
  (outshine-cycle-emulate-tab t))

;;; Other modes
;;;; Fireplace
(use-package fireplace
  :commands (fireplace))

;;; Finalization
;; restore GC-limit after timeout
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)))
