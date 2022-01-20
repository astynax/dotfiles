;; -*- lexical-binding: t -*-
;;; Initialization
;; increase GC-limit up to 100M for boot speedup
(setq gc-cons-threshold 100000000)

;; just a shortcut :)
(defun my/configure ()
  "Opens user-init-file"
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "M-<f12>") 'my/configure)

;;; Package management
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq tls-checktrust "ask")
(setq tls-program
      '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"))
(package-initialize)

(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq-default package-check-signature 'allow-unsigned)
  (setq-default package-unsigned-archives '("gnu"))
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (kill-emacs))

(require 'gnu-elpa-keyring-update)

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
  (prefer-coding-system 'utf-8)
  (put 'overwrite-mode 'disabled t))

(use-package frame
  :ensure nil
  :bind
  ("C-z" . nil))

;;; Faces
(use-package faces
  :ensure nil

  :preface
  (setq
   my/faces/size 19

   ;; TODO: make the font selection more robust
   my/faces/fixed-family "JetBrains Mono"

   my/faces/variable-family
   (if (string-equal system-type "darwin")
       "PT Serif"
     "DejaVu Serif"))

  :diminish (buffer-face-mode "")

  :config
  (set-face-attribute
   'variable-pitch nil
   :font
   (font-spec
    :family my/faces/variable-family
    :size my/faces/size))

  (set-face-attribute
   'fixed-pitch nil
   :font
   (font-spec
    :family my/faces/fixed-family
    :size my/faces/size))

  (set-face-attribute
   'default nil
   :font
   (font-spec
    :family my/faces/fixed-family
    :size my/faces/size))

  (set-face-attribute
   'mode-line nil
   :height 0.8)

  (set-face-attribute
   'mode-line-inactive nil
   :height 0.8))

;;; Date/Time
(use-package time
  :ensure nil

  :preface
  (defun insert-today ()
    (interactive)
    (insert (format-time-string "%Y-%m-%d" (current-time))))

  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (calendar-week-start-day 1)
  (calendar-date-style 'european))

;;; Files
(use-package files
  :ensure nil

  :preface
  (setq-default
   my/backup-directory-per-session
   (format "%sbackups/per-session" user-emacs-directory)

   my/backup-directory-per-save
   (format "%sbackups/per-save" user-emacs-directory))

  (defun my/backup-buffer ()
    (when (not buffer-backed-up)
      (let ((backup-directory-alist
             `((".*" . ,my/backup-directory-per-session)))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; back up unconditionaly
    (let ((buffer-backed-up nil))
      (backup-buffer)))

  :hook
  (before-save . my/backup-buffer)

  :custom
  ;; backup settings
  (backup-directory-alist `((".*" . ,my/backup-directory-per-save)))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (version-control t)
  (vc-make-backup-files t)
  (create-lockfiles nil)
  ;; autosave
  (auto-save-default nil))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package backup-walker
  :commands (backup-walker-start))

;;; Dired
(use-package dired
  :ensure nil

  :custom
  (dired-omit-files "^\\..*$" "Omit the dotfiles")
  (dired-isearch-filenames 'dwim)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-AFhlv --group-directories-first")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)

  :hook
  (dired-mode . dired-hide-details-mode)

  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil

  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-x-hands-off-my-keys t)

  :bind
  ("C-x C-j" . dired-jump)
  ("C-x 4 C-j" . dired-jump-other-window))

(use-package wdired
  :ensure nil

  :commands wdired-change-to-wdired-mode

  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package dired-subtree
  :custom
  (dired-subtree-use-backgrounds nil)

  :bind
  (:map
   dired-mode-map
   ("<tab>" . dired-subtree-toggle)
   ("<C-tab>" . dired-subtree-cycle)
   ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

;;; UI
;;;; Highlights
(use-package paren
  :ensure nil

  :config
  (show-paren-mode t))

(use-package visual-line
  :ensure nil

  :custom
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  :hook
  (text-mode . visual-line-mode))

(use-package rainbow-delimiters
  :diminish

  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package volatile-highlights
  :diminish

  :config
  (volatile-highlights-mode 1))

(use-package highlight-indentation
  :diminish

  :commands (highlight-indentation-mode))

;;;; Hydra
(use-package hydra)

;;;; Uniquify
(use-package uniquify
  :ensure nil

  :custom
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

;;;; Which key
(use-package which-key
  :demand

  :diminish

  :bind
  ("C-h C-k" . which-key-show-top-level)

  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)

  :config
  (which-key-mode))

;;;; Discover my major
(use-package discover-my-major
  :commands
  (discover-my-major discover-my-mode)

  :bind
  ("C-h C-m" . discover-my-major))

;;;; Global text scale
;; source: https://www.emacswiki.org/emacs/GlobalTextScaleMode
(use-package my/global-text-scale
  :ensure nil

  :after (hydra)

  :preface
  (define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

  (defun my/global-text-scale/adjust (inc)
    (interactive)
    (text-scale-set 1)
    (kill-local-variable 'text-scale-mode-amount)
    (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
    (global-text-scale-mode 1))

  (defun my/global-text-scale/zoom-in ()
    (interactive)
    (my/global-text-scale/adjust 1))

  (defun my/global-text-scale/zoom-out ()
    (interactive)
    (my/global-text-scale/adjust -1))

  (defun my/global-text-scale/reset ()
    (interactive)
    (my/global-text-scale/adjust (- text-scale-mode-amount))
    (global-text-scale-mode -1))

  (defhydra hydra-global-text-scale ()
    "Zoom"
    ("=" my/global-text-scale/zoom-in "Zoom in")
    ("-" my/global-text-scale/zoom-out "Zoom out")
    ("0" my/global-text-scale/reset "Reset zoom" :exit t)
    ("q" nil "Cancel"))

  (provide 'my/global-text-scale)

  :bind
  (:map
   mode-specific-map
   ("z" . 'hydra-global-text-scale/body)))

;;;; Theme
(use-package modus-themes
  :custom
  (modus-themes-syntax '(faint yellow-comments))
  (modus-themes-bold-constructs t)

  :config
  (modus-themes-load-themes)
  (load-theme 'modus-operandi))

;;;; Window sizing
(use-package my/window-sizing
  :ensure nil

  :preface
  (defhydra hydra-window-sizing (:hint nil)
    "
^Window sizing^
^ ^ _i_ ^ ^ _+_:balance
_j_ ^ ^ _l_ _=_:equalize
^ ^ _k_ ^ ^ _q_:quit mode
"
    ("j" shrink-window-horizontally)
    ("l" enlarge-window-horizontally)
    ("i" shrink-window)
    ("k" enlarge-window)
    ("+" balance-windows)
    ("=" balance-windows-area)
    ("q" nil))

  (provide 'my/window-sizing)

  :bind
  ("C-x 4 w" . 'hydra-window-sizing/body))

;;;; Window switching
(use-package ace-window
  :bind
  ("M-o" . ace-window))

;;;; Window rotation
(use-package rotate
  :after (hydra ace-window)

  :preface
  (defhydra hydra-rotate ()
    "Rotate"
    ("4" rotate-window "Rotate")
    ("l" rotate-layout "Layout")
    ("o" ace-swap-window "Swap")
    ("q" nil "Cancel"))

  :bind
  (:map
   mode-specific-map
   ("4" . 'hydra-rotate/body)))

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
(use-package beacon
  :demand

  :diminish

  :bind
  ("<f11>" . beacon-blink)

  :config
  (beacon-mode))

;;;; iBuffer
(use-package ibuffer
  :ensure nil
  :bind
  (:map
   mode-specific-map
   ("b" . ibuffer)))

;;;; Ivy
(use-package ivy
  :demand

  :diminish

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)

  :config
  (setq ivy-re-builders-alist
        '((swiper           . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (counsel-grep     . ivy--regex-plus)
          (counsel-ag       . ivy--regex-plus)
          (counsel-rg       . ivy--regex-plus)
          (t                . ivy--regex-fuzzy)))

  :hook
  (after-init . ivy-mode)

  :bind
  (:map
   ivy-minibuffer-map
   ("TAB" . ivy-partial))

  (:map
   mode-specific-map
   (:prefix
    "i"
    :prefix-map my/ivy-map
    ("r" . ivy-resume))))

(use-package counsel
  :demand

  :diminish

  :bind
  ([remap insert-char] . counsel-unicode-char)

  :config
  (counsel-mode 1))

(use-package my/helpful-counsel
  :ensure nil

  :after (helpful counsel)

  :preface
  (provide 'my/helpful-counsel)

  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package swiper
  :bind
  ("M-s s" . swiper)

  (:map
   isearch-mode-map
   ("M-s s" . swiper-from-isearch)))

(use-package ivy-rich
  :after (ivy)

  :custom
  (ivy-rich-path-style 'abbrev)

  :hook
  (ivy-mode . ivy-rich-mode))

(use-package smex)
(use-package flx)
(use-package ivy-hydra)

(use-package ivy-xref
  :after (ivy)

  :custom
  (xref-show-xrefs-function 'ivy-xref-show-xrefs))

;;;; TODO/FIXME/etc keyword highlight
(use-package hl-todo
  :demand

  :custom-face
  (hl-todo ((t (:bold t :inverse-video t))))

  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#6c71c4")
     ("FIXME" . "#dc322f")
     ("NOTE" . "#2aa198")))

  :hook
  (after-init . global-hl-todo-mode)

  :bind
  ("M-s t" . hl-todo-occur))

;;;; Helpful introspection
(use-package helpful
  :demand

  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;; Behaviour
;;;; reverse-im
(use-package reverse-im
  :demand

  :diminish

  :config
  (reverse-im-activate "russian-computer"))

;;;; Scratch
(use-package unkillable-scratch
  :preface
  (defun my/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  :hook
  (after-init . unkillable-scratch)

  :bind
  ("M-<f11>" . my/switch-to-scratch))

;;;; WWW Browser
(use-package www
  :ensure nil

  :preface
  (provide 'www)

  :custom
  (browse-url-browser-function 'browse-url-firefox))

;;; Editing
;;;; isearch
(use-package isearch
  :ensure nil

  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-highlight t)

  :config
  ;; Source: https://protesilaos.com/dotemacs
  (defun isearch/mark-and-exit ()
    "Mark the current search string and exit the search."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (isearch-done))

  ;; Source: https://protesilaos.com/dotemacs
  (defun isearch/query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  :bind
  (("M-s %" . isearch/query-replace-symbol-at-point)
   :map minibuffer-local-isearch-map
   ("M-/" . isearch-complete-edit)
   :map isearch-mode-map
   ("M-/" . isearch-complete)
   ("C-SPC" . isearch/mark-and-exit)))

;;;; Subwords
(use-package subword
  :ensure nil
  :diminish)

;;;; Electric
(use-package electric
  :hook
  (prog-mode . electric-pair-local-mode))

;;;; Whole line or region
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode

  :hook
  (after-init . whole-line-or-region-global-mode))

;;;; Indirect region editing
(use-package edit-indirect
  :bind
  (:map
   mode-specific-map
   ("'" . edit-indirect-region)))

;;;; Whitespaces
(use-package whitespace
  :ensure nil

  :diminish

  :preface
  (defun my/whitespace-prog-mode ()
    "whitespace mode for prog buffers"
    (setq-local whitespace-style '(face lines-tail tab-mark))
    (setq-local whitespace-line-column 80)
    (setq-local truncate-lines t)
    (whitespace-mode t))

  :hook
  (prog-mode . my/whitespace-prog-mode)

  :custom-face
  (whitespace-line ((t (:background "moccasin" :underline (:color foreground-color :style wave)))))
  (whitespace-tab ((t (:foreground "brown" :inverse-video nil :underline t))))

  :custom
  (indent-tabs-mode nil)
  (tab-width 4)
  (mode-require-final-newline nil))

(use-package ethan-wspace
  :demand t

  :hook
  (after-init . global-ethan-wspace-mode)

  :bind
  (:map
   mode-specific-map
   ("S" . ethan-wspace-clean-all)))

(use-package shrink-whitespace
  :bind
  ("M-\\" . shrink-whitespace))

;;;; Smart commenting
(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2))

;;;; Indentation
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :diminish " AI")

;;;; Expand Region
(use-package expand-region
  ;; :after (org)  ;; ugly hack :(
  :bind
  ("M-]" . er/expand-region)
  ("M-[" . er/contract-region))

;;;; Undo tree
(use-package undo-tree
  :diminish

  :custom
  (undo-tree-auto-save-history nil) ;; TODO: enable someday
  (undo-tree-history-directory-alist
   `((".*\\.emacs\\.d\\/elpa" . "/tmp/undo-tree-history")
     (".*" . ,(format "%sundo-tree-history" user-emacs-directory))))

  :hook
  (after-init . global-undo-tree-mode))

;;;; SmartParens & wrapping
(use-package smartparens
  :diminish " S"

  :preface
  (defun my/no-electric-with-startparens ()
    "Disables electric parens with smartparens enabled"
    (electric-pair-local-mode -1))

  :hook
  (smartparens-mode . my/no-electric-with-startparens)
  (emacs-lisp-mode . smartparens-strict-mode)
  (lisp-mode . smartparens-strict-mode)
  (lisp-interaction-mode . smartparens-strict-mode)
  (scheme-mode . smartparens-strict-mode)
  (eval-expression-minibuffer-setup . smartparens-strict-mode)

  :custom
  (sp-base-key-bindings 'sp)

  :bind
  (:map
   sp-keymap
   ("M-J" . sp-split-sexp)
   ("C-M-J" . sp-join-sexp))

  :config
  (require 'smartparens-config)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

;;;; Case formatting
(use-package string-inflection
  :bind
  (:map
   mode-specific-map
   :prefix
   "c"
   :prefix-map
   my/string-inflection-map
   ("c". string-inflection-all-cycle)
   ("s". string-inflection-underscore)
   ("C". string-inflection-camelcase)
   ("k". string-inflection-kebab-case)))

;;;; Quotes
(use-package cycle-quotes
  :bind
  (:map
   mode-specific-map
   ("q" . cycle-quotes)))

;;;; Multiple Cursors
(use-package multiple-cursors
  :preface
  (setq-default my/mc-map (make-sparse-keymap "Multiple cursors"))

  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/mark-next-word-like-this)
  ("C-M-<" . mc/mark-previous-word-like-this)

  (:map
   mode-specific-map
   :prefix
   "m"
   :prefix-map
   my/mc-map
   ("+" . mc/mark-all-like-this)
   ("r" . set-rectangular-region-anchor)
   ("c" . mc/edit-lines)
   ("e" . mc/edit-ends-of-lines)
   ("a" . mc/edit-beginnings-of-lines)))

;;;; Smart BOL
(use-package my/smart-bol
  :ensure nil

  :preface
  (defun my/smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line."
    (interactive "^p")
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  :bind
  ([remap move-beginning-of-line] . my/smarter-move-beginning-of-line))

;;;; ElDoc
(use-package eldoc
  :ensure nil
  :diminish)

;;;; Visual RegExp
(use-package visual-regexp
  :bind
  (:map
   global-map
   ([remap query-replace] . vr/query-replace))

  (:map
   my/mc-map
   ("m" . vr/mc-mark)))

;;;; Typographics
(use-package my/typographics
  :ensure nil

  :preface
  (defun my/typographics-fontify ()
    "Add a font lock highlighting for some particular characters."
    (font-lock-add-keywords
     nil
     '(("—" . '(0 font-lock-warning-face t)))))

  (provide 'my/typographics)

  :hook
  (prog-mode . my/typographics-fontify)
  (text-mode . my/typographics-fontify)

  :bind
  (:map
   mode-specific-map
   :prefix
   "8"
   :prefix-map my/typographics-map
   ("-" . "—")
   ("." . "…")))

;;; Navigation
;;;; imenu
(use-package imenu
  :ensure nil

  :bind
  ("M-g i" . imenu))

;;;; Avy
(use-package avy
  :init
  (define-prefix-command 'my/avy-map 'my/avy-map "Avy")

  :bind-keymap
  ("M-g SPC" . my/avy-map)

  :bind
  ("M-SPC" . avy-goto-char-timer)

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

;;;; Mark ring
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually moves"
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; C-u C-SPC C-SPC instead  C-u C-SPC C-u C-SPC
(setq set-mark-command-repeat-pop t)

;;;; Xref
(defun my/do-then-quit (&rest args)
  (let ((win (selected-window)))
    (apply (car args) (rest args))
    (quit-window nil win)))

(advice-add #'xref-goto-xref :around #'my/do-then-quit)

;;; Version Control
;;;; Git/Magit
(use-package magit
  :custom
  (magit-log-arguments '("--graph" "--color" "--decorate"))

  :bind
  ("C-x g" . magit-status))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package git-link
  :bind
  (:map
   mode-specific-map
   ("M-l" . git-link)))

;;;; GitHub
(use-package my/github
  :ensure nil

  :preface
  (defun my/github/match-file-url (s)
    "Extract a path and optional line number from GitHub URL"
    (when (string-match
           (rx bol
               "https://github.com/"
               (repeat 3 (seq (one-or-more (not (any ?/)))
                              (char ?/)))
               (seq (group (one-or-more (not (any ?/))))
                    (char ?/))
               (group (one-or-more (not (any ?#))))
               (optional (seq "#"
                              (group (seq "L"
                                          (one-or-more digit)
                                          (optional (seq "-L")
                                                    (one-or-more digit))))))
               eol)
           s)
      (list (cons 'ref (match-string 1 s))
            (cons 'path (match-string 2 s))
            (cons 'line (match-string 3 s)))))

  (defun my/github/commit-hash-p (s)
    "Check what string is a commit reference"
    (when (string-match
           (rx bol
               (repeat 40 (or digit letter))
               eol)
           s)
      t))

  (provide 'my/github))

;;; Languages
;;;; LSP
(use-package lsp-mode
  :ensure nil

  :commands (lsp)

  :hook
  (lsp-mode . lsp-enable-which-key-integration)

  :custom
  (lsp-enable-snippet nil)
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "C-c C-l"))

(use-package lsp-ui
  :after (lsp)

  :hook
  (lsp-mode . lsp-ui-mode)

  :bind
  (:map
   lsp-ui-mode-map
   ("M-g i" . lsp-ui-imenu))

  :custom
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-enable nil))

;;;; ELisp
(use-package elisp-mode
  :ensure nil

  :hook
  (emacs-lisp-mode . eldoc-mode)
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package my/helpful-elisp-mode
  :ensure nil

  :after (elisp-mode helpful)

  :preface
  (provide 'my/helpful-elisp-mode)

  :bind
  (:map
   emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

;;;; Shell Scripts
(use-package sh-mode
  :ensure nil

  :mode
  ("/\\.xsessionrc\\'|/\\.xprofile\\'" . sh-mode))

;;;; Racket (Geiser), Pollen
(use-package geiser
  :ensure nil

  :mode
  ("\\.rkt\\'" . racket-mode)

  :hook
  (racket-mode . smartparens-strict-mode))

(use-package pollen-mode
  :ensure nil

  :commands (pollen-mode)

  :init
  ;; I don't use :mode because ~use-package~ doesn't support
  ;; the (PATTERN MODE NON-NIL) format :/
  (add-to-list 'auto-mode-alist '("\\.pm$" . pollen-mode))
  (add-to-list 'auto-mode-alist '("\\.pmd$" . pollen-mode))
  (add-to-list 'auto-mode-alist '("\\.pp$" pollen-mode t))
  (add-to-list 'auto-mode-alist '("\\.p$"  pollen-mode t)))

;;;; Sly
(use-package sly
  :hook
  (sly-editing-mode . smartparens-strict-mode)
  (sly-editing-mode . rainbow-delimiters-mode)
  (sly-mrepl-mode . smartparens-strict-mode)
  (sly-mrepl-mode . rainbow-delimiters-mode)

  :config
  (when-let ((sbcl (executable-find "sbcl")))
    (setq inferior-lisp-program sbcl))

  ;; do (ql:quickload :clhs) before!
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

  (sp-local-pair
   '(sly-editing-mode
     sly-mrepl-mode)
   "'" nil :actions nil))

;;;; Clojure
(use-package clojure-mode
  :ensure nil

  :mode
  ("\\.clj\\'" . clojure-mode)
  ("\\.cljc\\'" . clojure-mode)
  ("\\.cljx\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)

  :hook
  (clojure-mode . aggressive-indent-mode)
  (clojure-mode . rainbow-delimiters-mode)
  (clojure-mode . smartparens-strict-mode)

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
  (cider-repl-mode . smartparens-strict-mode)
  (cider-mode . rainbow-delimiters-mode)
  (cider-mode . smartparens-strict-mode)

  :custom
  (cider-repl-result-prefix ";; => "))

;;;; Haskell
(use-package haskell-mode
  :ensure nil

  :diminish haskell-mode

  :magic
  (".*env stack" . haskell-mode)
  (".*env cabal" . haskell-mode)

  :mode
  ("\\.hs\\'" . haskell-mode)
  ("\\.lhs\\'" . literate-haskell-mode)
  ("\\.cabal\\'" . haskell-cabal-mode)
  ("\\.hamlet\\'" . shakespeare-hamlet-mode)
  ("\\.julius\\'" . shakespeare-julius-mode)
  ("routes\\'" . haskell-yesod-parse-routes-mode)

  :bind
  (:map
   haskell-mode-map
   :prefix
   "C-c SPC"
   :prefix-map
   my/haskell-map
   ("v" . haskell-cabal-visit-file)
   ("m" . haskell-auto-insert-module-template)
   ("I" . haskell-sort-imports)
   ("A" . haskell-align-imports)
   ("S" . haskell-mode-stylish-buffer)
   ("y" . haskell-hayoo)
   ("h" . haskell-hide-toggle)
   ("u" . my/haskell-swiper-todos))

  :hook
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . subword-mode)
  (haskell-mode . eldoc-mode)
  (haskell-mode . smartparens-mode)
  (haskell-mode . my/boot-haskell)
  (haskell-mode . interactive-haskell-mode)

  :config
  (defun my/haskell-swiper-todos ()
    "Shows the Swiper for todo-like items."
    (interactive)
    (swiper "undefined\\|TODO\\|FIXME"))

  (defun my/boot-haskell ()
    "Initialize haskell stuff"
    (interactive)
    (setq tags-case-fold-search nil))

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

(put 'haskell-stylish-on-save 'safe-local-variable #'booleanp)
(put 'haskell-hayoo-url 'safe-local-variable #'stringp)

(use-package hi2
  :after (haskell-mode)

  :diminish

  :hook
  (haskell-mode . hi2-mode)

  :custom
  (hi2-layout-offset 2)
  (hi2-left-offset 2)
  (hi2-where-post-offset 2)

  :bind
  (:map
   hi2-mode-map
   ("<tab>" . hi2-indent-line)))

(put 'hi2-where-post-offset 'safe-local-variable #'numberp)
(put 'hi2-left-offset 'safe-local-variable #'numberp)
(put 'hi2-layout-offset 'safe-local-variable #'numberp)

(use-package company-cabal
  :after (haskell-mode)

  :config
  (add-to-list 'company-backends 'company-cabal))

(use-package shakespeare-mode
  :after (haskell-mode))

(use-package inf-haskell
  :ensure nil)

(use-package lsp-haskell
  :if nil

  :after (haskell-mode)

  :hook
  (haskell-mode . lsp)

  :custom
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

;;;; Python
(use-package python
  :ensure nil

  :mode
  ("\\.py\\'" . python-mode)

  :hook
  (python-mode . smartparens-mode)

  :bind
  (:map
   python-mode-map
   ("C-c C-c" . compile)))

(use-package elpy
  :after (python)

  :hook
  (elpy-mode . flycheck-mode)

  :custom
  (elpy-rpc-python-command "python3")
  (elpy-rpc-virtualenv-path 'current)
  (elpy-modules
   '(elpy-module-company
     elpy-module-eldoc
     elpy-module-highlight-indentation
     elpy-module-django))

  :config
  (elpy-enable)
  (unbind-key "<C-left>" elpy-mode-map)
  (unbind-key "<C-right>" elpy-mode-map))

(use-package my/python
  :ensure nil

  :custom
  (python-shell-interpreter "python3")
  (lsp-pylsp-configuration-sources ["flake8"])

  :preface
  (defun my/python-mode-hook ()
    (if (executable-find "pylsp")
        (lsp)
      (elpy-mode)))

  :hook
  (python-mode . my/python-mode-hook))

;;;; Rust
(use-package rust-mode
  :ensure nil

  :mode
  ("\\.rs\\'" . rust-mode)

  :config
  (add-to-list 'company-dabbrev-code-modes 'rust-mode))

(use-package flycheck-rust
  :after (rust-mode)

  :hook
  (rust-mode . my/rust-mode-hook)

  :config
  (defun my/rust-mode-hook ()
    (flycheck-rust-setup)
    (flycheck-mode)))

;;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)

  :mode
  ("README.*\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)

  :hook
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . yas-minor-mode)
  (markdown-mode . smartparens-mode)

  :custom
  (markdown-command "pandoc")
  (markdown-header-scaling t)

  :config
  (unbind-key "DEL" gfm-mode-map))

(use-package my/markdown
  :ensure nil

  :after (markdown-mode my/github)

  :preface
  (defun my/markdown/capture-gh-link (&optional arg)
    "Insert a MD-link for the killed GitHub URL."
    (interactive "p")
    (let* ((url (current-kill 0))
           (match (my/github/match-file-url url))
           (path (alist-get 'path match))
           (line (alist-get 'line match))
           (ref (alist-get 'ref match)))
      (if path
          (if (or (my/github/commit-hash-p ref) (= 4 arg))
              (insert
               (format "[`%s%s`](%s)"
                       path
                       (if line (format ":%s" line) "")
                       url))
            (message "%s" "Non-local ref!"))
        (message "%s" "Non-github link!"))))

  (provide 'my/markdown)

  :bind
  (:map
   markdown-mode-map
   ("C-c l" . my/markdown/capture-gh-link)

   :map
   gfm-mode-map
   ("C-c l" . my/markdown/capture-gh-link)))

(use-package my/markdown-binding-fixes
  :ensure nil

  :after markdown-mode

  :preface
  (provide 'my/markdown-binding-fixes)

  :bind
  (:map
   markdown-mode-map
   ("C-." . undo-tree-undo)
   ("C-," . undo-tree-redo))

  (:map
   gfm-mode-map
   ("C-." . undo-tree-undo)
   ("C-," . undo-tree-redo)))

;;;; Elm
(use-package elm-mode
  :ensure nil

  :mode "\\.elm\\'"

  :custom
  (elm-indent-look-past-empty-line nil))

(put 'elm-format-on-save 'safe-local-variable #'booleanp)
(put 'elm-compile-arguments 'safe-local-variable #'listp)

(use-package flycheck-elm
  :after (elm-mode)

  :hook
  (elm-mode . flycheck-mode)
  (flycheck-mode . flycheck-elm-setup))

(use-package my/elm
  :ensure nil

  :preface
  (defun my/elm-mode-hook ()
    (when (equal (f-ext (or (buffer-file-name) "")) "elm")
      (elm--find-dependency-file-path))
    (elm-indent-mode -1))

  :hook
  (elm-mode . my/elm-mode-hook))

;;;; Go
(use-package go-mode
  :ensure nil

  :mode "\\.go\\'"

  :commands (go-mode)

  :hook (flycheck-mode)

  :config
  (defun my/go-mode-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save))

  (when (executable-find "gofmt")
    (add-hook 'go-mode-hook #'my/go-mode-hook)))

;;;; PureScript
(use-package purescript-mode
  :ensure nil

  :mode "\\.purs\\'"

  :hook
  (purescript-mode . psc-ide-mode))

(use-package psc-ide
  :ensure nil

  :after (purescript-mode)

  :commands (psc-ide-mode)

  :diminish

  :hook
  (purescript-mode . company-mode)
  (purescript-mode . flycheck-mode)
  (purescript-mode . my/purescript-mode-hook)

  :config
  (defun my/purescript-mode-hook ()
    (turn-on-purescript-indentation)))

;;;; Web
(use-package web-mode
  :commands (web-mode)

  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.css\\'" . web-mode)

  :preface
  (defun my/web-mode-hook ()
    (add-hook 'hack-local-variables-hook
              (defun my/web-mode-local-hook ()
                (when web-mode-engines-alist
                  (web-mode-guess-engine-and-content-type)
                  (unless (string= web-mode-engine "none")
                    (web-mode-set-engine web-mode-engine))))
              0 t))

  :hook
  (web-mode . company-mode)
  (web-mode . my/web-mode-hook)

  :custom
  (web-mode-enable-css-colorization t)
  (web-mode-enable-engine-detection nil)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-css-indent-offset 2)
  (web-mode-style-padding 2)

  :config
  (add-to-list 'company-backends 'company-css))

(put 'web-mode-engine 'safe-local-variable #'stringp)
(put 'web-mode-engines-alist 'safe-local-variable #'listp)

;;;; PlantUML
(use-package puml-mode
  :ensure nil

  :mode
  ("\\.puml\\'" . puml-mode)
  ("\\.plantuml\\'" . puml-mode))

;;;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"

  :hook
  (yaml-mode . highlight-indentation-mode)
  (yaml-mode . smartparens-mode)

  :bind
  (:map
   yaml-mode-map
   (">" . nil)))

;;;; TOML
(use-package toml-mode
  :ensure nil

  :mode "\\.toml\\'"

  :hook
  (toml-mode . smartparens-mode))

;;;; CSV
(use-package csv-mode
  :ensure nil

  :mode "\\.[Cc][Ss][Vv]\\'")

;;;; Nix
(use-package nix-mode
  :ensure nil

  :mode "\\.nix\\'")

;;;; SQL
(setq-default sql-dialect 'sql-postgres)

;;;; Kotlin
(use-package kotlin-mode
  :ensure nil

  :mode "\\.kts?\\'"

  :hook
  (kotlin-mode . highlight-indentation-mode))

;;;; Shell
(use-package sh-script
  :ensure nil

  :mode
  ("\\.ok\\'" . shell-script-mode)
  ("\\.sh\\'" . shell-script-mode)
  ("\\.bash\\'" . shell-script-mode))

;;;; Io
(use-package io-mode
  :ensure nil

  :mode
  ("\\.io\\'" . io-mode)

  :hook
  (io-mode . highlight-indentation-mode)

  :config
  ;; fix comment regex
  (set-variable
   'io-comments-re
   "\\(^#.*$#\\|//.*$\\|/\\*\\(.\\|[]\\)*?\\*/\\)"))

;;;; Processing
(use-package processing-mode
  :ensure nil

  :mode "\\.pde\\'"

  :custom
  (processing-location "~/.software/processing/processing-java")
  (processing-application-dir "~/.software/processing/")
  (processing-sketchbook-dir "~/Projects/processing"))

;;;; Gemini
(use-package gemini-mode
  :ensure nil

  :preface
  (defun my/gemini/insert-gemlog-link ()
    (interactive)
    (let* ((title (read-string "title: "))
           (slug (replace-regexp-in-string (rx (not alphanumeric)) "-" title))
           (today (format-time-string "%Y-%m-%d" (current-time))))
      (insert
       (format "=> ./%s-%s.gmi %s - %s" today slug today title))))

  :after (visual-fill-column)

  :mode ("\\.gmi$" . gemini-mode))

;;;; Dockerfile
(use-package dockerfile-mode)

;;; IDE
;;;; Autocompletion and abbreviation
(use-package abbrev
  :ensure nil

  :diminish)

(use-package dabbrev
  :ensure nil

  :commands (dabbrev-expand dabbrev-completion)

  :custom
  (dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (dabbrev-backward-only nil)
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-check-other-buffers t)
  (dabbrev-eliminate-newlines nil)
  (dabbrev-upcase-means-case-search t))

(use-package hippie
  :ensure nil
  :after (dabbrev)

  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-expand-list-all-buffers
     try-expand-list
     try-expand-line-all-buffers
     try-expand-line
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs))

  :bind
  ([remap dabbrev-expand] . hippie-expand))

(use-package company
  :demand

  :diminish

  :hook
  (after-init . global-company-mode)

  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 30)
  (company-idle-delay nil)
  (company-begin-commands '(self-insert-command))
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-transformers '(company-sort-by-occurrence))

  :bind
  (:map
   mode-specific-map
   ("/" . company-files))
  (:map
   company-mode-map
   ("M-<tab>" . company-complete))
  (:map
   company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(put 'company-backends 'safe-local-variable #'listp)

(use-package company-posframe
  :disabled ;; TODO: buggy :/

  :if (not (string-equal system-type "darwin"))

  :after (company)

  :diminish

  :hook
  (company-mode . company-posframe-mode))

(use-package company-try-hard
  :after (company)

  :bind
  (:map
   company-active-map
   ("M-<tab>" . company-try-hard)))

(use-package company-flx
  :after (company)

  :hook
  (company-mode . company-flx-mode))

(use-package company-quickhelp
  :after (company)

  :diminish company-quickhelp-mode

  :bind
  (:map
   company-active-map
   ("C-h" . company-quickhelp-manual-begin)))

;;;; Flycheck
(use-package flycheck
  :diminish " FC"

  :custom
  (flycheck-check-syntax-automatically
   '(save mode-enabled) "Only check on save")

  :bind
  (:map
   flycheck-mode-map
   ("<f5>" . flycheck-buffer)))

(put 'safe-local-variable-values 'flycheck-checker 'flycheck-ghc)

(use-package flycheck-color-mode-line
  :after (flycheck)

  :hook
  (flycheck-mode . flycheck-color-mode-line-mode))

;;;; Flymake
(use-package flymake
  :ensure nil

  :preface
  (defvar my/flymake-minor-mode-map (make-keymap))
  (define-minor-mode my/flymake-minor-mode
    "Auxiliary minor mode for flymake-minor-mode"
    nil nil 'my/flymake-minor-mode-map)

  :hook
  (flymake-mode . my/flymake-minor-mode)

  :bind
  (:map
   my/flymake-minor-mode-map
   ("M-g p" . flymake-goto-prev-error)
   ("M-g n" . flymake-goto-next-error)
   ("M-g M-p" . flymake-goto-prev-error)
   ("M-g M-n" . flymake-goto-next-error)))

;;;; Yasnippet
(use-package yasnippet
  :diminish (yas-minor-mode . " Y")

  :preface
  (setq-default my/yas-map (make-sparse-keymap "My Yasnippet map"))

  :bind
  (:map
   mode-specific-map
   :prefix
   "y"
   :prefix-map
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
  :after (yasnippet))

;;;; YankPad
(use-package yankpad
  :after (yasnippet)

  :custom
  (yankpad-file "~/Dropbox/org/yankpad.org")

  :bind
  (:map
   my/yas-map
   ("m" . yankpad-map)
   ("y" . yankpad-insert)))

(put 'yankpad-file 'safe-local-variable #'stringp)

;;;; Grep'likes
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; sudo apt-get install silversearcher-ag
(use-package ag
  :if (executable-find "ag")
  :custom
  (ag-highlight-search t))

;; install from github
(use-package rg
  :if (executable-find "rg")
  :after (wgrep)

  :custom
  (rg-group-result t)
  (rg-hide-command t)
  (rg-show-columns nil)
  (rg-show-header t)
  (rg-custom-type-aliases nil)
  (rg-default-alias-fallback "all")

  :config
  ;; source: https://protesilaos.com/dotemacs
  (rg-define-search rg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !*.lock"))

  ;; source: https://protesilaos.com/dotemacs
  (defun rg/save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind
  ("M-s g" . rg/grep-vc-or-dir)
  (:map
   rg-mode-map
   ("s" . rg/save-search-as-name)
   ("C-n" . next-line)
   ("C-p" . previous-line)
   ("M-n" . rg-next-file)
   ("M-p" . rg-prev-file)))

;;;; Projectile
(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-mode-line-function
   (lambda () (format " [%s]" (projectile-project-name)))))

(use-package counsel-projectile
  :after (projectile counsel ivy)

  :custom
  (projectile-completion-system 'ivy)

  :hook
  (after-init . counsel-projectile-mode))

(use-package projectile-ripgrep
  :if (package-installed-p 'ripgrep)

  :after (projectile)

  :bind
  (:map
   projectile-command-map
   ("s r" . projectile-ripgrep)))

(put 'projectile-tags-file-name 'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-files 'safe-local-variable #'listp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)

;;;; Terminal here
(use-package terminal-here
  :after (projectile)

  :bind
  (:map
   mode-specific-map
   :prefix
   "t"
   :prefix-map
   my/terminal-here-map
   ("t" . terminal-here-launch)
   ("p" . terminal-here-project-launch))

  :custom
  (terminal-here-project-root-function 'projectile-project-root)
  (terminal-here-terminal-command (list "x-terminal-emulator")))

;;;; RESTclient
(use-package restclient
  :commands
  (restclient-mode))

(use-package company-restclient
  :after (restclient)

  :hook
  (restclient-mode . my/restclient-mode-hook)

  :config
  (defun my/restclient-mode-hook ()
    (add-to-list 'company-backends
                 'company-restclient)))

;;;; Docker
(use-package docker
  :commands (docker))

;;; Spell Checking
(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil

  :commands
  (ispell-buffer)

  :custom
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-program-name "hunspell")
  (ispell-dictionary "ru_RU,en_US")

  :config
  (when (executable-find "hunspell-wrapper")
    (setq-default ispell-program-name "hunspell-wrapper"))
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US"))

(use-package flyspell
  :ensure nil

  :commands (flyspell-buffer flyspell-mode)

  :bind
  ("M-<f5>" . flyspell-buffer)
  ("M-<f8>" . flyspell-goto-next-error)
  (:map
   mode-specific-map
   ("s" . flyspell-correct-word-before-point)))

;;; Org-mode/Outline
;;;; Org
(use-package org
  :ensure org

  :pin org

  :mode ("\\.org\\'" . org-mode)

  :bind
  ("<f12>" . my/org-open-notes-file)
  (:map
   mode-specific-map
   ("C" . org-capture)
   ("<backspace>" . org-mark-ring-goto))

  (:map
   org-mode-map
   ("C-c M-RET" . org-insert-heading-after-current)
   ("C-c L" . org-cliplink))

  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . yas-minor-mode)
  (org-mode . smartparens-mode)
  (org-mode . my/org-mode-hook)

  :custom-face
  (org-link ((t (:inherit link :bold nil))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-block-begin-line ((t (:inherit fixed-pitch))))
  (org-block-end-line ((t (:inherit fixed-pitch))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-tag ((t (:weight normal :height 0.8))))

  :custom
  (org-directory "~/Dropbox/org")
  (org-default-notes-file "~/Dropbox/org/notes.org")
  (org-edit-src-content-indentation 0)
  (org-ellipsis "…")
  (org-enforce-todo-dependencies t)
  (org-export-use-babel nil)
  (org-export-with-sub-superscripts nil)
  (org-export-with-toc nil)
  (org-hide-leading-stars t)
  (org-html-postamble nil)
  (org-html-preamble nil)
  (org-outline-path-complete-in-steps nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (org-use-sub-superscripts nil)
  (org-adapt-indentation nil)
  (org-return-follows-link t)

  :config
  (require 'ob-shell)
  (require 'ob-python)
  (require 'ob-haskell)

  (defvar my/org-babel-langs
    '((shell . t)
      (emacs-lisp . t)
      (python . t)
      (haskell . t)))

  (defun my/org-find-file (file)
    "Find file, do it in other window witn C-u"
    (if current-prefix-arg
        (find-file-other-window file)
      (find-file file)))

  (defun my/org-open-notes-file ()
    (interactive)
    (if (file-exists-p org-default-notes-file)
        (find-file org-default-notes-file)
      (message "%s doesn't exist!" org-default-notes-file)))

  (setq org-link-frame-setup
        (cons '(file . my/org-find-file)
              (assq-delete-all 'file org-link-frame-setup)))

  (defun my/org-babel-load-langs ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     my/org-babel-langs))

  (defun my/org-mode-hook ()
    "Tweaks an org-mode"
    (setq-local truncate-lines nil))

  (setq
   org-capture-templates
   '(("t" "Add a daily note" entry
      (file+olp+datetree "" "Daily")
      "* %?\n%i"
      )
     ))
  )

(put 'org-default-notes-file 'safe-local-variable #'stringp)
(put 'org-export-use-babel 'safe-local-variable #'null)

(use-package org-bullets
  :after (org)

  :hook
  (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list '("●" "○" "⦿" "⦾")))

(use-package htmlize
  :after (org))

(use-package ox-pandoc
  :if (executable-find "pandoc")

  :after (org))

(use-package org-cliplink
  :bind
  (:map
   org-mode-map
   ("C-c S-l" . org-cliplink)))

(use-package ox-gfm
  :after (org))

(use-package ob-restclient
  :after (org restclient)

  :commands (org-babel-execute:restclient)

  :config
  (add-to-list 'my/org-babel-langs '(restclient . t))
  (my/org-babel-load-langs))

(use-package my/org
  :ensure nil

  :after (org my/github)

  :preface
  (defun my/org/capture-gh-link (&optional arg)
    "Insert a MD-link for the killed GitHub URL."
    (interactive "p")
    (let* ((url (current-kill 0))
           (match (my/github/match-file-url url))
           (path (alist-get 'path match))
           (line (alist-get 'line match))
           (ref (alist-get 'ref match)))
      (if path
          (if (or (my/github/commit-hash-p ref) (= 4 arg))
              (insert
               (format "[[%s][%s%s]]"
                       url
                       path
                       (if line (format ":%s" line) "")))
            (message "%s" "Non-local ref!"))
        (message "%s" "Non-github link!"))))

  (provide 'my/org)

  :bind
  (:map
   org-mode-map
   ("C-c l" . my/org/capture-gh-link)))

(use-package my/org-slideshow
  :ensure nil

  :after (org)

  :commands
  (my/org-simple-slideshow
   my/org-simple-slideshow-next
   my/org-simple-slideshow-prev)

  :bind
  (:map
   org-mode-map
   ("<f5>" . my/org-simple-slideshow))

  :preface
  (provide 'my/org-slideshow)

  :config
  (defun my/org-renarrow (move)
    (when (buffer-narrowed-p)
      (beginning-of-buffer)
      (widen)
      (let ((pos (funcall move)))
        (when pos
          (org-narrow-to-subtree)
          pos))))

  (defun my/org-try-renarrow (move)
    (when (buffer-narrowed-p)
      (when (save-excursion
              (save-restriction
                (my/org-renarrow move)))
        (my/org-renarrow move))))

  (defun my/org-simple-slideshow-first ()
    (interactive)
    (when (buffer-narrowed-p)
      (widen)
      (beginning-of-buffer)
      (org-next-visible-heading 1)
      (when (org-at-heading-p)
        (org-narrow-to-subtree))))

  (defun my/org-simple-slideshow-next ()
    (interactive)
    (my/org-try-renarrow #'org-get-next-sibling))

  (defun my/org-simple-slideshow-prev ()
    (interactive)
    (my/org-try-renarrow #'org-get-last-sibling))

  (defhydra my/org-simple-slideshow-hydra ()
    "Slideshow"
    ("<home>" my/org-simple-slideshow-first "First")
    ("<prior>" my/org-simple-slideshow-prev "Prev")
    ("<next>" my/org-simple-slideshow-next "Next")
    ("q" widen "Stop" :exit t))

  (defun my/org-simple-slideshow ()
    (interactive)
    (unless (buffer-narrowed-p)
      (org-narrow-to-subtree))
    (when (buffer-narrowed-p)
      (my/org-simple-slideshow-hydra/body))))

;;;; Outshine
(use-package outshine
  :diminish
  (outline-minor-mode . "")
  (outshine-mode . " O")

  :bind
  (:map
   outline-minor-mode-map
   ([C-tab] . outshine-cycle-buffer))

  :hook
  (outline-minor-mode . outshine-mode)
  (prog-mode . outline-minor-mode)

  :custom
  (outshine-preserve-delimiter-whitespace t)
  (outshine-cycle-emulate-tab t)

  :config
  ;; unbind M-tab
  (unbind-key "C-M-i" outline-minor-mode-map))

;;; Other
;;;; Nov
(use-package nov
  :ensure nil

  :mode
  ("\\.epub\\'" . nov-mode))

;;;; Olivetti
(use-package olivetti
  :after (hydra)

  :preface
  (defhydra hydra-olivetti ()
    "Olivetti"
    ("o" olivetti-mode "Toggle" :exit t)
    ("-" olivetti-shrink "Shrink")
    ("=" olivetti-expand "Expand")
    ("q" nil "Cancel"))

  :bind
  (:map
   mode-specific-map
   ("o" . 'hydra-olivetti/body))

  :custom
  (olivetti-body-width 64))

;;; Finalization
;; restore GC-limit after timeout
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)))
