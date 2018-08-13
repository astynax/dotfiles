;;; Initialization
;; increase GC-limit up to 100M for boot speedup
(setq gc-cons-threshold 100000000)

;; just a shortcut :)
(defun my/configure ()
  "Opens user-init-file"
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "M-<f12>") 'my/configure)

;;;; Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3625c04fa4b8a802e96922d2db3f48c9cb2f93526e1dc24ba0b400e4ee4ccd8a" "b6f42c69cf96795c75b1e79e5cd8ca62f9f9a0cb07bf11d1e0b49f97785358f1" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(package-selected-packages
   (quote
    (csound-mode yasnippet-snippets csv-mode geiser org-cliplink google-translate nix-mode ace-window go-mode web-mode reverse-im hydra hl-todo diminish cider htmlize ox-pandoc psc-ide-emacs psc-ide-mode psc-ide company-quickhelp shakespeare-mode projectile-ripgrep company-try-hard helm-flycheck helm-swoop outshine backup-walker backup-walket helm-xref helm-ag helm-projectile helm plantuml-mode bifocal yaml-mode seq dired-subtree ace-link company-web company-cabal org-brain terminal-here emmet-mode counsel ob-restclient zoom-window zeal-at-point yankpad whole-line-or-region which-key volatile-highlights vimish-fold use-package unkillable-scratch undo-tree toml-mode swiper sr-speedbar solarized-theme smartparens shrink-whitespace rust-mode ripgrep rainbow-delimiters purescript-mode projectile org names markdown-mode magit lua-mode intero hindent hi2 guide-key git-timemachine ghc fullframe flycheck-rust flycheck-purescript flycheck-haskell flycheck-elm flycheck-color-mode-line fireplace expand-region eno elpy elm-mode discover-my-major dired-single dired-hacks-utils dired-details+ company-restclient company-flx comment-dwim-2 clojure-mode-extra-font-locking caseformat beacon avy-zap auto-indent-mode align-cljlet aggressive-indent ag ace-mc)))
 '(safe-local-variable-values (quote ((flycheck-check-syntax-automatically (quote nil))))))

;;;; Package menagement
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;;(setq package-pinned-packages '((cider . "melpa")))

;; ensure use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

;;;; Shared keymaps
(bind-keys
 :prefix "C-c y"
 :prefix-map my/yas-map)

;;; UI
;;;; "tweaks"
(setq indicate-empty-lines t)

;;;;; Window title
(setq frame-title-format "Emacs: %b")

;;;;; Bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;;;; Cursor position
(global-hl-line-mode t)
(line-number-mode t)
(column-number-mode t)

;;;;; Cursor look
(blink-cursor-mode -1)
(setq cursor-type 'bar)
(setq x-stretch-cursor t)

;;;;; hl matching parenthesis
(show-paren-mode)

;;;; Mode line
(setq mode-line-position
      '((line-number-mode ("%l" (column-number-mode ":%c")))))

;;;; Font
(setq
 my/font-anonymous
 (font-spec :family "Anonymous Pro"
            :width 'normal
            :size 20)
 my/font my/font-anonymous)

(defun my/set-font (font)
  (set-face-attribute 'default nil :font font)
  (setq after-make-frame-functions
        (lambda (&optional ARGS)
          (set-face-attribute 'default nil :font font))))

(my/set-font my/font-anonymous)

;;;; Global symbol prettify
(global-prettify-symbols-mode t)

;;;; Hydra
(use-package hydra
  :ensure t)

;;;; Which key
(use-package which-key
  :ensure t

  :diminish which-key-mode

  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25)
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

(defhydra hydra-global-text-scale (global-map "C-c z")
  "Zoom"
  ("=" global-text-scale/zoom-in "Zoom in")
  ("-" global-text-scale/zoom-out "Zoom out")
  ("0" global-text-scale/reset "Reset zoom"))

;;;; Theme
(use-package solarized-theme
  :ensure t

  :config
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light))

;;;; Diminish'ing
(use-package diminish
  :ensure t

  :config
  (diminish 'eldoc-mode))

;;;; Window switching
(use-package ace-window
  :ensure t

  :bind
  ("M-o" . ace-window))

;;;; Fullframe
(use-package fullframe
  :ensure t

  :config
  (fullframe list-packages quit-window)
  (fullframe package-list-packages quit-window)
  (fullframe magit-log-all quit-window)
  (fullframe magit-log-current quit-window))

;;;; Popup windows manupulation
(use-package popwin
  :ensure t

  :config
  (popwin-mode))

;;;; Beacon
;; (visualizes cursor position)
(use-package beacon
  :ensure t

  :diminish beacon-mode

  :bind
  ("<f11>" . beacon-blink)

  :config
  (beacon-mode 1)
  (setq beacon-color "#A06600"
        beacon-push-mark 35
        beacon-dont-blink-commands nil))

;; h/l some actions like a killing, yanking, pasting e.t.c.
(use-package volatile-highlights
  :ensure t

  :diminish volatile-highlights-mode

  :config
  (volatile-highlights-mode 1))

;;;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t

  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Helm
(use-package helm
  :ensure t

  :diminish helm-mode

  :init
  (setq helm-command-prefix-key "C-c h")

  :bind
  ("C-x b" . helm-mini)
  ("C-x C-b" . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-h a" . helm-apropos)
  ("M-s o" . helm-occur)
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)

  (:map
   helm-command-map
   ("SPC" . helm-all-mark-rings))

  (:map
   minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history))

  (:map
   isearch-mode-map
   ("C-o" . helm-occur-from-isearch))

  (:map
   search-map
   ("C-o" . helm-occur-from-isearch))

  :config
  (require 'helm-config)
  (helm-mode 1)

  (setq helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-split-window-default-side 'below
        helm-split-window-in-side-p nil
        helm-scroll-amount 8)

  ;; autoresize
  (setq helm-autoresize-max-height 30
        helm-autoresize-min-height 0)
  (helm-autoresize-mode 1))

(use-package helm-xref
  :ensure t

  :after (helm)

  :init
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs)

  :commands (helm-xref-show-xrefs))

(use-package helm-swoop
  :ensure t

  :after (helm)

  :bind
  (:map
   helm-command-map
   ("M-s s" . helm-swoop))

  (:map
   isearch-mode-map
   ("M-s M-s" . helm-swoop-from-isearch)))

;;;; TODO/FIXME/etc keyword highlight
(use-package hl-todo
  :ensure t

  :init

  :commands
  (global-hl-todo-mode)

  :bind
  ("M-s t" . hl-todo-occur)

  :custom-face
  (hl-todo ((t (:bold t :background "#073642"))))

  :config
  (setq
   hl-todo-keyword-faces
   '(("TODO" . "#6c71c4")
     ("FIXME" . "#dc322f")
     ("NOTE" . "#2aa198")))

  (global-hl-todo-mode))

;;; Behaviour
;;;; reverse-im
(use-package reverse-im
  :ensure t

  :diminish reverse-im-mode

  :custom
  (reverse-im-input-methods '("russian-computer"))

  :config
  (reverse-im-activate "russian-computer"))
;;;; Keybindings
(global-unset-key (kbd "C-z"))

(bind-key "C-c b" 'ibuffer)

;;;; Startup messages e.t.c
(setq initial-scratch-message ""
      inhibit-startup-screen t
      inhibit-startup-message t)

;;;; Autosaves & backup
(setq
 auto-save-default nil

 auto-save-file-name-transforms
 `((".*" ,(format "%sauto-saves/\\2" user-emacs-directory) t))

 my/backup-directory-per-session
 (format "%sbackups/per-session" user-emacs-directory)

 my/backup-directory-per-save
 (format "%sbackups/per-save" user-emacs-directory)

 backup-directory-alist
 `((".*" . ,my/backup-directory-per-save))

 backup-by-copying t
 kept-new-versions 10
 kept-old-versions 0
 delete-old-versions t
 version-control t
 vc-make-backup-files t)

(defun my/force-backup-of-buffer ()
  (when (not buffer-backed-up)
    (let ((backup-directory-alist `((".*" . ,my/backup-directory-per-session)))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'my/force-backup-of-buffer)

(use-package backup-walker
  :ensure t

  :commands (backup-walker-start))

(put 'create-lockfiles 'safe-local-variable #'booleanp)

;;;; Misc
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; No TABs in source
(setq-default indent-tabs-mode nil)

;; short "y/n" messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(require 'uniquify)

;; No Shift-selection!
(setq shift-select-mode nil)

;; Exit confirmation
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
            kill-emacs-query-functions))

(setq resize-mini-windows t)

(setq calendar-week-start-day 1
      calendar-date-style 'european)

;;;; Scratch
(use-package unkillable-scratch
  :ensure t

  :config
  (unkillable-scratch 1))

(defun my/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(bind-key "M-<f11>" #'my/switch-to-scratch)

;;;; Client/server
(use-package server
  :ensure t

  :config
  (unless (server-running-p)
    (server-start)))

;;;; Dired
(put 'dired-find-alternate-file 'disabled nil)

(require 'dired)
(require 'dired-x)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (setq dired-omit-files "^\\..*$")
            (add-hook 'dired-mode-hook
                      (lambda () (dired-omit-mode)))))

(setq dired-recursive-deletes 'top
      dired-listing-switches "-al --group-directories-first")

(use-package dired-single
  :ensure t)

(use-package dired-details+
  :ensure t

  :config
  (setq dired-details-initially-hide nil
        dired-details-propagate-flag nil))

(use-package dired-subtree
  :ensure t

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

;;; Editing
;;;; Misc
(electric-pair-mode t)

;;;; Whole line or region
(use-package whole-line-or-region
  :ensure t

  :diminish whole-line-or-region-local-mode

  :config
  (whole-line-or-region-mode t))

;;;; Whitespaces
(use-package whitespace
  :ensure t

  :diminish whitespace-mode

  :custom-face
  (whitespace-line ((t (:background "moccasin" :underline (:color foreground-color :style wave)))))
  (whitespace-tab ((t (:foreground "brown" :inverse-video nil :underline t))))

  :config
  (setq
   whitespace-style '(face lines-tail trailing tab-mark)
   whitespace-line-column 80
   default-tab-width 4)

  (defun my/whitespace-mode ()
    "whitespace mode for prog buffers"
    (setq require-final-newline t
          next-line-add-newlines nil)
    (whitespace-mode t)
    (toggle-truncate-lines t)
    ;; trim triling spaces on save
    (add-hook 'before-save-hook 'delete-trailing-whitespace))

  (add-hook 'prog-mode-hook #'my/whitespace-mode))

(use-package shrink-whitespace
  :ensure t

  :config
  (bind-key "M-\\" 'shrink-whitespace))

;;;; Smart commenting
(use-package comment-dwim-2
  :ensure t

  :bind
  ("M-;" . comment-dwim-2))

;;;; Indentation
;; electric indent
(defun my/disable-electric-indent-mode ()
  (electric-indent-mode -1))

(add-hook 'after-change-major-mode-hook #'my/disable-electric-indent-mode)

;; autoindent (must be loader before yasnippet!)
(use-package auto-indent-mode
  :ensure t

  :diminish auto-indent-mode

  :config
  (setq auto-indent-key-for-end-of-line-then-newline "<M-return>"
        auto-indent-blank-lines-on-move nil
        auto-indent-newline-function 'newline-and-indent
        auto-indent-on-yank-or-paste nil
        auto-indent-indent-style 'conservative))

(use-package aggressive-indent
  :ensure t

  :diminish aggressive-indent-mode

  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;;;; Expand Region
(use-package expand-region
  :ensure t

  :bind
  ("M-]" . er/expand-region)
  ("M-[" . er/contract-region))

;;;; Case formatting
(use-package caseformat
  :ensure t

  :bind
  ("M-L" . caseformat-backward))

;;;; Undo tree
(use-package undo-tree
  :ensure t

  :diminish undo-tree-mode

  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `((".*" . ,(format "%sundo-tree-history" user-emacs-directory)))))

;;;; SmartParens
(use-package smartparens
  :ensure t

  :diminish (smartparens-mode . "🄢")

  :hook
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
  :ensure t

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
  :ensure t

  :after (multiple-cursors))

;;;; Spell Checking
(when (executable-find "hunspell")
  (progn
    (setq ispell-program-name "hunspell")
    (eval-after-load "ispell"
      '(progn (defun ispell-get-coding-system () 'utf-8)))))

;;;; Vim'ish folding
(use-package vimish-fold
  :ensure t

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

;;; Navigation
;;;; Avy/Ace/Eno
(use-package avy
  :ensure t

  :bind
  ("M-SPC" . avy-goto-char)

  (:map
   isearch-mode-map
   ("M-SPC" . avy-isearch))

  (:map
   search-map
   ("M-SPC" . avy-isearch))

  (:prefix
   "M-g SPC"
   :prefix-map my/avy-map
   :menu-name "Avy"
   ("w" . avy-goto-word-or-subword-1)
   ("l" . avy-goto-line))

  :config
  (let ((sfa '(lambda (fc bg)
                (set-face-attribute
                 fc nil :background bg :foreground "white"))))
    (funcall sfa 'avy-lead-face "chocolate4")
    (funcall sfa 'avy-lead-face-0 "chocolate3")
    (funcall sfa 'avy-lead-face-2 "chocolate2")))

(use-package avy-zap
  :ensure t

  :after (avy)

  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim)

  (:map
   my/avy-map
   ("z" . avy-zap-to-char)
   ("Z" . avy-zap-up-to-char)))

(use-package ace-link
  :ensure t

  :bind
  ("M-g l" . ace-link)

  :config
  (ace-link-setup-default))

(use-package eno
  :ensure t

  :init
  (define-prefix-command 'my/eno/goto 'my/eno/goto "Eno:GoTo")
  (define-prefix-command 'my/eno/copy 'my/eno/copy "Eno:Copy")
  (define-prefix-command 'my/eno/cut 'my/eno/cut "Eno:Cut")
  (define-prefix-command 'my/eno/paste 'my/eno/paste "Eno:Paste")

  (bind-keys
   :prefix "C-c e"
   :prefix-map my/eno-map
   ("g" . my/eno/goto)
   ("M-w" . my/eno/copy)
   ("C-w" . my/eno/cut)
   ("C-y" . my/eno/paste))

  :bind
  (:map
   my/eno/goto
   ("w" . eno-word-goto)
   ("l" . eno-line-goto)
   ("'" . eno-str-goto)
   ("[" . eno-paren-goto)
   ("s" . eno-symbol-goto))

  (:map
   my/eno/copy
   ("w" . eno-word-copy)
   ("l" . eno-line-copy)
   ("t" . eno-line-copy-to)
   ("f" . eno-line-copy-from-to)
   ("'" . eno-str-copy)
   ("[" . eno-paren-copy)
   ("s" . eno-symbol-copy)
   ("S" . eno-symbol-copy-to)
   ("M-s" . eno-symbol-copy-from-to))

  (:map
   my/eno/cut
   ("w" . eno-word-cut)
   ("l" . eno-line-cut)
   ("t" . eno-line-cut-to)
   ("f" . eno-line-cut-from-to)
   ("'" . eno-str-cut)
   ("[" . eno-paren-cut)
   ("s" . eno-symbol-cut)
   ("S" . eno-symbol-cut-to)
   ("M-s" . eno-symbol-cut-from-to))

  (:map
   my/eno/paste
   ("w" . eno-word-paste)
   ("l" . eno-line-paste)
   ("t" . eno-line-paste-to)
   ("f" . eno-line-paste-from-to)
   ("'" . eno-str-paste)
   ("[" . eno-paren-paste)
   ("s" . eno-symbol-paste)
   ("S" . eno-symbol-paste-to)
   ("M-s" . eno-symbol-paste-from-to)))

;;;; Swiper
(use-package swiper
  :ensure t

  :bind
  ("M-s s" . swiper)

  (:map
   swiper-map
   ("M-SPC" . swiper-avy))

  :custom-face
  (swiper-match-face-1 ((t (:box (:line-width 1 :color "dark orange")))))
  (swiper-match-face-2 ((t (:box (:line-width 1 :color "orange")))))
  (swiper-match-face-3 ((t (:box (:line-width 1 :color "gold")))))
  (swiper-match-face-4 ((t (:box (:line-width 1 :color "yellow"))))))

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
(defun my/elisp-mode-hook ()
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-hook)

;;;; Clojure
(use-package clojure-mode
  :ensure t

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
  :ensure t

  :after (clojure-mode))

(use-package cider
  :ensure t

  :after (clojure-mode)

  :hook
  (cider-repl-mode . rainbow-delimiters-mode)
  (cider-mode . rainbow-delimiters-mode)

  :config
  (setq cider-repl-result-prefix ";; => "))

;;;; Haskell
(use-package haskell-mode
  :ensure t

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

  :config
  (defun my/haskell-yesod-parse-routes-mode-hook ()
    ;; Disables the line wrapping and auto-fill-mode
    (toggle-truncate-lines t))

  (add-hook 'haskell-yesod-parse-routes-mode-hook
            #'my/haskell-yesod-parse-routes-mode-hook)

  (defun my/hack-locals-for-haskell ()
    (when my/use-intero
      (intero-mode))
    (flycheck-mode))

  (defun my/boot-haskell ()
    "Initialize haskell stuff"
    (interactive)
    (haskell-decl-scan-mode)
    ;; case sensitive tags
    (setq tags-case-fold-search nil)
    ;; docs
    (eldoc-mode t)
    ;; auto-indentation
    (hi2-mode t)
    (auto-indent-mode -1)
    (setq indent-line-function (lambda () 'noindent))
    ;; configure flycheck
    (add-hook 'hack-local-variables-hook
              #'my/hack-locals-for-haskell
              nil t))
  (add-hook 'haskell-mode-hook 'my/boot-haskell)

  ;; hemmet
  (defun my/hemmet-expand-region ()
    (interactive)
    (when (executable-find "hemmet")
      (let ((f (lambda (b e)
                 (shell-command-on-region
                  b e "hemmet bem react-flux" t t "*hemmet error*" t))))
        (if (region-active-p)
            (funcall f (region-beginning) (region-end))
          (funcall f (line-beginning-position) (line-end-position))))))

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
               (insert l2) (newline))))))))

  ;; Make flycheck to use path to .cabal-file if local var was set
  (defun my/override-flycheck-haskell-default-directory (proc &rest args)
    (if my/flycheck-haskell-prefer-cabal-path
        (funcall proc 'haskell-ghc)
      (apply proc args)))
  (advice-add 'flycheck-haskell--find-default-directory
              :around
              #'my/override-flycheck-haskell-default-directory))

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

(defvar my/flycheck-haskell-prefer-cabal-path nil
  "If 't' then flycheck will start checker at dir with .cabal-file")
(put 'my/flycheck-haskell-prefer-cabal-path 'safe-local-variable #'booleanp)
(put 'flycheck-ghc-language-extensions   'safe-local-variable #'listp)
(put 'flycheck-hlint-language-extensions 'safe-local-variable #'listp)
(put 'haskell-stylish-on-save            'safe-local-variable #'booleanp)

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
    (setq whitespace-style '(face lines-tail trailing))
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

  :diminish company-mode

  :bind
  ("C-c /" . company-files)

  (:map
   company-mode-map
   ("M-<tab>" . company-complete))

  (:map
   company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))

  :config
  (global-company-mode t)

  (add-hook
   'prog-mode-hook
   (lambda ()
     "stops dabbrev to downcase variants"
     (setq company-dabbrev-downcase nil)))

  (setq
   company-tooltip-limit 20
   company-begin-commands '(self-insert-command)
   company-selection-wrap-around t
   company-show-numbers t
   company-tooltip-align-annotations t
   company-require-match nil
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil)

  )

(put 'company-backends 'safe-local-variable #'listp)

(bind-key "M-/" 'hippie-expand)

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
  (company-mode company-flx-mode))

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

;;;; Zeal-at-point
(use-package zeal-at-point
  :if (executable-find "zeal")

  :bind
  ("C-c d" . zeal-at-point))

;;;; Flycheck
(use-package flycheck
  :ensure t

  :diminish "Ⓕ"

  :bind
  ("<f5>" . flycheck-buffer)
  ("<f7>" . flycheck-previous-error)
  ("<f8>" . flycheck-next-error)

  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(put 'flycheck-checker 'safe-local-variable #'symbolp)

(use-package flycheck-color-mode-line
  :ensure t

  :after (flycheck)

  :hook
  (flycheck-mode . flycheck-color-mode-line-mode))

(use-package helm-flycheck
  :ensure t

  :after (flycheck)

  :bind
  ("<f6>" . helm-flycheck)

  (:map
   flycheck-command-map
   ("l" . helm-flycheck)
   ("L" . flycheck-list-errors)))

;;;; Yasnippet
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

  :ensure t

  :config
  (setq ag-highlight-search t))

;; install from github
(use-package ripgrep
  :if (executable-find "rg")

  :ensure t)

;;;; Projectile
(use-package projectile
  :ensure t

  :custom
  (projectile-keymap-prefix (kbd "C-c p"))

  :config
  (projectile-mode)

  (setq projectile-mode-line
        '(:eval (format "[%s]" (projectile-project-name)))))

(put 'projectile-tags-file-name 'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-files 'safe-local-variable #'listp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)

(use-package helm-projectile
  :ensure t

  :after (projectile)

  :config
  (helm-projectile-on))

(use-package projectile-ripgrep
  :if (package-installed-p 'ripgrep)

  :ensure t

  :after (projectile)

  :bind
  (:map
   projectile-command-map
   ("s r" . projectile-ripgrep)))

;;;; Git/Magit
(use-package magit
  :ensure t

  :bind
  ("C-x g" . magit-status))

(use-package git-timemachine
  :ensure t

  :commands (git-timemachine))

;;;; YankPad
(use-package yankpad
  :ensure t

  :init
  (setq yankpad-file "~/Dropbox/org/yankpad.org")

  :bind
  (:map
   my/yas-map
   ("m" . yankpad-map)
   ("y" . yankpad-insert)))

;;;; Terminal here
(use-package terminal-here
  :ensure t

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

  :bind
  (:map
   outline-minor-mode-map
   ("M-i" . outline-cycle))

  :hook
  (outline-minor-mode . outshine-hook-function)
  (prog-mode . outline-minor-mode)

  :config
  (setq outshine-preserve-delimiter-whitespace t)
  (unbind-key "C-M-i" outline-minor-mode-map))

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

;;;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after this line emacs writes all the stuff itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo ((t (:bold t :background "#073642"))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.0))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :height 1.0))))
 '(org-tag ((t (:weight normal :height 0.8))))
 '(swiper-match-face-1 ((t (:box (:line-width 1 :color "dark orange")))))
 '(swiper-match-face-2 ((t (:box (:line-width 1 :color "orange")))))
 '(swiper-match-face-3 ((t (:box (:line-width 1 :color "gold")))))
 '(swiper-match-face-4 ((t (:box (:line-width 1 :color "yellow")))))
 '(vimish-fold-mouse-face ((t (:box (:line-width 1 :color "yellow")))))
 '(vimish-fold-overlay ((t (:box (:line-width 1 :color "dim gray")))))
 '(whitespace-line ((t (:background "moccasin" :underline (:color foreground-color :style wave)))))
 '(whitespace-tab ((t (:foreground "brown" :inverse-video nil :underline t)))))

;;;; Place for Emacs to append to
(put 'narrow-to-region 'disabled nil)
