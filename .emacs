;; increase GC-limit up to 100M for boot speedup
(setq gc-cons-threshold 100000000)

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
    (bifocal yaml-mode seq dired-subtree ace-link pocket-mode company-web company-cabal smex org-brain terminal-here emmet-mode web-mode counsel counsel-projectile ob-restclient zoom-window zeal-at-point yankpad window-numbering whole-line-or-region which-key volatile-highlights vimish-fold use-package unkillable-scratch undo-tree toml-mode switch-window swiper sr-speedbar solarized-theme smartparens shrink-whitespace rust-mode ripgrep rainbow-delimiters purescript-mode projectile org names markdown-mode magit lua-mode js2-mode intero idomenu ido-vertical-mode ido-ubiquitous ido-occur hindent hi2 guide-key git-timemachine ghc fullframe flycheck-rust flycheck-purescript flycheck-haskell flycheck-elm flycheck-color-mode-line flx-ido fireplace expand-region eno elpy elm-mode dumb-jump discover-my-major dired-single dired-hacks-utils dired-details+ company-restclient company-flx comment-dwim-2 clojure-mode-extra-font-locking clj-refactor caseformat beacon avy-zap auto-indent-mode align-cljlet aggressive-indent ag ace-mc)))
 '(safe-local-variable-values
   (quote
    ((flycheck-checker)
     (intero-targets)
     (create-lockfiles)
     (org-default-notes-file . "~/Projects/aviora/notes.org")
     (my/suppress-hindent . t)
     (my/suppress-intero . t)
     (hi2-where-post-offset . 2)
     (hi2-left-offset . 2)
     (hi2-layout-offset . 2)))))

;; just a shortcut
(defun my/configure ()
  "Opens user-init-file"
  (interactive)
  (find-file user-init-file))

;; Package menagement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; shared keymaps
(bind-keys
 :prefix "C-c y"
 :prefix-map my/yas-map)

;; Font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Global text scale ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(bind-keys
 ("C-x C-+" . (lambda () (interactive) (global-text-scale-adjust 1)))
 ("C-x C-=" . (lambda () (interactive) (global-text-scale-adjust 1)))
 ("C-x C--" . (lambda () (interactive) (global-text-scale-adjust -1)))
 ("C-x C-0" . (lambda () (interactive)
                (global-text-scale-adjust (- text-scale-mode-amount))
                (global-text-scale-mode -1))))

;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq indicate-empty-lines t)

;; window title
(setq frame-title-format "Emacs: %b")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; hl matching parenthesis
(show-paren-mode)

;; cursor position
(global-hl-line-mode t)
(line-number-mode t)
(column-number-mode t)

;; cursor
(blink-cursor-mode -1)
(setq cursor-type 'bar)

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode "§"))

;; theme
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-dark))

;; Behaviour ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; startup messages e.t.c
(setq initial-scratch-message ""
      inhibit-startup-screen t
      inhibit-startup-message t)

;; autosaves & backup
(setq auto-save-default nil

      auto-save-file-name-transforms
      `((".*" ,(format "%sauto-saves/\\2" user-emacs-directory) t))

      backup-directory-alist
      `((".*" . ,(format "%sbackups/" user-emacs-directory)))

      backup-by-copying t
      kept-new-versions 4
      kept-old-versions 0
      delete-old-versions t
      version-control t)

;; no TABs in source
(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; short "y/n" messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; TAGS
;;(setq tags-table-list '("~/.emacs.d/TAGS"))

;; SQL
(setq-default sql-dialect 'sql-postgres)

;; popup windows manupulation
(use-package popwin
  :ensure t
  :config
  (popwin-mode))

;; beacon (visualizes cursor position)
(use-package beacon
  :ensure t

  :diminish beacon-mode

  :bind
  (("<f11>" . beacon-blink))

  :config
  (beacon-mode 1)
  (setq beacon-color "#A06600"
        beacon-push-mark 35
        beacon-dont-blink-commands nil))

;; smart commenting
(use-package comment-dwim-2
  :ensure t
  :config
  (bind-key "M-;" 'comment-dwim-2))

(require 'uniquify)

;; No Shift-selection!
(setq shift-select-mode nil)

;; Exit confirmation
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
            kill-emacs-query-functions))

;; whitespaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face lines-tail trailing tab-mark)
        whitespace-line-column 80
        default-tab-width 4)

  (add-hook 'prog-mode-hook
            (lambda ()
              "whitespace mode for prog buffers"
              (setq require-final-newline t
                    next-line-add-newlines nil)
              (whitespace-mode t)
              (toggle-truncate-lines t)
              ;; trim triling spaces on save
              (add-hook 'before-save-hook 'delete-trailing-whitespace))))

(use-package shrink-whitespace
  :ensure t
  :config
  (bind-key "M-\\" 'shrink-whitespace))

;; Scratch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package unkillable-scratch
  :ensure t
  :config
  ;;(add-to-list 'unkillable-scratch "\\*scratch\\*")
  (unkillable-scratch 1))

;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   ("<tab>" . dired-subtree-cycle)
   ))

;; fullframe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fullframe
  :ensure t

  :config
  (fullframe ibuffer ibuffer-quit)
  (fullframe list-packages quit-window)
  (fullframe package-list-packages quit-window))

;; whole line or region
(use-package whole-line-or-region
  :ensure t

  :diminish whole-line-or-region-mode

  :config
  (whole-line-or-region-mode t))

;; emacsclient support
(use-package server
  :ensure t

  :config
  (unless (server-running-p)
    (server-start)))

;; hl some actions like a killing, yanking, pasting e.t.c.
(use-package volatile-highlights
  :ensure t

  :diminish volatile-highlights-mode

  :config
  (volatile-highlights-mode 1))

;; autoindent (must be loader before yasnippet!) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-indent-mode
  :ensure t

  :diminish auto-indent-mode

  :config
  (setq auto-indent-key-for-end-of-line-then-newline "<M-return>"
        auto-indent-blank-lines-on-move nil
        auto-indent-newline-function 'newline-and-indent
        auto-indent-on-yank-or-paste nil
        auto-indent-indent-style 'conservative)
  ;; (auto-indent-global-mode)
  )

(use-package aggressive-indent
  :ensure t

  :diminish aggressive-indent-mode

  :config
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (aggressive-indent-mode t)
               (eldoc-mode))))

;; FLX + IDO + Smex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ido
  :ensure t

  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-use-virtual-buffers t
        ido-confirm-unique-completion t)

  (use-package ido-ubiquitous
    :ensure t

    :config
    (ido-ubiquitous-mode t))

  (use-package flx-ido
    :ensure t

    :config
    (flx-ido-mode t))

  (use-package ido-vertical-mode
    :ensure t

    :config
    (ido-vertical-mode t)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only))

  (use-package smex
    :ensure t

    :config
    (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
    (smex-initialize)
    (global-set-key [remap execute-extended-command] 'smex))

  (use-package idomenu
    :ensure t
    :bind
    (("M-g j" . idomenu))
    ;; :config
    ;; (setq imenu-auto-rescan t
    ;;       imenu-use-popup-menu nil)
    )

  (use-package ido-occur
    :ensure t

    :bind
    (("M-s o" . ido-occur)
     ("M-s O" . ido-occur-at-point))))

;; Autocomplete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t

  :diminish "Ⓒ"

  :bind
  (("C-c /" . company-files)

   :map
   company-mode-map
   ("M-<tab>" . company-complete)

   :map
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
   company-tooltip-align-annotations 't
   ;; company-idle-delay .3
   company-begin-commands '(self-insert-command)
   )

  (use-package company-flx
    :ensure t

    :config
    (add-hook 'company-mode-hook (lambda () (company-flx-mode +1))))
  )

;; Zeal-at-point
(use-package zeal-at-point
  :if (executable-find "zeal")

  :bind
  ("C-c d" . zeal-at-point)
  ;;(add-to-list 'zeal-at-point-mode-alist '(smth-mode . "smth")
  )

;; Avy & Eno ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t

  :init
  (bind-keys
   :prefix "M-g SPC"
   :prefix-map my/avy-map
   :menu-name "Avy")

  :bind
  (("M-SPC" . avy-goto-char)

   :map
   isearch-mode-map
   ("M-SPC" . avy-isearch)

   :map
   search-map
   ("M-SPC" . avy-isearch)

   :map
   my/avy-map
   ("w" . avy-goto-word-or-subword-1)
   ("l" . avy-goto-line)
   )

  :config
  (use-package avy-zap
    :ensure t

    :bind
    (("M-z" . avy-zap-to-char-dwim)
     ("M-Z" . avy-zap-up-to-char-dwim)

     :map
     my/avy-map
     ("z" . avy-zap-to-char)
     ("Z" . avy-zap-up-to-char)
     ))

  (let ((sfa '(lambda (fc bg)
                (set-face-attribute
                 fc nil :background bg :foreground "white"))))
    (funcall sfa 'avy-lead-face "chocolate4")
    (funcall sfa 'avy-lead-face-0 "chocolate3")
    (funcall sfa 'avy-lead-face-2 "chocolate2"))
  )

(use-package ace-link
  :ensure t

  :bind
  (("M-o" . ace-link))

  :config
  (ace-link-setup-default)
  )

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
   ("s" . eno-symbol-goto)

   :map
   my/eno/copy
   ("w" . eno-word-copy)
   ("l" . eno-line-copy)
   ("t" . eno-line-copy-to)
   ("f" . eno-line-copy-from-to)
   ("'" . eno-str-copy)
   ("[" . eno-paren-copy)
   ("s" . eno-symbol-copy)
   ("S" . eno-symbol-copy-to)
   ("M-s" . eno-symbol-copy-from-to)

   :map
   my/eno/cut
   ("w" . eno-word-cut)
   ("l" . eno-line-cut)
   ("t" . eno-line-cut-to)
   ("f" . eno-line-cut-from-to)
   ("'" . eno-str-cut)
   ("[" . eno-paren-cut)
   ("s" . eno-symbol-cut)
   ("S" . eno-symbol-cut-to)
   ("M-s" . eno-symbol-cut-from-to)

   :map
   my/eno/paste
   ("w" . eno-word-paste)
   ("l" . eno-line-paste)
   ("t" . eno-line-paste-to)
   ("f" . eno-line-paste-from-to)
   ("'" . eno-str-paste)
   ("[" . eno-paren-paste)
   ("s" . eno-symbol-paste)
   ("S" . eno-symbol-paste-to)
   ("M-s" . eno-symbol-paste-from-to)
   )
  )

;; swiper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swiper
  :ensure t

  :bind
  (("M-s s" . swiper)

   :map
   swiper-map
   ("C-c c" . swiper-mc)
   )
  )

;; Key help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t

  :diminish which-key-mode

  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25)
  (which-key-mode)
  )

;; Expand Region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :ensure t

  :bind
  (("M-]" . er/expand-region)
   ("M-[" . er/contract-region)))

;; Rainbow delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t

  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Case formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package caseformat
  :ensure t

  :bind
  ("M-L" . caseformat-backward))

;; Flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :ensure t

  :diminish "Ⓕ"

  :config
  (use-package flycheck-color-mode-line
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Clojure-mode + CIDER + align-cljlet + ac-nrepl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package clojure-mode
  :ensure t

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
    (context 2))

  (use-package clojure-mode-extra-font-locking
    :ensure t)

  ;; TODO: remove when clj-refactor will be fixed!
  (use-package seq
    :ensure t)

  (use-package cider
    :ensure t
    :config
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
    (setq cider-repl-result-prefix ";; => "))

  (use-package clj-refactor
    :ensure t
    :config
    (add-hook 'clojure-mode-hook
              (lambda ()
                (clj-refactor-mode 1)
                (cljr-add-keybindings-with-prefix "C-c C-r"))))

  (add-hook 'clojure-mode-hook (lambda () (aggressive-indent-mode 1))))

;; haskell mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode
  :ensure t

  :diminish haskell-mode

  :init
  (add-to-list
   'auto-mode-alist
   '("routes\\'" . haskell-yesod-parse-routes-mode))
  (bind-keys
   :map haskell-mode-map
   :prefix "C-c h"
   :prefix-map my/haskell-map)

  :bind
  (:map
   my/haskell-map
   ("v" . haskell-cabal-visit-file)
   ("m" . haskell-auto-insert-module-template)
   ("I" . haskell-sort-imports)
   ;; ("h" . my/hemmet-expand-region)
   ("y" . haskell-hayoo)
   )

  :config
  (add-hook
   'haskell-mode-hook
   (lambda ()
     "Declaration scanning hook"
     (haskell-decl-scan-mode)))

  (add-hook
   'haskell-yesod-parse-routes-mode-hook
   (lambda ()
     "Disables the line wrapping and auto-fill-mode"
     (toggle-truncate-lines t)
     (yas-minor-mode nil) ;; TODO: make possible to disable only autofill-mode
     ))

  (use-package hi2
    :ensure t

    :diminish hi2-mode

    :config
    (bind-key "<tab>" 'hi2-indent-line hi2-mode-map))

  (use-package intero
    :ensure t

    :diminish intero-mode

    :bind
    (:map
     my/haskell-map
     ("i r" . intero-restart)
     ("i t" . intero-targets))

    :config
    (unbind-key "M-." intero-mode-map)
    (defvar my/suppress-intero nil "Suppresses an intero-mode")
    (add-hook 'haskell-mode-hook
              (lambda ()
                (add-hook 'hack-local-variables-hook
                          (lambda ()
                            (if my/suppress-intero
                                (setq flycheck-checker 'haskell-stack-ghc)
                              (intero-mode))
                            (flycheck-mode))
                          nil t))))

  (use-package hindent
    :if (file-exists-p "~/.software/hindent/elisp")
    :load-path "~/.software/hindent/elisp"

    :ensure t

    :config
    (defvar my/suppress-hindent nil "Suppresses an autofromatting on save")
    (add-hook 'haskell-mode-hook
              (lambda ()
                (add-hook 'hack-local-variables-hook
                          (lambda ()
                            (when (not my/suppress-hindent)
                              (setq hindent-reformat-buffer-on-save t)
                              (hindent-mode)))
                          nil t))))

  (use-package company-cabal
    :ensure t

    :config
    (add-to-list 'company-backends 'company-cabal))

  (defun my/boot-haskell ()
    "Initialize haskell stuff"
    (interactive)

    ;; auto-indentation
    (hi2-mode)
    (auto-indent-mode -1)
    (setq indent-line-function (lambda () 'noindent)
          electric-indent-inhibit 1)
    )

  (setq hi2-layout-offset 4
        hi2-left-offset 4
        hi2-where-post-offset 2)

  (add-hook 'haskell-mode-hook 'my/boot-haskell)

  ;; hemmet
  (defun my/hemmet-expand-region ()
    (interactive)
    (when (executable-find "hemmet")
      (let ((f (lambda (b e)
                 (shell-command-on-region
                  b e "hemmet bem" t t "*hemmet error*" t))))
        (if (region-active-p)
            (funcall f (region-beginning) (region-end))
          (funcall f (line-beginning-position) (line-end-position)))
        )))

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
            (loop for m in methods do
                  (let* ((name (concat (downcase m) rname))
                         (l1 (concat name " :: Handler TypedContent"))
                         (l2 (concat name " = error \"" name " not implemented\"")))
                    (end-of-line)
                    (newline)
                    (insert l1) (newline)
                    (insert l2) (newline))))))))
  )

;; Python mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
  :ensure t

  :mode ("\\.py\\'" . python-mode)

  :commands (my/boot-python)

  :init
  (add-hook 'python-mode-hook 'my/boot-python)

  :config
  (use-package elpy
    :ensure t

    :diminish elpy-mode

    :config
    (set-variable
     'elpy-modules
     '(elpy-module-company
       elpy-module-eldoc
       ;;elpy-module-flymake
       elpy-module-pyvenv
       elpy-module-yasnippet
       elpy-module-sane-defaults))

    (bind-key "M-g j" 'elpy-menu python-mode-map)
    (elpy-enable)
    )

  (defun my/python-enforce-indentation ()
    "Enforces python indentation to 4 spaces"
    (interactive)
    (auto-indent-mode -1)
    (setq indent-tabs-mode nil
          python-indent-offset 4
          electric-indent-inhibit t
          python-indent-guess-indent-offset nil)
    )

  (defun my/boot-python ()
    "Initialize python stuff"
    (interactive)
    (my/python-enforce-indentation)
    (flycheck-mode)
    (flycheck-select-checker 'python-pylint)
    )
  )

;; Rust ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t

  :mode "\\.rs\\'"

  :commands (rust-mode)

  :config
  (use-package toml-mode
    :ensure t)

  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'rust-mode-hook
              (lambda ()
                (flycheck-rust-setup)
                (flycheck-mode))))

  (add-to-list 'company-dabbrev-code-modes 'rust-mode)
  (add-to-list 'company-keywords-alist (cons 'rust-mode rust-mode-keywords))
  ;; zeal docset advice
  (when (fboundp 'zeal-at-point-mode-alist)
    (add-to-list 'zeal-at-point-mode-alist '(rust-mode . "rust")))
  )

;; Markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t

  :mode "\\.md\\'")

;; window-number ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package window-numbering
  :ensure t

  :config
  (setq window-numbering-auto-assign-0-to-minibuffer t)
  (set-face-foreground 'window-numbering-face "#b58900")
  (set-face-bold-p     'window-numbering-face t)
  (window-numbering-mode))

(use-package switch-window
  :ensure t

  :commands
  (switch-window)

  :bind
  ("C-x o" . switch-window))

;; zoom window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zoom-window
  :ensure t

  :bind
  ("C-x C-z" . zoom-window-zoom)

  :config
  (setq zoom-window-mode-line-color "DarkGreen"))

;; undo tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t

  :diminish undo-tree-mode

  :config
  (global-undo-tree-mode))

;; Git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t

  :bind
  ("C-x g" . magit-status))

(use-package git-timemachine
  :ensure t

  :commands (git-timemachine))

;; Elm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elm-mode
  :ensure t

  :mode "\\.elm\\'"

  :config
  (use-package flycheck-elm
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook
              #'flycheck-elm-setup)
    (add-hook
     'elm-mode-hook
     #'flycheck-mode))

  (when (executable-find "elm-oracle")
    (add-hook
     'elm-mode-hook
     #'elm-oracle-setup-completion
     ))

  (add-hook
   'elm-mode-hook
   (lambda ()
     (setq electric-indent-inhibit t)))

  (bind-keys
   :map elm-mode-map
   ("TAB" . elm-indent-cycle)
   )
  (diminish 'elm-indent-mode)
  )

;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  ;; :ensure t

  :mode "\\.go\\'"

  :commands
  (go-mode)

  :config
  (add-hook
   'go-mode-hook
   (lambda ()
     (add-hook 'before-save-hook (lambda () (gofmt)) t t)
     (setq-local whitespace-style '(face lines-tail trailing))
     (flycheck-mode)
     )))

;; PureScript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package purescript-mode
  :ensure t

  :mode "\\.purs\\'"

  :commands
  (purescript-mode)

  :config
  (use-package flycheck-purescript
    :ensure t
    :config
    (eval-after-load 'flycheck
      '(flycheck-purescript-setup)))

  (defun my/boot-purescript ()
    "Initialize purs stuff"
    (auto-indent-mode -1)
    (electric-indent-mode -1)
    (setq purescript-indent-offset 2)
    (purescript-indent-mode)
    (flycheck-mode)
    )

  (add-hook 'purescript-mode-hook 'my/boot-purescript))

;; JavaScript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package js2-mode
  :ensure t

  :mode "\\.jsx?\\'"

  :commands
  (js2-jsx-mode)

  :bind
  (:map
   js2-jsx-mode-map
   ("<tab>" . js2-jsx-indent-line))

  :config
  (setq js2-basic-offset 2)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  (add-hook 'js2-jsx-mode-hook
            (lambda ()
              (setq electric-indent-inhibit t))))

;; SmartParens ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t

  :diminish smartparens-mode

  :commands
  (smartparens-strict-mode)

  :init
  (dolist
      (m '(emacs-lisp-mode-hook
           ielm-mode-hook
           lisp-mode-hook
           lisp-interaction-mode-hook
           scheme-mode-hook
           clojure-mode-hook
           cider-repl-mode-hook
           cider-mode-hook
           eval-expression-minibuffer-setup-hook))
    (add-hook m (lambda () (smartparens-strict-mode t))))

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
  (require 'smartparens-config))

;; Web Mode
(use-package web-mode
  :ensure t

  :commands
  (web-mode)

  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

  :config
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-script-padding 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 2)

  (use-package emmet-mode
    :ensure t

    :commands
    (emmet-expand-line)

    :bind
    (:map
     web-mode-map
     ("C-c C-j" . emmet-expand-line)))

  (use-package company-web
    :ensure t

    :init
    (add-to-list 'company-backends 'company-web-html)

    :commands
    (company-web-html))
  )

;; Yasnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t

  :defer 15

  :diminish (yas-minor-mode . "Ⓨ")

  :bind
  (:map
   my/yas-map
   ("<tab>" . company-yasnippet)

   :map
   yas-minor-mode-map
   ("C-c <tab>" . yas-expand)
   ("TAB" . nil))

  :config
  (yas-reload-all)
  (when (file-exists-p "~/.emacs.d/snippets")
    (add-to-list 'yas/snippet-dirs "~/.emacs.d/snippets"))
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'html-mode-hook 'yas-minor-mode)
  )

;; Projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t

  :config
  (projectile-global-mode 1)
  (setq projectile-mode-line
        '(:eval (format "[%s]" (projectile-project-name))))

  (defmacro add-alternative (target alternative)
    `(let ((fn (lambda  (old &rest _)
                 (interactive "P")
                 (if (and (projectile-project-p) (not current-prefix-arg))
                     (,alternative)
                   (funcall old)))))
       (advice-add (quote ,target) :around fn)))

  (add-alternative ido-find-file projectile-find-file)
  (add-alternative ido-switch-buffer projectile-switch-to-buffer)
  )

;; sudo apt-get install silversearcher-ag
(use-package ag
  :if (executable-find "ag")
  :ensure t

  :config
  (setq ag-highlight-search t))

;; install from github
(use-package ripgrep
  :if (executable-find "rg")
  :ensure t
  ;; :config
  ;; (bind-key "s r" 'projectile-ripgrep 'projectile-command-map)
  )

;; counsel
(use-package counsel-projectile
  :ensure t

  :bind
  (:map
   projectile-command-map
   ("SPC" . counsel-projectile)
   ("s S". counsel-projectile-ag))

  :config
  (add-to-list 'ivy-re-builders-alist '(t . ivy--regex-fuzzy)))

;; gotodef with dumb-jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dumb-jump
  :ensure t

  :bind
  (("C-c ." . dumb-jump-go)
   ("C-c ," . dumb-jump-back)))

;; Multiple Cursors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t

  :init
  (bind-keys
   :prefix "C-c m"
   :prefix-map my/mc-map)

  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M->" . mc/mark-next-word-like-this)
   ("C-M-<" . mc/mark-previous-word-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   )

  :bind
  (:map
   my/mc-map
   ("+" . mc/mark-all-like-this)
   ("r" . set-rectangular-region-anchor)
   ("c" . mc/edit-lines)
   ("e" . mc/edit-ends-of-lines)
   ("a" . mc/edit-beginnings-of-lines)
   ("SPC" . ace-mc-add-multiple-cursors))

  :config
  (use-package ace-mc
    :ensure t)
  )

;; Spell Checking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (executable-find "hunspell")
  (progn
    (setq ispell-program-name "hunspell")
    (eval-after-load "ispell"
      '(progn (defun ispell-get-coding-system () 'utf-8)))))

;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure org
  :pin org

  :mode ("\\.org\\'" . org-mode)

  :commands
  (org-mode
   org-capture)

  :bind
  (("C-c c" . org-capture)

   :map
   org-mode-map
   ("C-c M-RET" . org-insert-heading-after-current))

  :config
  (add-hook 'org-mode-hook 'yas-minor-mode)

  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE"))
        org-enforce-todo-dependencies t
        org-hide-leading-stars t
        ;; use ido for completion
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-html-postamble nil
        )

  (setq org-src-preserve-indentation t)

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

  (use-package org-brain
    :ensure t

    :commands
    (org-brain-visualize)

    :config
    (setq org-brain-path "~/Dropbox/org/brain"))

  (use-package ob-restclient
    :ensure t

    :commands
    (org-babel-execute:restclient)

    :config
    (use-package restclient
      :ensure t)

    (use-package company-restclient
      :ensure t
      :config
      (add-hook 'restclient-mode-hook
                (lambda ()
                  (add-to-list 'company-backends
                               'company-restclient))))

    (add-to-list 'my/org-babel-langs '(restclient . t))
    (my/org-babel-load-langs))

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
             (beginning-of-line 0))
           )))
      "* [[file:/%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%(haskell-guess-module-name-from-file-name (buffer-file-name (org-capture-get :original-buffer)))]]"
      :immediate-finish
      :kill-buffer))
   )
  )

;; Vim-like folding
(use-package vimish-fold
  :ensure t

  :init
  (bind-keys
   :prefix "C-c f"
   :prefix-map my/vimish-fold-map)

  :bind
  (:map
   my/vimish-fold-map
   ("l" . vimish-fold-avy)
   ("f" . vimish-fold)
   ("F" . vimish-fold-refold)
   ("u" . vimish-fold-unfold)
   ("U" . vimish-fold-unfold-all)
   ("d" . vimish-fold-delete)
   ("SPC" . vimish-fold-toggle)
   )

  :config
  (setq vimish-fold-blank-fold-header "<...>"
        vimish-fold-indication-mode 'left-fringe)

  (custom-set-faces
   '(vimish-fold-mouse-face ((t (:box (:line-width 1 :color "yellow")))))
   '(vimish-fold-overlay ((t (:box (:line-width 1 :color "dim gray"))))))
  )

;; PlantUML

;; (use-package puml-mode
;;   :config
;;   (add-to-list 'auto-mode-alist
;;                '("\\.puml\\'" . puml-mode)
;;                '("\\.plantuml\\'" . puml-mode)))

(use-package yaml-mode
  :ensure t

  :mode "\\.yaml\\'")

;; YankPad ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yankpad
  :ensure t

  :init
  (setq yankpad-file "~/Dropbox/org/yankpad.org")

  :bind
  (:map
   my/yas-map
   ("m" . yankpad-map)
   ("y" . yankpad-insert)))

;; terminal here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package terminal-here
  :ensure t

  :init
  (bind-keys
   :prefix "C-c T"
   :prefix-map my/terminal-here-map)

  :bind
  (:map
   my/terminal-here-map
   ("t" . terminal-here-launch)
   ("p" . terminal-here-project-launch)))

;; Pocket client ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pocket-mode
  :ensure t

  :commands
  (list-pocket))

;; Fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fireplace
  :commands
  (fireplace))

;; ===========================================================================
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually moves"
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; C-u C-SPC C-SPC  instead  C-u C-SPC C-u C-SPC
(setq set-mark-command-repeat-pop t)

;; smart BOL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line."
  (interactive "^p")
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;; xref tweaks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/do-then-quit (&rest args)
  (let ((win (selected-window)))
    (apply (car args) (rest args))
    (quit-window nil win)))

(advice-add #'xref-goto-xref :around #'my/do-then-quit)

;; ===========================================================================
;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys
 ("M-/" . hippie-expand)
 ("C-x C-b" . ibuffer))

(global-unset-key (kbd "C-z"))


;;(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")
(put 'dired-find-alternate-file 'disabled nil)

(setq resize-mini-windows t)

(setq calendar-week-start-day 1
      calendar-date-style 'european)

;; restore GC-limit after timeout
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after this line emacs writes all the stuff themself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(swiper-match-face-1 ((t (:background "OrangeRed4"))))
 '(swiper-match-face-2 ((t (:background "DarkOrange4"))))
 '(swiper-match-face-3 ((t (:background "orange4"))))
 '(swiper-match-face-4 ((t (:background "gold4"))))
 '(swiper-match-face1 ((t (:box (:line-width 1 :color "dark orange")))))
 '(swiper-match-face2 ((t (:box (:line-width 1 :color "orange")))))
 '(swiper-match-face3 ((t (:box (:line-width 1 :color "gold")))))
 '(swiper-match-face4 ((t (:box (:line-width 1 :color "yellow")))))
 '(vimish-fold-mouse-face ((t (:box (:line-width 1 :color "yellow")))))
 '(vimish-fold-overlay ((t (:box (:line-width 1 :color "dim gray")))))
 '(whitespace-line ((t (:background "OrangeRed4"))))
 '(whitespace-tab ((t (:foreground "brown" :inverse-video nil :underline t)))))

(put 'narrow-to-region 'disabled nil)
