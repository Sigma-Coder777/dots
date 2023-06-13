(setq gc-cons-threshold (* 50 1000 1000))
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(set-default-coding-systems 'utf-8)
;; Disable Garbage
(setq make-backup-files nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/emacs/backups/" user-emacs-directory))))    

					; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)
(use-package no-littering)
(setq warning-minimum-level :emergency)
(server-mode)

(use-package undo-tree
  :after evil
  :init
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))
(use-package evil

  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-tree)
  (evil-mode))
(use-package evil-collection

  :after evil
  :config
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines)
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil)

(use-package all-the-icons)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)
(delete-selection-mode t)
;; Disable line numbers for some modes

(electric-pair-mode t)
(dolist (mode '(  dashboard-mode-hook
                  term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (char/ligature-re
         `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
           (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
           (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
           (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                               "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
                               "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
                               "</" "<*")
                           (+ "<"))))
           (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
           (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
           (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
           (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
           (?&  . ,(rx (+ "&")))
           (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
                               "|]" "|}" "|=")
                           (+ "|"))))
           (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
           (?+  . ,(rx (or "+>" (+ "+"))))
           (?\[ . ,(rx (or "[<" "[|")))
           (?\{ . ,(rx "{|"))
           (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                           (+ "#"))))
           (?\; . ,(rx (+ ";")))
           (?_  . ,(rx (or "_|_" "__")))
           (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  . ,(rx "$>"))
           (?^  . ,(rx "^="))
           (?\] . ,(rx "]#"))))
  (let ((char (car char/ligature-re))
        (ligature-re (cdr char/ligature-re)))
    (set-char-table-range composition-function-table char
                          `([,ligature-re 0 font-shape-gstring]))))

(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :height 90
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Ubuntu Mono Nerd Font"
                    :height 100
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrainsMono Nerd Font"
                    :height 90
                    :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font"))

(setq global-prettify-symbols-mode t)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config

  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)

(use-package general
  :after evil
  :config
  (general-evil-setup t))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))
;; it removes ^ in counsel

(use-package ivy-rich
  :init (ivy-rich-mode 1))
(use-package smex
  :defer
  :init
  (smex-initialize))

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(general-create-definer sigma/leader-key
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC" ;; set leader
  :global-prefix "M-SPC") ;; access leader in insert mode


(sigma/leader-key
  "/"     '(swiper :which-key "Swiper")
  "SPC"   '(counsel-M-x :which-key "M-x")
  "b"     '(:ignore t :wk "Buffer")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "b B"   '(ibuffer :which-key "iBuffer")
  "b b"   '(counsel-ibuffer :which-key "Switch Buffer")
  "b n"   '(next-buffer :which-key "Next Buffer")
  "b p"   '(previous-buffer :which-key "Previous Buffer")
  "m"     '(:ignore t :wk "Org")
  "m t"   '(org-shiftright :which-key "Cycle Todo/List-Style")
  "m d"   '(org-timestamp :which-key "Org Timestamp")
  "m o"   '(org-open-at-point :which-key "Org Open")
  "m /"   '(org-sparse-tree :which-key "Query Todos")
  "f"     '(:ignore t :wk "Files")
  "f s"   '(save-buffer :which-key "Save Current Buffer")
  "f r"   '(counsel-recentf :which-key "Save Current Buffer")
  "h"     '(:ignore t :wk "Settings")
  "h t"   '(counsel-load-theme :which-key "Change Theme")
  "h r r"     '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :which-key "Reload emacs config")
  "w"     '(:ignore t :wk "Windows")
  "w w"   '(evil-window-next :which-key "Switch to Next window")
  "w q"   '(evil-quit :which-key "Close current window")
  "w v"       '(evil-window-vsplit :which-key "Vertical split window")
  "w n"       '(evil-window-new :which-key "New window")
  "w s"   '(evil-window-split :which-key "Horizontal split window")
  "w h"        '(evil-window-left :which-key "Window left")
  "w j"  '(evil-window-down :which-key "Window down")
  "w k"  '(evil-window-up :which-key "Window up")
  "w l"  '(evil-window-right :which-key "Window right")
  "."     '(find-file :which-key "Find File"))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(use-package doom-modeline)
(doom-modeline-mode 1)
(use-package doom-themes

  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'catppuccin t))
;; For transparent Background
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(use-package dashboard
  :config      ;; tweak dashboard config before loading it
  (dashboard-setup-startup-hook)
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "I'm The Same As You. I Didn't Have Any Other Choice.")
  (setq dashboard-startup-banner "~/.local/share/rice/pfp-medium-round.png")  ;; use custom image as banner
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          ))
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(use-package org
  :defer
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-ellipsis " ▾"
        org-agenda-files  '("~/Documents/Habits.org")
        org-deadline-warning-days 3
        org-hide-emphasis-markers t)

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)
  (electric-indent-mode -1)
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#d20f39" :weight bold)) 
          ("DOING" . (:foreground "#a6e3a1" :weight bold))))
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-auto-tangle
  :defer t
  :config (setq org-auto-tangle-default t)
  :hook (org-mode . org-auto-tangle-mode))

(use-package magit
  :commands magit-status)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(with-eval-after-load 'lsp-mode
  (defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode)))

(use-package lsp-mode
  :defer
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t))
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp-mode)

(use-package company
  :defer
  :hook (lsp-mode . company-mode)
  (prog-mode . global-company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package python-mode
  :after lsp
  :hook (python-mode . lsp-deferred))

(use-package yasnippet
  :after company
  :init
  (yas-global-mode))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(setq gc-cons-threshold (* 2 1000 1000))
