(setq inhibit-compacting-font-caches t)
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
(server-start)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)

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
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))
(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package minimap
:commands minimap-mode
:config
(setq minimap-window-location 'right)
(custom-set-faces
 '(minimap-active-region-background ((t (:extend t :background "#11111b")))))
)

(use-package beacon
:init (beacon-mode 1))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(delete-selection-mode t)

(electric-pair-mode t)

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
                    :font "Ubuntu Nerd Font"
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
  "b i"   '(ibuffer :which-key "iBuffer")
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
  "f p" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Open Emacs config")

  "h"     '(:ignore t :wk "Help")
  "h t"   '(counsel-load-theme :which-key "Change Theme")
  "h r r"     '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :which-key "Reload emacs config")
  "h f" '(counsel-describe-function :wk "Describe function")
  "h v" '(counsel-describe-variable :wk "Describe variable")

  "w"     '(:ignore t :wk "Windows")
  "w w"   '(evil-window-next :which-key "Switch to Next window")
  "w q"   '(evil-quit :which-key "Close current window")
  "w v"   '(evil-window-vsplit :which-key "Vertical split window")
  "w n"  '(evil-window-new :which-key "New window")
  "w s"   '(evil-window-split :which-key "Horizontal split window")
  "w h"  '(evil-window-left :which-key "Window left")
  "w j"  '(evil-window-down :which-key "Window down")
  "w k"  '(evil-window-up :which-key "Window up")
  "w l"  '(evil-window-right :which-key "Window right")

  "e" '(:ignore t :wk "Eshell/Evaluate")    
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e r" '(eval-region :wk "Evaluate elisp in Selection")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate and elisp expression")
  "e h" '(counsel-esh-history :which-key "Eshell history")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region")
  "e s" '(eshell :which-key "Eshell")

  "t" '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t m" '(minimap-mode :wk "Toggle Minimap")
  "t t" '(visual-line-mode :wk "Toggle truncated lines")
  "t p" '(treemacs :wk "Toggle Project Drawer")
  "t r" '(rainbow-mode :wk "Toggle Colors Preview")

  "o" '(:ignore t :wk "Open")
  "o p" '(treemacs :wk "Toggle Project Drawer")
  "o t" '(vterm-toggle :wk "Vterm")
  "o d" '(dired :wk "Dired")
  "o g" '(magit-status :wk "Magit")

  "."     '(find-file :which-key "Find File"))

(use-package vterm
:config
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

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
                          (projects . 5)
                          ))
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(use-package centaur-tabs
  :config
  ;; Available are alternate, bar, box, chamfer, rounded, slant, wave, zigzag
  (setq centaur-tabs-style "wave")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-show-new-tab-button t)
  ;; (setq centaur-tabs-plain-icons t)
  (centaur-tabs-mode t)
  :bind
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward))
  ("C-S-t" . centaur-tabs--create-new-tab)
  ("C-S-w" . centaur-tabs-do-close)
  ("C-<next>" . centaur-tabs-forward))

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

(use-package rainbow-mode)

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (fset 'projectile-command-map projectile-command-map)
  (sigma/leader-key
    "p" '(projectile-command-map :wk "Project")))

(with-eval-after-load 'lsp-mode
  (defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
(setq lsp-keymap-prefix "C-c l") 
    (lsp-headerline-breadcrumb-mode)))

(use-package lsp-mode
  :defer
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t)
  (fset 'lsp-command-map lsp-command-map)
  (sigma/leader-key
    "l" '(lsp-command-map :wk "Lsp"))
  )

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
  (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

;; (use-package tree-sitter
;;   :config
;;   ;; (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package flycheck
:hook (prog-mode . global-flycheck-mode))

(use-package python-mode
  :commands python-mode
  :hook (python-mode . lsp-deferred))

(use-package haskell-mode
  :commands haskell-mode
  :hook (haskell-mode . lsp-deferred))
(use-package lsp-haskell
 :hook
  ((haskell-mode . lsp)
   (haskell-literate-mode . lsp-deferred)))

(use-package rust-mode
  :commands rust-mode
  :config
  (setq rust-format-on-save t)
  :hook (rust-mode . lsp-deferred))
(use-package cargo-mode
:defer 
  :config
  (setq compilation-scroll-output t)
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package yasnippet
  :after company
  :init
  (yas-global-mode))

(use-package yasnippet-snippets)

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(setq gc-cons-threshold (* 2 1000 1000))
