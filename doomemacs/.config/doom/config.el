;;(add-to-list 'default-frame-alist '(alpha . (85 . 85)))
;;(setq doom-theme 'ewal-doom-vibrant)

(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
(setq doom-theme 'catppuccin)
(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      ;;doom-variable-pitch-font (font-spec :family "Ubuntu nerd" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "I'm The Same As You. I Didn't Have Any Other Choice.")
   (setq dashboard-startup-banner "~/.local/share/rice/pfp-medium-round.png")  ;; use custom image as banner
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          ))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq doom-fallback-buffer-name "*dashboard*")

(use-package org-bullets
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(after! org
   (setq org-directory "~/.local/src/org/"
         org-ellipsis " ▼ "
         ))
