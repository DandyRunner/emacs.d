;; [[file:~/.emacs.d/init.org::*Package%20sources][Package sources:1]]
(eval-when-compile
  (require 'package)
  (setq package-archives
	`(("melpa" . ,(concat "/home/ejansen/" ".elpa-mirror/melpa/"))
	  ("org"   . ,(concat "/home/ejansen/" ".elpa-mirror/org/"))
	  ("gnu"   . ,(concat "/home/ejansen/" ".elpa-mirror/gnu/")))))
;; Package sources:1 ends here

;; [[file:~/.emacs.d/init.org::*Use-package%20install][Use-package install:1]]
(eval-when-compile
  (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
    (setq package-enable-at-startup nil)          ; To prevent initializing twice
    (package-initialize)))

;; Setup `use-package'
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Should set before loading `use-package'
(eval-and-compile
  ;;Don't use (setq use-package-always-ensure t). It prevents customising build in packages.
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (setq use-package-verbose t)
  (setq use-package-minimum-reported-time 0.01)
  (require 'use-package))
;; Use-package install:1 ends here

;; [[file:~/.emacs.d/init.org::*Use-package%20configuration][Use-package configuration:1]]
(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package
  :ensure t)

;; diminish keyword
(use-package diminish
  :ensure t)

;; bind keyword
(use-package bind-key
  :ensure t)
;; Use-package configuration:1 ends here

;; [[file:~/.emacs.d/init.org::*Common][Common:1]]
(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :custom
  (scroll-step 1)
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (debug-on-quit nil))
;; Common:1 ends here

;; [[file:~/.emacs.d/init.org::*Other%20built%20in%20settings][Other built in settings:1]]
(use-package ibuffer
  :bind([remap list-buffers] . ibuffer))
;; Other built in settings:1 ends here

;; [[file:~/.emacs.d/init.org::*Highlighting][Highlighting:1]]
(use-package paren
  :config
  (show-paren-mode t))

(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :config (hes-mode))

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook prog-mode)
;; Highlighting:1 ends here

;; [[file:~/.emacs.d/init.org::*Files][Files:2]]
(use-package recentf
  :custom
  (recentf-auto-cleanup 30)
  :config
  (run-with-idle-timer 30 t 'recentf-save-list))
;; Files:2 ends here

;; [[file:~/.emacs.d/init.org::*Files][Files:3]]
(use-package cus-edit
  :custom
  ;;(custom-file null-device "Don't store customizations"))
  (custom-file (expand-file-name ".custom.el" user-emacs-directory) "Store customizations in seperate file"))

(when (file-exists-p custom-file)
  (load custom-file))
;; Files:3 ends here

;; [[file:~/.emacs.d/init.org::*Localization][Localization:1]]
(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8))
;; Localization:1 ends here

;; [[file:~/.emacs.d/init.org::*Fonts][Fonts:1]]
(use-package faces
  :defer 0.1
  :custom
  (face-font-family-alternatives '(("Source Code Pro")))
  :config
  (set-face-attribute 'default
                      nil
                      :family (caar face-font-family-alternatives)
                      :weight 'regular
                      :width 'semi-condensed
                      :height 120))
;; Fonts:1 ends here

;; [[file:~/.emacs.d/init.org::*GUI][GUI:1]]
(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tooltip
  :defer t
  :custom
  (tooltip-mode -1))
;; GUI:1 ends here

;; [[file:~/.emacs.d/init.org::*Theme][Theme:1]]
(use-package darktooth-theme
  :ensure t
  :demand t
  :config
  (load-theme 'darktooth))
;;(load-theme 'wombat)
;; Theme:1 ends here

;; [[file:~/.emacs.d/init.org::*Some%20fancy%20gadgets%20for%20graphics][Some fancy gadgets for graphics:1]]
(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

(use-package fancy-battery
  :ensure t
  :hook
  (after-init . fancy-battery-mode))

;;(use-package font-lock+
;;  :quelpa
;;  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0)
          (jabber-chat-mode all-the-icons-material "chat" :v-adjust 0.0)
          (jabber-roster-mode all-the-icons-material "contacts" :v-adjust 0.0)
          (telega-chat-mode all-the-icons-fileicon "telegram" :v-adjust 0.0
                            :face all-the-icons-blue-alt)
          (telega-root-mode all-the-icons-material "contacts" :v-adjust 0.0))))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (all-the-icons-ivy-setup))
;; Some fancy gadgets for graphics:1 ends here

;; [[file:~/.emacs.d/init.org::*Modeline][Modeline:1]]
(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon t))
;; Modeline:1 ends here

;; [[file:~/.emacs.d/init.org::*Dashboard][Dashboard:1]]
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5))))
;; Dashboard:1 ends here

;; [[file:~/.emacs.d/init.org::*Which-key][Which-key:1]]
(use-package which-key
  :ensure t
  :defer 3
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))
;; Which-key:1 ends here

;; [[file:~/.emacs.d/init.org::*Company%20mode][Company mode:1]]
(use-package company
  :defer 3
  :ensure t
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  (global-company-mode 1))
;; Company mode:1 ends here

;; [[file:~/.emacs.d/init.org::*Ivy][Ivy:1]]
(use-package ivy
  :demand t
  :diminish ivy-mode
  :custom
  ;; Displays the current and total number in the collection in the prompt
  (ivy-count-format "%d%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
  ;;Add recent files and bookmarks to the ivy-switch-buffer
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-r"   . ivy-resume))
  :bind
  (:map ivy-minibuffer-map
        ("<tab>" . ivy-alt-done)
        ;;("SPC"   . ivy-alt-done-or-space)
        ("C-d"   . ivy-done-or-delete-char)
        ("C-i"   . ivy-partial-or-done)
        ("C-r"   . ivy-previous-line-or-history)
        ("M-r"   . ivy-reverse-i-search))
  :bind
  (:map ivy-switch-buffer-map
        ("C-k"   . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))
;; Ivy:1 ends here

;; [[file:~/.emacs.d/init.org::*Counsel][Counsel:1]]
(use-package counsel
  :ensure t
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper)
   :map mode-specific-map
   :prefix-map counsel-prefix-map
   :prefix "c"
   ("a" . counsel-apropos)
   ("b" . counsel-bookmark)
   ("B" . counsel-bookmarked-directory)
   ("c w" . counsel-colors-web)
   ("c e" . counsel-colors-emacs)
   ("d" . counsel-dired-jump)
   ("f" . counsel-file-jump)
   ("F" . counsel-faces)
   ("g" . counsel-org-goto)
   ("h" . counsel-command-history)
   ("H" . counsel-minibuffer-history)
   ("i" . counsel-imenu)
   ("j" . counsel-find-symbol)
   ("l" . counsel-locate)
   ("L" . counsel-find-library)
   ("m" . counsel-mark-ring)
   ("o" . counsel-outline)
   ("O" . counsel-find-file-extern)
   ("p" . counsel-package)
   ("r" . counsel-recentf)
   ("s g" . counsel-grep)
   ("s r" . counsel-rg)
   ("s s" . counsel-ag)
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  :init
  (counsel-mode))
;; Counsel:1 ends here

;; [[file:~/.emacs.d/init.org::*Swiper][Swiper:1]]
(use-package swiper
  :ensure t
  :after ivy
  :bind
  ("C-s"  . swiper)
  :bind
  (:map swiper-map
        ("M-y" . yank)
        ("M-%" . swiper-query-replace)
        ("C-." . swiper-avy)
        ("M-c" . swiper-mc))
  :bind
  (:map isearch-mode-map
       ("C-o" . swiper-from-isearch)))
;; Swiper:1 ends here

;; [[file:~/.emacs.d/init.org::*Magit][Magit:1]]
(use-package magit
  :ensure t
  :hook (magit-mode  . hl-line-mode)
  :bind
  (("C-x g"  . magit-status)
   ("C-x G"  . magit-status-with-prefix)))
;; Magit:1 ends here
