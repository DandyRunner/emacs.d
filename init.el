;; [[file:~/.emacs.d/init.org::*Directories][Directories:1]]
;; Set the emacs directory
(setq user-emacs-directory (file-name-directory user-init-file))
;; Set the home directory
(defvar user-home-directory (expand-file-name "~/"))
;; Directories:1 ends here

;; [[file:~/.emacs.d/init.org::*Package%20Sources][Package Sources:1]]
;; Increase the garbage collector memory to 500MB
(setq gc-cons-threshold (* 500 1024 1024))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

(require 'package)
(setq package-archives
      `(("melpa"        . ,(concat user-home-directory ".elpa-mirror/melpa/"))
        ("org"          . ,(concat user-home-directory ".elpa-mirror/org/"))
        ("melba-stable" . ,(concat user-home-directory ".elpa-mirror/melpa-stable/"))
        ("gnu"          . ,(concat user-home-directory ".elpa-mirror/gnu/"))))
;; Package Sources:1 ends here

;; [[file:~/.emacs.d/init.org::*Use-package%20install][Use-package install:1]]
(eval-when-compile
  (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
    (setq package-enable-at-startup nil)          ; To prevent initializing twice
    (package-initialize)))

;; Setup `use-package'
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key)))

;; Should set before loading `use-package'
(eval-and-compile
  ;;Don't use (setq use-package-always-ensure t). It prevents customising build in packages.
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (setq use-package-verbose t)
  (setq use-package-minimum-reported-time 0.01)
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))
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
  (custom-file (expand-file-name ".custom.el" user-emacs-directory) "Store customizations in seperate file"))

(when (file-exists-p custom-file)
  (load custom-file))
;; Files:3 ends here

;; [[file:~/.emacs.d/init.org::*Elmacro][Elmacro:1]]
(use-package macrostep
  :ensure t
  :bind
  (("C-c e m" . macrostep-expand)))
;; Elmacro:1 ends here

;; [[file:~/.emacs.d/init.org::*Expand%20Region][Expand Region:1]]
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)
   :map mode-specific-map
   :prefix-map region-prefix-map
   :prefix "r"
   ("(" . er/mark-inside-pairs)
   (")" . er/mark-outside-pairs)
   ("'" . er/mark-inside-quotes)
   ([34] . er/mark-outside-quotes) ; it's just a quotation mark
   ("o" . er/mark-org-parent)
   ("u" . er/mark-url)
   ("b" . er/mark-org-code-block)
   ("." . er/mark-method-call)
   (">" . er/mark-next-accessor)
   ("w" . er/mark-word)
   ("d" . er/mark-defun)
   ("e" . er/mark-email)
   ("," . er/mark-symbol)
   ("<" . er/mark-symbol-with-prefix)
   (";" . er/mark-comment)
   ("s" . er/mark-sentence)
   ("S" . er/mark-text-sentence)
   ("p" . er/mark-paragraph)
   ("P" . er/mark-text-paragraph)))
;; Expand Region:1 ends here

;; [[file:~/.emacs.d/init.org::*Expand%20Region][Expand Region:2]]
(use-package edit-indirect
  :ensure t
  :after expand-region
  :bind
  (:map region-prefix-map
        ("r" . edit-indirect-region)))
;; Expand Region:2 ends here

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

;; [[file:~/.emacs.d/init.org::*Theme][Theme:1]]
(load-theme 'wombat)
;; Theme:1 ends here

;; [[file:~/.emacs.d/init.org::*Dashboard][Dashboard:1]]
(use-package dashboard
  :ensure t
  :demand t
  :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Welcome to EMacs Erwin")
  (dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory))
  (dashboard-items '((recents  . 5)
                     (agenda . 10)
                     (bookmarks . 5)
                     (projects . 5)
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
  (global-company-mode 1)
  (use-package company-quickhelp
    :ensure t
    :defer t
    :custom
    (company-quickhelp-delay 3)
    :config
    (company-quickhelp-mode 1)))
;; Company mode:1 ends here

;; [[file:~/.emacs.d/init.org::*Helpful][Helpful:1]]
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h C" . helpful-command)
   ("C-h F" . helpful-function)))
;; Helpful:1 ends here

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
   ("C-x G"  . magit-status-with-prefix))
  :bind
  (:map mode-specific-map
        :prefix-map magit-prefix-map
        :prefix "m"
        (("a" . magit-stage-file) ; the closest analog to git add
         ("b" . magit-blame)
         ("B" . magit-branch)
         ("c" . magit-checkout)
         ("C" . magit-commit)
         ("d" . magit-diff)
         ("D" . magit-discard)
         ("f" . magit-fetch)
         ("g" . vc-git-grep)
         ("G" . magit-gitignore)
         ("i" . magit-init)
         ("l" . magit-log)
         ("m" . magit)
         ("M" . magit-merge)
         ("n" . magit-notes-edit)
         ("p" . magit-pull-branch)
         ("P" . magit-push-current)
         ("r" . magit-reset)
         ("R" . magit-rebase)
         ("s" . magit-status)
         ("S" . magit-stash)
         ("t" . magit-tag)
         ("T" . magit-tag-delete)
         ("u" . magit-unstage)
         ("U" . magit-update-index))))
;; Magit:1 ends here

;; [[file:~/.emacs.d/init.org::*IBuffer-vc][IBuffer-vc:1]]
(use-package ibuffer-vc
  :ensure t
  :config
  (define-ibuffer-column icon
    (:name "Icon" :inline t)
    (all-the-icons-ivy--icon-for-mode major-mode))
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))
;; IBuffer-vc:1 ends here

;; [[file:~/.emacs.d/init.org::*Git%20modes][Git modes:1]]
(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)
;; Git modes:1 ends here

;; [[file:~/.emacs.d/init.org::*Diff-hl][Diff-hl:1]]
(use-package diff-hl
  :ensure t
  :hook
  ((magit-post-refresh-hook . diff-hl-post-refresh)
   (prog-mode . diff-hl-margin-mode)
   (org-mode . diff-hl-margin-mode)
   (dired-mode . diff-hl-dired-mode)))
;; Diff-hl:1 ends here

;; [[file:~/.emacs.d/init.org::*Paredit][Paredit:1]]
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :commands (paredit-mode)
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode))
;; Paredit:1 ends here

;; [[file:~/.emacs.d/init.org::*Smart%20Commenting][Smart Commenting:1]]
(use-package smart-comment
  :ensure t
  :bind
  ("M-;" . smart-comment))
;; Smart Commenting:1 ends here

;; [[file:~/.emacs.d/init.org::*Projectile][Projectile:1]]
(use-package projectile
  :ensure t
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :custom
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :after counsel prokectile
  :config
  (counsel-projectile-mode))
;; Projectile:1 ends here

;; [[file:~/.emacs.d/init.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

(use-package avy-flycheck
  :defer t
  :config
  (avy-flycheck-setup))
;; Flycheck:1 ends here

;; [[file:~/.emacs.d/init.org::*Emacs%20Lisp][Emacs Lisp:1]]
;; Check if all parenthesis are in place after save
;; Places the pointer on the faulty line. Invaluable.
(use-package lisp
  :hook
  (after-save . check-parens))

;; Highlights defined Emacs Lisp symbols in source code.
;; Currently it recognizes Lisp function, built-in function, macro, face and variable names.
(use-package highlight-defined
  :ensure t
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

;; Highlight of Lisp quotes and quoted symbols
(use-package highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

;;Discovering elisp functions based on examples
(use-package suggest
  :ensure t
  :defer t)

;; Pretty symbols
(use-package ipretty
  :ensure t
  :config
  (ipretty-mode 1))

;; provides a flycheck checker for the metadata in Emacs Lisp files which are intended to be packages.
(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  (flycheck-package-setup))
;; Emacs Lisp:1 ends here

;; [[file:~/.emacs.d/init.org::*The%20End][The End:1]]
(setq dashboard-banner-logo-title (concat "Welcome to Emacs: " user-full-name ". Startup time: " (emacs-init-time)))
;; The End:1 ends here
