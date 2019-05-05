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
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (setq use-package-verbose t)
  (setq use-package-minimum-reported-time 0.01)
  (require 'use-package))
;; Use-package install:1 ends here

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
  (hl-line-mode t "Higlight the current line")
  (debug-on-quit nil))
;; Common:1 ends here

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
(use-package doom-themes
  :hook
  (after-init . doom-themes-org-config)
  :config
  (doom-themes-org-config)
  (load-theme 'wombat))
;; Theme:1 ends here

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
  :defer 3
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))
;; Which-key:1 ends here

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
  :after ivy
  :demand t
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))
  :diminish
  :bind
  (("C-*"     . counsel-org-agenda-headlines)
   ("C-x C-f" . counsel-finf-file)
   ("C-c e l" . counsel-find-library)
   ("C-c e q" . counsel-set-variable)
   ("C-h f"   . counsel-describe-function)
   ("c-h v"   . counsel-describe-variable)
   ("C-x r b" . counsel-describe-bookmark)
   ("M-x"     . counsel-M-x)
   ("M-s f"   . counsel-file-jump)
   ("M-s j"   . counsel-dired-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file. ivy-sort-files-by-date)))
;; Counsel:1 ends here

;; [[file:~/.emacs.d/init.org::*Swiper][Swiper:1]]
(use-package swiper
  :ensure t
  :after ivy
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
  :bind
  (("C-x g"  . magit-status)
   ("C-x G"  . magit-status-with-prefix)))
;; Magit:1 ends here
