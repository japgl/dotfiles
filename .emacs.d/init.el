;;; PACKAGE LOADING/MANAGEMENT SETTINGS
;;; Make sure to load this before any package settings!
;; Load local installations
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; (let ((default-directory "~/.emacs.d/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

;;;;;
;; Package manager
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-enable-at-startup nil)

(package-initialize)

;;;;;
;; Nice defaults
(setq inhibit-splash-screen nil
      initial-major-mode 'org-mode
      initial-scratch-message (format "%s\n" (shell-command-to-string "fortune"))
      large-file-warning-threshold nil
      locale-coding-system 'utf-8)

(menu-bar-mode 0)

(tool-bar-mode 0)

(scroll-bar-mode 0)

(global-visual-line-mode 1) ;; Line wrapping

(set-terminal-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)

(set-selection-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(when (fboundp 'windmove-default-keybindings) ; shift-arrow keys to move windows
  (windmove-default-keybindings))

(setq password-cache t ; enable password caching
      password-cache-expiry 3600) ; for one hour
;;;;;
;; System integration
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"));; ":/usr/local/texlive/2016/bin/universal-darwin")) ;set paths to run files

(setq exec-path (append exec-path '("/usr/local/bin"));; "/usr/local/texlive/2016/bin/universal-darwin")))
      backup-directory-alist `((".*" . ,temporary-file-directory)) ; Save backups to tmp dir
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))) ; Save autosaves to tmp dir

;;;;;
;; General functions
(defun open-init-file ()
  "Open main emacs configuration file."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun load-this-file ()
  "Load the current lisp file."
  (interactive)
  (save-buffer)
  (load-file buffer-file-name))

(defun run-this-file ()
  "Run the current executable file."
  (interactive)
  (save-buffer)
  (shell-command (concat "chmod +x " buffer-file-name))
  (shell-command buffer-file-name))

(defun indent-buffer ()
  "Indent this buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun random-theme ()
  "Select a random theme."
  (interactive)
  (customize-themes)
  (goto-line (+ (random (- (line-number-at-pos (point-max)) 12)) 12))
  (widget-button-press (point))
  (kill-buffer))

(defun cycle-windows ()
  (interactive)
  (other-window 1))

(defun save-and-kill ()
  "Check for modified buffers and kill emacs daemon."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun toggle-transparency ()
  "Turn transparency on or off."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 50) '(100 . 100)))))

;;(set-face-attribute 'default t :font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")

;;;;;
;; Global keybindings
(global-set-key (kbd "C-c c") 'comment-region)

(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "<C-tab>") 'cycle-windows)

(global-set-key (kbd "<RET>") 'newline-and-indent)

(global-set-key (kbd "<tab>") 'completion-at-point)

(global-set-key (kbd "s-i") 'indent-buffer)

(global-set-key (kbd "C-x M-i") 'open-init-file)

(global-set-key (kbd "C-x M-k") 'save-and-kill)

(global-set-key (kbd "C-x M-l") 'load-this-file)

(global-set-key (kbd "C-x M-r") 'run-this-file)

(global-set-key (kbd "C-c t") 'toggle-transparency)

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)

(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

(global-set-key (kbd "M-s") 'replace-string)

(global-set-key (kbd "M-q") 'kill-buffer-and-window)

;;;;;
;; Package config
(use-package paradox
  :bind (("C-x M-p" . paradox-list-packages)) ; global binding for package list

  :config
  (paradox-enable) ; use paradox instead of normal package interface
  (setq paradox-execute-asynchronously t)) ; always execute async, never ask

;; (use-package exwm
;;   :ensure t)

;; (use-package exwm-config
;;   :after exwm

;;   :config (exwm-config-default))

(use-package ispell
  :config
  (setenv "DICTIONARY" "en_AU")
  (setq ispell-program-name "hunspell"))

(use-package elisp-mode
  :bind (("C-c C-k" . load-this-file)))

(use-package eshell
  :bind (("C-!" . eshell-here)
         :eshell-mode-map
         ("<tab>" . completion-at-point))

  :config
  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))))

(use-package em-tramp
  :after eshell

  :config
  (setq eshell-prefer-lisp-functions t
        eshell-prefer-lisp-variables t))

(use-package doc-view
  :bind (:map doc-view-mode-map
              ("<C-mouse-4>" . doc-view-enlarge)
              ("<C-mouse-5>" . doc-view-shrink)
              ("g" . doc-view-goto-page))

  :config
  (setq doc-view-continuous nil
        doc-view-resolution 500) ; slow, high res. docs. 10x default value

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)) ; Make doc-view auto refresh

(use-package tex-mode
  :init
  (setq TeX-auto-save t
        TeX-parse-self t))

(use-package LilyPond-mode
  :bind (:map LilyPond-mode-map
              ("C-c p" . LilyPond-command-lilypond-and-view))

  :mode "\\.ly$"

  :config (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock))))

(use-package helm
  :ensure t

  :config (helm-mode 1))

(use-package helm-config
  :after helm

  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x r b" . helm-bookmarks)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         (("<tab>" . helm-execute-persistent-action)))

  :config
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t))


(use-package dired
  :bind (:map dired-mode-map
              ("<deletechar>" . dired-flag-file-deletion)
              ("e"   . dired-open)
              ("<mouse-2>" . dired-open)
              ("u"   . dired-up-directory)
              ("y"   . dired-copy-filename-as-kill)
              ("<left>" . dired-up-directory)
              ("<right>" . dired-open)

              ;; make home, end, Pup, Pdown work as on the buffer instead of text line
              ("<prev>" . scroll-up-command)
              ("<next>" . scroll-down-command)
              ("<home>" . beginning-of-buffer)
              ("<end>" . end-of-buffer)

              ;; some ranger-like keybinds
              ("h" . dired-up-directory)
              ("j" . diredp-next-line)
              ("k" . diredp-previous-line)
              ("l" . dired-open)

              ("'" . bookmark-bmenu-list)

              ("/"   . isearch-forward) ;;isearch-forward)
              ("P"   . image-dired)

              ("C"   . dired-rsync)

              ("M-o" . dired-omit-mode))

  :config
  (defun dired-open ()
    "Open files with the `xdg-open' command."
    (interactive)
    (let ((lawlist-filename (dired-get-file-for-visit)))
      (if (file-directory-p lawlist-filename)
          (dired-find-file)
        (start-process "default-app" nil "xdg-open" lawlist-filename))))

  (defun dired-rsync (dest)
    "Copy files using rsync."
    (interactive
     (list
      (expand-file-name
       (read-file-name
        "Rsync to:"
        (dired-dwim-target-directory)))))
    ;; store all selected files into "files" list
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          ;; the rsync command
          (tmtxt/rsync-command
           "rsync -arvz --progress "))
      ;; add all selected file names as arguments
      ;; to the rsync command
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      ;; append the destination
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      ;; run the async shell command
      (async-shell-command tmtxt/rsync-command "*rsync*")
      ;; finally, switch to that window
      (other-window 1)))

  (setq dired-recursive-copies (quote always) ; allow dired to operate on directories
        ;;        dired-listing-switches "alh" ; show file sizes as human readable values
        dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"
        dired-omit-mode t
        dired-dwim-target t) ; multi-pane-file-management

  (setq-default dired-omit-files-p 1)

  (defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough

  (add-hook 'dired-mode-hook 'hl-line-mode)

  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package dired+
  :after dired)

(use-package dired-details+ ; toggle file details
  :after dired

  :config (setq dired-details-hide-link-targets nil)) ; always show symlink targets

(use-package dired-x ; hide unimportant files
  :after dired)

(use-package dired-ranger
  :after dired

  :bind (:map dired-mode-map
              ("M-w" . dired-ranger-copy)
              ("C-w" . dired-ranger-move)
              ("C-y" . dired-ranger-paste)))

(use-package direx
  :bind (("C-x C-M-d" . direx:find-directory)
         ("C-x C-j" . direx:jump-to-directory)))

(use-package image-mode
  :bind (:map image-mode-map
              ("<up>" . image-previous-file)
              ("<left>" . image-previous-file)
              ("h" . image-previous-file)
              ("k" . image-previous-file)

              ("<right>" . image-next-file)
              ("<down>" . image-next-file)
              ("l" . image-next-file)
              ("j" . image-next-file)


              ("e" . image-open)
              ("f" . image-open)
              ("v" . image-open)
              ("o" . image-open))

  :config
  (defun image-open ()
    "Open the current buffer with `xdg-open'."
    (interactive)
    (start-process "default-app" nil "xdg-open" buffer-file-name))

  (defun show-image-dimensions-in-mode-line ()
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))

  (add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line))

(use-package magit
  :bind (("C-x M-g" . magit-status)))

(use-package org
  :preface
  (defun org-latex-export-to-pdf-async ()
    (interactive)
    (org-latex-export-to-pdf t))

  :bind (:map org-mode-map
              ("C-c a" . org-agenda)
              ("C-c g" . omlg-grab-link)
              ("C-c p" . org-latex-export-to-pdf-async ))

  :config
  (setq 

  (setq-default org-startup-indented t) ;; Start org mode in indented mode

  ;;  (setq org-hide-emphasis-markers t)    ;; Fix italics

  (font-lock-add-keywords 'org-mode     ;; Unicode bullet points
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;;  (require 'org-bullets)                ;; Better header bullets

  (setq org-hide-leading-stars t
        org-export-html-style-include-scripts nil
        org-export-html-style-include-default nil
        org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"org-style.css\" />"
	org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f") ; this should get bibtex files working
        org-agenda-files '("~/org/master.org")
        org-log-repeat "note"
        org-publish-project-alist '(("html"
                                     :base-directory "~/org/"
                                     :base-extension "org"
                                     :publishing-directory "~/org/exports"
                                     :publishing-function org-publish-org-to-html)
                                    ("pdf"
                                     :base-directory "~/org/"
                                     :base-extension "org"
                                     :publishing-directory "~/org/exports"
                                     :publishing-function org-publish-org-to-pdf)
                                    ("all" :components ("html" "pdf"))))

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(use-package org-ref
  :config (setq bibtex-dialect 'biblatex))

(use-package bibtex-mode
  :bind (:map bibtex-mode-map
              ("C-c d" . doi-utils-add-bibtex-entry-from-doi)))

(use-package define-word
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package ledger-mode
  :init (setq ledger-clear-whole-transactions 1)

  :mode "\\.dat\\'")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" "6ae174add87509daef7a844174f4f985592d70ea05c3d82377ad0a38a380ae80" "e654ce0507ae5b2d7feeaef2c07354206781527941e7feb178c0a94be4a98e90" "3d0142352ce19c860047ad7402546944f84c270e84ae479beddbc2608268e0e5" "a33858123d3d3ca10c03c657693881b9f8810c9e242a62f1ad6380adf57b031c" "a40eac965142a2057269f8b2abd546b71a0e58e733c6668a62b1ad1aa7669220" default)))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (magit pcomplete-extension exwm gruvbox-theme define-word wc-mode org-ref helm olivetti org-bullets dashboard spinner dired+ dired-details dired-details+ dired-hacks-utils dired-ranger helm-config notmuch use-package paradox)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
