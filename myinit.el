;;set where found plugginsy
(add-to-list 'load-path "~/emacs.git/lisp")

;; list the packages you want
(setq package-list '(package whitespace linum-relative ido paren auto-complete auto-complete-clang-async bs ibuffer))

(load "package")
(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; activate all the packages (in particular autoloads)
(package-initialize)


; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; set theme
(load-theme 'wombat)
;;custom name
(setq frame-title-format "PiMax")
;; turn off line wrapping
(setq-default truncate-lines 1)
;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-hour time format in mode-line
(display-time-mode             t) ;; show hours in mode-line
(size-indication-mode          t) ;; file size in percentages
;; small cursor
(set-default 'cursor-type '(hbar . 3))
;; set cursor color
(set-cursor-color "#ffffff")
;; hide cursor if window not have focus
(setq-default cursor-in-non-selected-windows nil)
;;hidi scrolbar
(menu-bar-mode -1)
;;hide toolbar
(tool-bar-mode -1)
;;hide scrollbar
(scroll-bar-mode -1)
;;sound bell off
(setq visible-bell 1)
;; Indent settings
(setq c-default-style "stroustrup") ;;indent style in c/c++ code
(setq-default indent-tabs-mode nil) ;;disable the ability to indent TAB
(setq-default tab-width          4) ;;tab width - 4 whitespace
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) ;;standard width of indent - 4 whitespace
;; Highlight search resaults
(setq search-highlight        t)
(setq query-replace-highlight t)



;;restore last session on startup
(desktop-save-mode 1)
;;delete selection
(delete-selection-mode t)
;;better buffer rendering
(setq redisplay-dont-pause t)
;; Clipboard settings
(setq x-select-enable-clipboard t)
;;set short confirm command
(fset 'yes-or-no-p 'y-or-n-p)



(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)
(global-whitespace-mode t)



;; show relative linum numbers
(require 'linum-relative)

;;cbf
(defun linum-relative-set-on ()
  (interactive)
  (message "Activated linum mode")
  (linum-relative-mode t)
  ;;(setq linum-relative-backend 'display-line-numbers-mode) ;;use v26 for smooth performance
  (setq linum-relative-current-symbol "") ;;set current line number
  (set-face-foreground 'linum "#008080") ;;linum relative foreground color
  (if (display-graphic-p) ;;set format
    (setq linum-relative-format " %3s")
    (setq linum-relative-format " %3s|")))

;;set hooks
(add-hook 'emacs-lisp-mode-hook 'linum-relative-set-on)
(add-hook 'c-mode-hook 'linum-relative-set-on)
(add-hook 'c++-mode-hook 'linum-relative-set-on)
(add-hook 'cc-mode-hook 'linum-relative-set-on)
(add-hook 'php-mode-hook 'linum-relative-set-on)
(add-hook 'python-mode-hook 'linum-relative-set-on)
(add-hook 'java-mode-hook 'linum-relative-set-on)
(add-hook 'perl-mode-hook 'linum-relative-set-on)
(add-hook 'shell-mode-hook 'linum-relative-set-on)
(add-hook 'sh-mode-hook 'linum-relative-set-on)
(add-hook 'sh-lisp-mode-hook 'linum-relative-set-on)
(add-hook 'xml-mode-hook 'linum-relative-set-on)
(add-hook 'css-mode-hook 'linum-relative-set-on)
(add-hook 'javascript-mode-hook 'linum-relative-set-on)
(add-hook 'makefile-mode-hook 'linum-relative-set-on)
(add-hook 'cmake-mode-hook 'linum-relative-set-on)



;;Interactively Do Things mode
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)



;;highlight blocks and quots
(require 'paren)
(show-paren-mode 1) ;; highlight quots
;;(setq show-paren-style 'expression) ;;highlight curent block


(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode)


(require 'auto-complete-clang-async)
;; (setq ac-clang-flags
;;   (mapcar (lambda (item)(concat "-I" item))
;;     (split-string "
;;       /cygdrive/d/projects/fcgi/src/include
;;       /usr/include
;;       /usr/i586-pc-msdosdjgpp/sys-include
;;       /usr/i686-pc-cygwin/sys-root/usr/include
;;       /usr/i686-w64-mingw32/sys-root/mingw/include
;;       /usr/x86_64-w64-mingw32/sys-root/mingw/include")))

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

;;autocomplete
;; (require 'ac-clang)
;; (require 'cpputils-cmake)
;; (global-auto-complete-mode t)
;; (add-hook 'c-mode-common-hook
;;   (lambda () (if (derived-mode-p 'c-mode 'c++-mode)
;;     (cppcm-reload-all))))
;; ;; OPTIONAL, somebody reported that they can use this package with Fortran
;; (add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; ;; OPTIONAL, avoid typing full path when starting gdb
;; (global-set-key (kbd "C-c C-g")
;;   '(lambda ()
;;     (interactive)
;;     (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
;; ;; OPTIONAL, some users need specify extra flags forwarded to compiler
;; (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;;select using modes
;;(add-to-list 'ac-modes 'sql-mode) ;;or ;;(setq ac-modes '(c++-mode sql-mode))

;;(ac-clang-server-type 'debug)
;;(setq ac-clang-clang-translation-unit-flags FLAG-STRING)
;;(setq ac-clang-clang-complete-at-flags FLAG-STRING)
;;(setq ac-clang-cflags CFLAGS)
;;(ac-clang-update-clang-parameters)

;;set server resident
;;(ac-clang-initialize)
;;activate after set CFLAGS
;;(ac-clang-activate-after-modify) ;; or
;;(ac-clang-activate)
;;(setq ac-clang-debug-log-buffer-p t)
;;(setq ac-clang-debug-log-buffer-size (* 1024 1000))
;;(setq ac-clang-debug-profiler-p t)



;;auto-close quotes and parentheses
;;(require 'autopair)
;;(autopair-global-mode 1)



;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;;separate list of buffers when clicked C-x C-b
;;(iswitchb-mode 1)
;; disable dired buffer
(put 'dired-find-alternate-file 'disabled nil) ;; hide dired buffer





;; show invalid whitespace
;;(global-set-key "\C-c_w" 'whitespace-mode)
;;(global-set-key "\C-c_t" 'whitespace-toggle-options)
;;(global-set-key "\C-c=w" 'global-whitespace-mode)
;;(global-set-key "\C-c=t" 'global-whitespace-toggle-options)

;; ;; Windows performance tweaks for irony-mode
;; (when (boundp 'w32-pipe-read-delay)
;;   (setq w32-pipe-read-delay 0))
;; ;; Set the buffer size to 64K on Windows (from the original 4K)
;; (when (boundp 'w32-pipe-buffer-size)
;;   (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
;;
;; ;; checker for the C, C++ and Objective-C
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; ;; company-mode
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; ;; irony-mode
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; ;; If you want to enable tab-completion with no delay use the following:
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (require 'company-irony-c-headers)
;;   ;; Load with `irony-mode` as a grouped backend
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends
;;      '(company-irony-c-headers company-irony)))

;; ;; company-irony
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (eval-after-load 'company
;;   '(progn
;;     '(add-to-list 'company-backends 'company-irony))

;; ;; Tab completion
;; (setq company-idle-delay 0)
;; ;; ==========================================
;; ;; (optional) bind TAB for indent-or-complete
;; ;; ==========================================
;; (defun irony--check-expansion ()
;;   (save-excursion (if (looking-at "\\_>") t (backward-char 1)
;;                       (if (looking-at "\\.") t (backward-char 1)
;;                           (if (looking-at "->") t nil)))))
;; (defun irony--indent-or-complete
;;   () "Indent or Complete" (interactive) (cond ((and (not (use-region-p)) (irony--check-expansion)) (message "complete")
;;                                                (company-complete-common)) (t (message "indent")
;;                                                                              (call-interactively 'c-indent-line-or-region))))
;; (defun irony-mode-keys () "Modify keymaps used by `irony-mode'."
;;   (local-set-key (kbd "TAB") 'irony--indent-or-complete) (local-set-key [tab] 'irony--indent-or-complete))
;; (add-hook 'c-mode-common-hook 'irony-mode-keys)

;; ;;;; company-mode
;; ;;(add-hook 'after-init-hook 'global-company-mode)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'myinit)
