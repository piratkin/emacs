;; set proxy
(setq url-proxy-services
    '(("no_proxy" . "^\\(localhost\\|192\\.168\\.*\\|10.*\\)")
      ("http" . "192.168.2.7:3128")
      ("https" . "192.168.2.7:3128")))

(setq package-check-signature nil)

;;set where found plugginsy
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/custom")
;;(require ‘init)

(load "package")
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; set theme
(load-theme 'wombat)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

;; turn off line wrapping
(setq-default truncate-lines 1)

;;custom name
(setq frame-title-format "PiMax")

;;set short confirm command
(fset 'yes-or-no-p 'y-or-n-p)

;;show character position in string
(column-number-mode)

;;(setq-default indicate-empty-lines t) ;;no glyphs next to a line with a line number
;;(setq-default indicate-buffer-boundaries 'left) ;; indication only on the left

;; Linum plugin
(require 'linum)          ;; call Linum
(line-number-mode   t)    ;; show line number in mode-line
(global-linum-mode  t)    ;; show line numbers in all buffers
(column-number-mode t)    ;; show column number in mode-line
(setq linum-format " %d") ;; set the numbering format for strings

(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)

;;Interactively Do Things mode
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-hour time format in mode-line
(display-time-mode             t) ;; show hours in mode-line
(size-indication-mode          t) ;; file size in percentages

;;highlight blocks and quots
(require 'paren)
(show-paren-mode 1)
;;(setq show-paren-style 'expression)

;;auto-close quotes and parentheses
;;(require 'autopair)
;;(autopair-global-mode 1)

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;;separate list of buffers when clicked C-x C-b
;;(iswitchb-mode 1)

;; disable dired buffer
(put 'dired-find-alternate-file 'disabled nil)

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

;;delete selection
(delete-selection-mode t)

;;better buffer rendering
(setq redisplay-dont-pause t)

;; Clipboard settings
(setq x-select-enable-clipboard t)

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









;;(provide 'init)
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
