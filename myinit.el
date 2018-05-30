;;set where found plugginsy
(add-to-list 'load-path "~/emacs.git/lisp")
;; list the packages you want
(setq package-list '(
    package
    magit
    whitespace
    linum-relative
    ;font-lock
    ;xcscope
    rtags
    highlight-numbers
    ido
    paren
    bs
    ibuffer
    buffer-move
    yasnippet
    yasnippet-snippets
    cl-lib
    json
    ;auto-complete
    ;auto-complete-c-headers
    company
    company-quickhelp
    company-c-headers
    company-rtags
    company-irony-c-headers
    company-irony
    irony
    irony-eldoc
    flycheck
    flycheck-pos-tip
    flycheck-irony
    flycheck-rtags))
;; setup repos
(load "package")
(require 'package)
(add-to-list 'package-archives '("elpa"  . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
;; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))










;;reverse-input-method RET russian-computer RET
;(defun reverse-input-method (input-method)
;  "Build the reverse mapping of single letters from INPUT-METHOD."
;  (interactive
;   (list (read-input-method-name "Use input method (default current): ")))
;  (if (and input-method (symbolp input-method))
;      (setq input-method (symbol-name input-method)))
;  (let ((current current-input-method)
;        (modifiers '(nil (control) (meta) (control meta))))
;    (when input-method
;      (activate-input-method input-method))
;    (when (and current-input-method quail-keyboard-layout)
;      (dolist (map (cdr (quail-map)))
;        (let* ((to (car map))
;               (from (quail-get-translation
;                      (cadr map) (char-to-string to) 1)))
;          (when (and (characterp from) (characterp to))
;            (dolist (mod modifiers)
;              (define-key local-function-key-map
;                (vector (append mod (list from)))
;                (vector (append mod (list to)))))))))
;    (when input-method
;      (activate-input-method current))))









;; set theme
(load-theme 'wombat)
;;custom name
(setq frame-title-format "GNU Emacs: %b")
;; turn off line wrapping
(setq-default truncate-lines 1)
;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-hour time format in mode-line
(display-time-mode t) ;; show hours in mode-line
(size-indication-mode t) ;; file size in percentages
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
(setq-default tab-width 4) ;;tab width - 4 whitespace
(setq-default c-basic-offset 4)
(setq-default standart-indent 4) ;;standard width of indent - 4 whitespace
;; highlight search resaults
(setq search-highlight t)
(setq query-replace-highlight t)
;;hide welcome screen
(setq inhibit-startup-screen t)
;;highlight the same words on the screen
;(add-hook 'prog-mode-hook #'semantic-idle-local-symbol-highlight-mode)
;;highlight-numbers
(add-hook 'prog-mode-hook #'highlight-numbers-mode)









;;
;;font-lock
;;
(require 'font-lock)
(global-font-lock-mode             t)
(setq font-lock-maximum-decoration t)









;;restore last session on startup
;;(desktop-save-mode 1)
;;delete selection
(delete-selection-mode t)
;;better buffer rendering
(setq redisplay-dont-pause t)
;;use system clipboard
(setq x-select-enable-clipboard t)
;;set short confirm command
(fset 'yes-or-no-p 'y-or-n-p)
;;
;;(setq ac-disable-faces nil)
;;set build command
(setq compile-command "cd ../build && cmake -G \"Unix Makefiles\" -DCMAKE_EXPORT_COMPILE_COMMANDS=YES .. && make")
;;auto complete quots ({},[],())
(electric-pair-mode 1)
;;move the cursor to a new window
(setq split-window-keep-point t)
;;automatically delete extra spaces and tabs at the end of lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;do not remember the password in the cache for gpg files
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)









(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)







(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)
(global-whitespace-mode t)









;;show relative linum numbers
(require 'linum-relative)
;;setup and turn on linum-relative mode
(defun my:linum-relative ()
    (interactive)
    (linum-relative-mode t)
    ;;(setq linum-relative-backend 'display-line-numbers-mode) ;;use v26 for smooth performance
    (setq linum-relative-current-symbol "") ;;set current line number
    (set-face-foreground 'linum "#008080") ;;linum relative foreground color
    (if (display-graphic-p) ;;set format
        (setq linum-relative-format " %3s")
        (setq linum-relative-format " %3s|")))
;;set hooks
(add-hook 'emacs-lisp-mode-hook 'my:linum-relative)
(add-hook 'c-mode-hook 'my:linum-relative)
(add-hook 'c++-mode-hook 'my:linum-relative)
;(add-hook 'java-mode-hook 'my:linum-relative)
;(add-hook 'objc-mode-hook 'my:linum-relative)
;(add-hook 'php-mode-hook 'my:linum-relative)
;(add-hook 'python-mode-hook 'my:linum-relative)
;(add-hook 'perl-mode-hook 'my:linum-relative)
;(add-hook 'shell-mode-hook 'my:linum-relative)
;(add-hook 'sh-mode-hook 'my:linum-relative)
;(add-hook 'sh-lisp-mode-hook 'my:linum-relative)
;(add-hook 'xml-mode-hook 'my:linum-relative)
;(add-hook 'css-mode-hook 'my:linum-relative)
;(add-hook 'javascript-mode-hook 'my:linum-relative)
;(add-hook 'makefile-mode-hook 'my:linum-relative)
;(add-hook 'cmake-mode-hook 'my:linum-relative)









;;Interactively Do Things mode
(require 'ido)
(ido-mode t)
(icomplete-mode t)
(ido-everywhere t)
(setq ido-vitrual-buffers t)
(setq ido-enable-flex-matching t)








;;highlight blocks and quots
(require 'paren)
(show-paren-mode 1) ;; highlight quots
;;highlight curent block
;(setq show-paren-style 'expression)








;;
;; buffer-move
;;
(require 'buffer-move)
(global-set-key (kbd "M-s <up>") 'buf-move-up)
(global-set-key (kbd "M-s <down>") 'buf-move-down)
(global-set-key (kbd "M-s <left>") 'buf-move-left)
(global-set-key (kbd "M-s <right>") 'buf-move-right)
;;(setq buffer-move-stay-after-swap t)
;;(setq buffer-move-behavior 'move)








;;
;;yasnippet
;;
(require 'yasnippet)
(require 'yasnippet-snippets)
;(setq yas-snippet-dirs '("~/emacs.git/snippets"))
(yas-global-mode 1)








;; start auto-complete with emacs
;(require 'auto-complete)
;; do default config for auto-complete
;(ac-config-default)

;(setq ac-auto-start nil)
;(setq ac-auto-show-menu nil)
;(setq ac-use-fuzzy t)
;(setq ac-ignore-case 'smart)

;;;let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
;(defun my:ac-c-header-init ()
;  (require 'auto-complete-c-headers)
;  (add-to-list 'ac-sources 'ac-source-c-headers)
;  (setq include-directories my:c-headers-path-system))
;;; now let's call this function from c/c++ hooks
;(my:ac-c-header-init)








;; ; turn on Semantic
;; (semantic-mode 1)
;; ; let's define a function which adds semantic as a suggestion backend to auto complete
;; ; and hook this function to c-mode-common-hook
;; (defun my:add-semantic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic)
;; )
;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; ;; turn on ede mode
;; (global-ede-mode 1)
;; ;; create a project for our program.
;; (ede-cpp-root-project "fcgi project" :file "/cygdrive/d/projects/fcgi/src/main.cpp" :include-path '(quote ("/include" "../include")))
;; ;; you can use system-include-path for setting up the system header file locations.
;; ;; turn on automatic reparsing of open buffers in semantic
;; ;;(global-semantic-idle-scheduler-mode 1)









;; Buffer Selection and ibuffer settings
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)








;;
;;irony
;;
(require 'json)
(require 'cl-lib)
(require 'irony)
;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
;;add path to search "compile_commands.json"
;(setq irony-cdb-search-directory-list (append '("../build")))
;; irony-mode hook that is called when irony is triggered
(defun my:irony-mode-hook ()
    "Custom irony mode hook to remap keys."
    (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
;;start irony mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'my:irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;binding autocomplete hotkey
(global-set-key (kbd "M-RET") 'company-complete)









;;
;;company
;;
(require 'company)
(require 'company-quickhelp)
(require 'company-keywords)
(setq company-idle-delay 0)
(setq company-auto-complete t)
(setq company-minimum-prefix-length 3)
(setq company-transformers '(company-sort-by-occurrence))
(add-hook 'after-init-hook 'global-company-mode)









;;
;;company-c-headers
;;
(require 'company-c-headers)
;;set path
(setq company-c-headers-path-user my:c-headers-path-user)
(setq company-c-headers-path-system my:c-headers-path-system)









;;
;;company-irony-c-headers
;;
(require 'company-irony-c-headers)









(require 'rtags)
(require 'company-rtags)
(setq rtags-completions-enabled t)
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
;;hook mode
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;;user functions
(defun my:rtags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
        (gtags-find-tag)))
(defun my:rtags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
        (gtags-find-rtag)))
;;binding keys
(global-set-key (kbd "M-.") 'my:rtags-find-symbol-at-point)
(global-set-key (kbd "M-,") 'my:rtags-find-references-at-point)
(global-set-key (kbd "C-;") 'rtags-location-stack-back)
(global-set-key (kbd "C-'") 'rtags-location-stack-forward)
;;if onunly c/c++ mode
;(define-key c-mode-base-map (kbd "M-.") 'my:rtags-find-symbol-at-point)
;(define-key c-mode-base-map (kbd "M-,") 'my:rtags-find-references-at-point)
;(define-key c-mode-base-map (kbd "C-;") 'rtags-location-stack-back)
;(define-key c-mode-base-map (kbd "C-'") 'rtags-location-stack-forward)
;;other imposible commands
;;'rtags-find-symbol-at-point
;;'rtags-find-references-at-point
;;'rtags-references-tree
;;'rtags-find-virtuals-at-point
;;'rtags-print-enum-value-at-point
;;'rtags-find-all-references-at-point
;;'rtags-cycle-through-diagnostics
;;'rtags-find-symbol
;;'rtags-find-references
;;'rtags-location-stack-back
;;'rtags-location-stack-forward
;;'rtags-diagnostics
;;'rtags-compile-file
;;'rtags-guess-function-at-point
;;'rtags-dependency-tree
;;'rtags-dependency-tree-all
;;'rtags-reparse-file
;;'rtags-preprocess-file
;;'rtags-rename-symbol
;;'rtags-symbol-info
;;'rtags-display-summary-as-message
;;'rtags-display-summary
;;'rtags-goto-offset
;;'rtags-find-file
;;'rtags-fixit
;;'rtags-copy-and-print-current-location
;;'rtags-apply-fixit-at-point
;;'rtags-show-rtags-buffer
;;'rtags-make-member
;;'rtags-imenu
;;'rtags-taglist
;;'rtags-print-class-hierarchy
;;'rtags-print-source-arguments
;;'rtags-find-functions-called-by-this-function
;;'rtags-list-results
;;'rtags-location-stack-visualize









;;
;;company-irony
;;
(require 'company-irony)
;;company-irony setup, c-header completions
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;;load backend
(eval-after-load 'company
    '(add-to-list 'company-backends '(
        company-keywords
        company-irony-c-headers
        company-c-headers
        company-irony
        company-rtags)))
;;other backends
;'(company-capf company-keywords company-yasnippet company-tempo)
;'company-ispell ;ounly if text
;'(company-nxml company-css) ;ounly if css
;'(company-clang  company-cmake) - lomaet vydachu!!!
;'(company-dabbrev-code company-abbrev company-dabbrev) - lomaen vydachu!!!
;'company-files - ne ponyatno???
;'(company-gtags company-etags)
;'company-elisp









;;
;;irony-eldoc
;;
(require 'irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)









;;
;;flycheck
;;
(require 'flycheck)
(require 'flycheck-rtags)
;;set mode
(add-hook 'irony-mode-hook (lambda ()
    (flycheck-mode)))
;;setup flycheck
(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;set local path
(setq flycheck-clang-include-path
    (append my:c-headers-path-user my:c-headers-path-system))










(require 'flycheck-pos-tip)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))









;;(require 'xcscope)
;;(cscope-setup)










;;http://syamajala.github.io/c-ide.html
;;(cmake-ide-setup)
;;((nil . ((cmake-ide-build-dir . "<PATH_TO_PROJECT_BUILD_DIRECTORY>"))))









(provide 'myinit)
