;;set where found plugginsy
(add-to-list 'load-path "~/emacs.git/lisp")

;; list the packages you want
;;(setq package-list '(package whitespace linum-relative ido paren bs ibuffer buffer-move auto-complete auto-complete-c-headers yasnippet company company-irony company-c-headers magit))
(setq package-list '(
    package
    whitespace
    linum-relative
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
    company-irony-c-headers
    company
    company-c-headers
    company-irony
    irony
    flycheck))

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
;;(menu-bar-mode -1)
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
;; Highlight search resaults
(setq search-highlight t)
(setq query-replace-highlight t)
;;hide welcome screen
(setq inhibit-startup-screen t)



;;restore last session on startup
;;(desktop-save-mode 1)
;;delete selection
(delete-selection-mode t)
;;better buffer rendering
(setq redisplay-dont-pause t)
;; Clipboard settings
(setq x-select-enable-clipboard t)
;;set short confirm command
(fset 'yes-or-no-p 'y-or-n-p)
;;
;;(setq ac-disable-faces nil)
;;
(setq compile-command "cd ../build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .. && make")


(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)
(global-whitespace-mode t)



;; show relative linum numbers
(require 'linum-relative)

;;cbf
(defun linum-relative-set-on ()
  (interactive)
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
;(add-hook 'php-mode-hook 'linum-relative-set-on)
;(add-hook 'python-mode-hook 'linum-relative-set-on)
;(add-hook 'java-mode-hook 'linum-relative-set-on)
;(add-hook 'perl-mode-hook 'linum-relative-set-on)
;(add-hook 'shell-mode-hook 'linum-relative-set-on)
;(add-hook 'sh-mode-hook 'linum-relative-set-on)
;(add-hook 'sh-lisp-mode-hook 'linum-relative-set-on)
;(add-hook 'xml-mode-hook 'linum-relative-set-on)
;(add-hook 'css-mode-hook 'linum-relative-set-on)
;(add-hook 'javascript-mode-hook 'linum-relative-set-on)
;(add-hook 'makefile-mode-hook 'linum-relative-set-on)
;(add-hook 'cmake-mode-hook 'linum-relative-set-on)



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
;;(setq show-paren-style 'expression) ;;highlight curent block



(require 'buffer-move)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
;;(setq buffer-move-stay-after-swap t)
;;(setq buffer-move-behavior 'move)



;;start auto-complete with emacs
;(require 'auto-complete)
;;do default config for auto-complete
;(ac-config-default)

;;(setq ac-auto-start nil)
;;(setq ac-auto-show-menu nil)
;(setq ac-use-fuzzy t)
;(setq ac-ignore-case 'smart)

;;let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
;;#gcc -xc++ -E -v -
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories
    (append '("/cygdrive/d/projects/fcgi/src/include"
              "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++"
              "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/x86_64-pc-cygwin"
              "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/backward"
              "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include"
              "/usr/lib/clang/5.0.1/include"
              "/usr/include/w32api"
              "/usr/include")
            achead:include-directories)))
;;now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)




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
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;;separate list of buffers when clicked C-x C-b
(iswitchb-mode 1)
;; disable dired buffer
(put 'dired-find-alternate-file 'disabled nil)




;;
;;company-irony-c-headers
;;
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
 '(add-to-list
   'company-backends '(company-irony-c-headers company-irony)))





;;
;;company
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)





;;
;;company-c-headers
;;
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(setq company-c-headers-path-user '(
    "/cygdrive/d/projects/fcgi/src/include"))
(setq company-c-headers-path-system '(
    "/cygdrive/d/projects/fcgi/src/include"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/x86_64-pc-cygwin"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/backward"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include"
    "/usr/lib/clang/5.0.1/include"
    "/usr/include/w32api"
    "/usr/include"))





;;
;;company-irony
;;
(require 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))




;;
;;irony
;;
(require 'json)
(require 'cl-lib)
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)




;;
;;flycheck
;;
(require 'flycheck)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))




;;
;;yasnippet
;;
(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/emacs.git/snippets"))





;;
;;company
;;
;(require 'company)
;(add-hook 'c-mode-common-hook 'company-mode)
;(add-hook 'emacs-lisp-mode-hook 'company-mode)
;(setq company-c-headers-path-user '("/cygdrive/d/projects/fcgi/src/include"))
;(eval-after-load 'company
;  '(add-to-list 'company-backends 'company-irony 'company-c-headers))

;(setenv "PATH"
;        (concat
;         "/usr/local/bin" ";"
;         "/home/Peretykin/.emacs.d" ";"
;         "/home/Peretykin/.emacs.d/irony/bin" ";"
;         (getenv "PATH")
;         )
;)
;(setq exec-path (append '("/usr/local/bin" "/home/Peretykin/.emacs.d/irony/bin")
;                        exec-path))

(add-hook 'c++-mode-hook
    (lambda ()
        (setq flycheck-clang-language-standard "c++14")
        (setq flycheck-gcc-language-standard "c++14")
        (setq company-clang-arguments '("-std=c++14"))
    )
)

(provide 'myinit)
