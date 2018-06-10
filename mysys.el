;;system hederfile path


; check-ccl-program OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")
    (setq my:c-headers-path-system '(
        "/usr/include"
        "/usr/lib/clang/5.0.1/include"
        "/usr/include/w32api"
        "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include"
        "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++"
        "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/x86_64-pc-cygwin"
        "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/backward"))))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux")
    (setq my:c-headers-path-system '(
        "/usr/include/c++/5"
        "/usr/include/x86_64-linux-gnu/c++/5"
        "/usr/include/c++/5/backward"
        "/usr/lib/gcc/x86_64-linux-gnu/5/include"
        "/usr/local/include"
        "/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed"
        "/usr/include/x86_64-linux-gnu"
        "/usr/include"
        "/usr/lib/llvm-3.8/lib/clang/3.8.0/include")))))

;;
;;exclude to project file
;;

;;user hederfile path
(setq my:c-headers-path-user '(
    "/cygdrive/d/projects/fcgi/src/include"))

;;set syntax standard version
(add-hook 'flycheck-mode-hook (lambda ()
    (setq flycheck-clang-language-standard "c++14")
    (setq flycheck-gcc-language-standard "c++14")
    (setq irony-additional-clang-options '("-std=c++14"))
    (setq company-clang-arguments '("-std=c++14"))))

(provide 'mysys)
