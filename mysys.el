;;system hederfile path
(setq my:c-headers-path-system '(
    "/usr/include"
	"/usr/lib/clang/5.0.1/include"
	"/usr/include/w32api"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/x86_64-pc-cygwin"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/backward"))

;;
;;exclude to project file
;;

;;user hederfile path
(setq my:c-headers-path-user '(
    "/cygdrive/d/projects/fcgi/src/include"
    "/cygdrive/d/projects/fcgi/src/include"))

;;set syntax standard version
(add-hook 'flycheck-mode-hook (lambda ()
    (setq flycheck-clang-language-standard "c++14")
    (setq flycheck-gcc-language-standard "c++14")
    (setq irony-additional-clang-options '("-std=c++14"))
    (setq company-clang-arguments '("-std=c++14"))))

(provide 'mysys)
