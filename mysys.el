;;user hederfile path
(setq my:c-headers-path-user '(
    "/cygdrive/d/projects/fcgi/src_cmake_new/include"
	"/cygdrive/d/projects/fcgi/src_make_old/include"))

;;system hederfile path
(setq my:c-headers-path-system '(
    "/usr/include"
	"/usr/lib/clang/5.0.1/include"
	"/usr/include/w32api"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/x86_64-pc-cygwin"
    "/usr/lib/gcc/x86_64-pc-cygwin/6.4.0/include/c++/backward"))

(provide 'mysys)
