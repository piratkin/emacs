;;
;;system hederfile path
;;
; check-ccl-program OS type
(cond
 ((string-equal system-type "cygwin") ; Microsoft Windows
  (progn
    (message "Detected: Microsoft Windows")
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
    (message "Detected: Mac OS X")))
 ((string-equal system-type "gnu/linux") ; GNU/Linux
  (progn
    (message "Detected: GNU/Linux")
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

(provide 'mysys)
