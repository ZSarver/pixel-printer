binary:
	buildapp --output test --manifest-file src/ql-man.txt --load-system unix-options --load-system sb-posix --load-system sb-bsd-sockets --load package.lisp --load pixel-printer.lisp --entry pixel-printer:main
