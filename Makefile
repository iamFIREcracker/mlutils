quickutils := $(shell find vendor/quickutil/ -type f \( -iname \*.asd -o -iname \*.lisp \))

.PHONY: all
all: mlutils.lisp

mlutils.lisp: make-mlutils.lisp $(quickutils) vendor/quickutil/
	sbcl --noinform --load "make-mlutils.lisp"  --eval "(uiop:quit)"

vendor/quickutil:
	ln -sf ~/Workspace/quickutil vendor/quickutil

