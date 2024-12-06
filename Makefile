SBCL_BIN ?= sbclw
SBCL_ARGS ?= --noinform

.PHONY: all
all: build doc


.PHONY: mgl-pax
mgl-pax:
	mkdir -p vendor/mgl-pax
	curl -L https://github.com/melisgl/mgl-pax/archive/6bfd77ddfa79f0e0905eb4b99d46ac5d2dd69c24.tar.gz \
		| tar -xz -C vendor/mgl-pax --strip-components=1

.PHONY: build
build:
	${SBCL_BIN} ${SBCL_ARGS} --load "make-mlutils.lisp"  --eval "(uiop:quit)"

.PHONY: doc
doc:
	${SBCL_BIN} ${SBCL_ARGS} \
		--eval "(pushnew '*default-pathname-defaults* asdf:*central-registry*)" \
		--eval "(handler-case (ql:quickload :net.matteolandi.utils/doc) \
			  (error (a) \
			    (format t \"caught error ~s~%~a~%\" a a) \
			    (uiop:quit 17)))" \
		--eval "(handler-case (time (asdf:make :net.matteolandi.utils/doc)) \
			  (error (a) \
			    (format T \"caught error ~s~%~a~%\" a a) \
			    (uiop:quit 13)))" \
		--eval "(uiop:quit 0)"
