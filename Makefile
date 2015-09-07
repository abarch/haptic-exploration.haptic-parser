all: haptic-bag-translator

haptic-bag-translator: $(concat $< .asd)  *.lisp
	cl                                                     \
	  -Q                                                   \
	  -S "(:source-registry                                \
	       (:directory \""$$(pwd)"\")                      \
	       (:directory \""$$(pwd)/../rsbag-helper"\")      \
	       :inherit-configuration)"                        \
	  -s $@                                                \
	  --dump ! --output $@ -r "haptic-bag-translator:main"

.PHONY: all
