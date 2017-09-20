# If (the right) Emacs isn't in your path, define the variable EMACS
# in localvars.mk.  e.g.
# EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
-include localvars.mk

ifndef EMACS
EMACS=`which emacs`
endif

ELPA=./elpa

all:
	for i in *.org; do \
	    $(EMACS) --batch \
	             --visit=$$i \
	             --eval "(progn (require 'ob) (org-babel-tangle nil (file-name-nondirectory \"`basename $$i .org`.el\")))"; \
	done

	for i in *.el; do \
	    $(EMACS) --batch \
	             --eval "(progn (require 'package-x) (setq package-archive-upload-base (expand-file-name \"$(ELPA)\")) (package-upload-file \"$$i\"))"; \
	done
