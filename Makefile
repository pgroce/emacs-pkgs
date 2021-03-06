# If (the right) Emacs isn't in your path, define the variable EMACS
# in localvars.mk.  e.g.
# EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
-include localvars.mk

ifndef EMACS
EMACS=`which emacs`
endif

ELPA=./docs/elpa

ORGS := $(wildcard *.org)

%.el: %.org
	$(EMACS) --batch --visit $< \
          --eval "(progn (require 'ob) (org-babel-tangle nil \"$@\"))"

%.published: %.el
	$(EMACS) --batch \
          --eval "(progn (require 'package-x) (setq package-archive-upload-base (expand-file-name \"$(ELPA)\")) (package-upload-file \"$<\"))";
	touch $@

all: tooclean publish

publish: $(ORGS:.org=.published)


clean:
	rm -f pg-*.el
	rm -f *.published

# "Too" clean because it deletes the contents of docs/elpa, which are
# generated from tangle but checked in to be visible to Github Pages
tooclean:
	rm -f pg-*.el
	rm -f *.published
	rm -rf 	$(ELPA)/*
