.PHONY: all clean build help init setup setup-app setup-emacs setup-env setup-python bin/fbook bin/ff

all: init build

build: bin/fbook bin/ff

setup: setup-app setup-env
	@bin/Fsetup.sh
	@bin/Fsetup-emacs.sh

setup-env: setup-python setup-emacs

clean :
	@find . -type f \( -name "*~" -o -name "*.elc" \) -delete

bin/fbook:
	@cd go/src/ff; go build -o ${FCHOME}/bin/ff

bin/ff:
	@cd go/src/fbook; go build -o ${FCHOME}/bin/fbook

init:
	@chmod +x bin/* emacs/linux/* emacs/mac/*
	@bin/Fsetup-bash.sh

setup-app:
	@bin/Fsetup-app.sh

setup-emacs:
	@bin/Fsetup-emacs.sh

setup-go:
	@bin/Fsetup-go.sh

setup-python:
	@cd python; python3 setup.py egg_info; pip3 install -r fconfig.egg-info/requires.txt

setup-nvim:
	@bin/Fsetup-nvim.sh

help:
	@echo "setup :- setup the whole environment"
	@echo "setup-app :- install applications"
	@echo "setup-emacs :- setup emacs"
	@echo "setup-nvim :- setup nvim"
	@echo "setup-python :- setup python"
