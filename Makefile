.PHONY: all clean build help init setup setup-app setup-emacs setup-env setup-python bin/fbook bin/ff

all: init build

build: bin/fbook bin/ff

setup: init setup-app setup-env
	@bash/setup/setup.sh
	@bash/setup/setup-emacs.sh

setup-env: setup-emacs setup-go setup-nvim  setup-python

clean :
	@find . -type f \( -name "*~" -o -name "*.elc" \) -delete

bin/fbook:
	@cd go/src/ff; go build -o ${FCHOME}/bin/ff

bin/ff:
	@cd go/src/fbook; go build -o ${FCHOME}/bin/fbook

init:
	@chmod +x bin/* emacs/linux/* emacs/mac/*
	@bash/setup/setup-bash.sh

setup-app:
	@bash/setup/setup-app.sh

setup-emacs:
	@bash/setup/setup-emacs.sh

setup-go:
	@bash/setup/setup-go.sh

setup-python:
	@cd python; python3 setup.py egg_info; pip3 install -r fconfig.egg-info/requires.txt

setup-nvim:
	@bash/setup/setup-nvim.sh

help:
	@echo "setup        :- setup the whole environment"
	@echo "setup-app    :- install applications"
	@echo "setup-emacs  :- setup emacs"
	@echo "setup-nvim   :- setup nvim"
	@echo "setup-python :- setup python"
