.PHONY: help test e2e deps check-compile

define DEPS_SCRIPT
(progn
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(package-install 'buttercup)
(package-install 'consult)
(package-install 's))
endef
export DEPS_SCRIPT

help:
	@echo "Available commands:"
	@echo "  make deps          Install dependencies"
	@echo "  make test          Run the tests"
	@echo "  make e2e           Run E2E tests against real Jira (local only)"
	@echo "  make compile       Byte-compile the package"
	@echo "  make check-compile Check for clean byte-compilation"

deps:
	@echo "Installing dependencies"
	emacs --batch --eval "$$DEPS_SCRIPT"

TEST_FILES := $(filter-out test/go-jira-e2e-test.el, $(wildcard test/*.el))

test:
	emacs --batch --funcall package-initialize -L . -L test \
	$(foreach f,$(TEST_FILES),-l $(notdir $(f))) \
	--funcall buttercup-run

e2e:
	emacs -Q --batch -L . -l test/go-jira-e2e-test.el

check-compile: deps
	@echo "Checking byte-compilation..."
	emacs -Q --batch \
	--eval "(require 'package)" \
	--eval "(setq package-user-dir \"$(CURDIR)/.elpa\")" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-initialize)" \
	--eval "(package-install 'consult)" \
	--eval "(package-install 's)" \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(byte-compile-file \"go-jira-status.el\")" \
	--eval "(byte-compile-file \"go-jira-assign.el\")" \
	--eval "(byte-compile-file \"go-jira.el\")" \
	--eval "(byte-compile-file \"go-jira-board.el\")" \
	--eval "(byte-compile-file \"go-jira-markup.el\")" \
	--eval "(byte-compile-file \"go-jira-eldoc.el\")" \
	--eval "(byte-compile-file \"go-jira-embark.el\")"
