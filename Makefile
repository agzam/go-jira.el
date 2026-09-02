.PHONY: help test e2e deps compile check-compile clean

# Every emacs invocation below is sandboxed: --init-directory keeps the eln
# cache, auto-save lists and url/ out of the developer's real ~/.emacs.d, and
# package-user-dir is read at package.el load time so it must be set before
# package-initialize runs.
SANDBOX  := $(CURDIR)/.sandbox
ELPA     := $(SANDBOX)/elpa
EMACS    ?= emacs
BATCH     = $(EMACS) -Q --batch --init-directory=$(SANDBOX) \
	--eval "(setq package-user-dir \"$(ELPA)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	--eval "(package-initialize)"

DEPS      = buttercup consult s markdown-mode embark

# E2E files talk to a real Jira and mutate a ticket; they never run under `test'.
E2E_FILES  := $(wildcard test/*e2e*.el)
TEST_FILES := $(filter-out $(E2E_FILES), $(wildcard test/*.el))

help:
	@echo "Available commands:"
	@echo "  make deps          Install dependencies into .sandbox/elpa"
	@echo "  make test          Run the tests"
	@echo "  make e2e           Run all E2E tests against real Jira (local only)"
	@echo "  make e2e-markup    E2E: Jira markup round-trip"
	@echo "  make e2e-comments  E2E: comment identity, replies, editing"
	@echo "  make check-compile Check for clean byte-compilation"
	@echo "  make clean         Remove the sandbox and byte-compiled files"

deps:
	@echo "Installing dependencies into $(ELPA)"
	$(BATCH) --eval "(package-refresh-contents)" \
	--eval "(dolist (p '($(DEPS))) (unless (package-installed-p p) (package-install p)))"

test:
	$(BATCH) -L . -L test \
	$(foreach f,$(TEST_FILES),-l $(notdir $(f))) \
	--funcall buttercup-run

e2e: e2e-markup e2e-comments

e2e-markup:
	$(BATCH) -L . -l test/go-jira-e2e-test.el

e2e-comments:
	$(BATCH) -L . -l test/go-jira-comment-e2e-test.el

check-compile: deps
	@echo "Checking byte-compilation..."
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(byte-compile-file \"go-jira-status.el\")" \
	--eval "(byte-compile-file \"go-jira-assign.el\")" \
	--eval "(byte-compile-file \"go-jira-comment.el\")" \
	--eval "(byte-compile-file \"go-jira.el\")" \
	--eval "(byte-compile-file \"go-jira-board.el\")" \
	--eval "(byte-compile-file \"go-jira-markup.el\")" \
	--eval "(byte-compile-file \"go-jira-eldoc.el\")" \
	--eval "(byte-compile-file \"go-jira-embark.el\")"

clean:
	rm -rf $(SANDBOX)
	rm -f *.elc test/*.elc
