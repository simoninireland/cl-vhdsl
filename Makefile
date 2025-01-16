# Makefile for cl-vhdsl
#
# Copyright (C) 2024--2025 Simon Dobson
#
# This file is part of cl-vhdsl, a Common Lisp DSL for hardware design
#
# cl-vhdsl is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# cl-vhdsl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.

# The version we're building
VERSION = $(shell cat version.sexp)


# ----- Sources -----

SOURCES_ASDF = cl-vhdsl.asd

SOURCES = $(shell find src -name '*.lisp' -print)
SOURCES_GENERATED =
SOURCES_GENERATED_EXTRA = TAGS

SOURCES_DOC =  $(shell ls doc/*.rst)
SOURCES_DOC_CONF = doc/conf.py
SOURCES_DOC_BUILD_DIR = doc/_build

BINARIES = \
	vhdslc
BINARIES_BUILD_DIR = bin


# ----- Tools -----

# Base commands
LISP = sbcl
LISPOPTS =
PYTHON = python3
PIP = pip
VIRTUALENV = $(PYTHON) -m venv
ACTIVATE = . $(VENV)/bin/activate
ETAGS = etags
CHDIR = cd
RM = rm -fr
MKDIR = mkdir -p

# Files that are locally changed vs the remote repo
# (See https://unix.stackexchange.com/questions/155046/determine-if-git-working-directory-is-clean-from-a-script)
GIT_DIRTY = $(shell $(GIT) status --untracked-files=no --porcelain)

# The git branch we're currently working on
GIT_BRANCH = $(shell $(GIT) rev-parse --abbrev-ref HEAD 2>/dev/null)

# Root directory
ROOT = $(shell pwd)

# Requirements for the documentation venv
VENV = venv3
REQUIREMENTS = requirements.txt

 # Constructed commands
RUN_SPHINX_HTML = $(CHDIR) doc && make html


# ----- Top-level targets -----

# Default prints a help message
help:
	@make usage

# Build the output binaries
bin: $(BINARIES)
	$(MKDIR) $(BINARIES_BUILD_DIR)
	$(LISP) $(LISPOPTS) \
	--eval '(asdf:make :cl-vhdsl/cli)' \
	--eval '(quit)'

# Build the API documentation using Sphinx
.PHONY: doc
doc: env $(SOURCES_ASDF) $(SOURCES_DOC) $(SOURCES_DOC_CONF)
	$(ACTIVATE) && $(RUN_SPHINX_HTML)

# Build a documentation Python venv
.PHONY: env
env: $(VENV)

$(VENV):
	$(VIRTUALENV) $(VENV)
	$(ACTIVATE) && $(PIP) install -U pip wheel && $(PIP) install -r requirements.txt

# Make a new release
release: $(SOURCES_GENERATED) main-only commit upload

# Check we're on the main branch before uploading
main-only:
	if [ "$(GIT_BRANCH)" != "main" ]; then echo "Can only release from main branch"; exit 1; fi

# Update the remote repos on release
# (This will trigger .github/workflows/release.yaml to create a GitHib release, and
# .github/workflows/ci.yaml to run the full integration test suite)
commit: check-local-repo-clean
	$(GIT) push origin main
	$(GIT) tag -a v$(VERSION) -m "Version $(VERSION)"
	$(GIT) push origin v$(VERSION)

.SILENT: check-local-repo-clean
check-local-repo-clean:
	if [ "$(GIT_DIRTY)" ]; then echo "Uncommitted files: $(GIT_DIRTY)"; exit 1; fi

# Clean up the distribution build and documentation
clean:
	$(RM) $(SOURCES_GENERATED) $(SOURCES_GENERATED_EXTRA) $(SOURCES_DOC_BUILD_DIR) $(BINARIES)

# Clean up everything, including the documentation venv
reallyclean: clean
	$(RM) $(VENV)


# ----- Dependencies -----

vhdslc:
	$(ASDF-MAKE)


# ----- Generated files -----

# The tags file
TAGS:
	$(ETAGS) -o TAGS $(SOURCES)


# ----- Usage -----

define HELP_MESSAGE
Available targets:
   make bin          build the binaries
   make doc          build the API documentation using Sphinx
   make release      make a release
   make clean        clean-up the build
   make reallyclean  clean up the virtualenv as well

endef
export HELP_MESSAGE

usage:
	@echo "$$HELP_MESSAGE"
