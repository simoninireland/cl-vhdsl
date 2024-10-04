# Makefile for cl-vhdsl
#
# Copyright (C) 2024 Simon Dobson
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
VERSION = 0.1


# ----- Sources -----


# ----- Tools -----

# Base commands
LISP = sbcl
PYTHON = python3
PIP = pip
VIRTUALENV = $(PYTHON) -m venv
ACTIVATE = . $(VENV)/bin/activate
CHDIR = cd

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
RUN_SPHINX_HTML = PYTHONPATH=$(ROOT) make html


# ----- Top-level targets -----

# Default prints a help message
help:
	@make usage

# Build the API documentation using Sphinx
.PHONY: doc
doc: env $(SOURCES_DOCUMENTATION) $(SOURCES_DOC_CONF)
	$(ACTIVATE) && $(CHDIR) doc && $(RUN_SPHINX_HTML)

# Build a documentation Python venv
.PHONY: env
env: $(VENV)

$(VENV):
	$(VIRTUALENV) $(VENV)
	$(ACTIVATE) && $(PIP) install -U pip wheel && $(PIP) install -r requirements.txt

# Make a new release
release: $(SOURCES_GENERATED) master-only commit upload

# Check we're on the master branch before uploading
master-only:
	if [ "$(GIT_BRANCH)" != "master" ]; then echo "Can only release from master branch"; exit 1; fi

# Update the remote repos on release
# (This will trigger .github/workflows/release.yaml to create a GitHib release, and
# .github/workflows/ci.yaml to run the full integration test suite)
commit: check-local-repo-clean
	$(GIT) push origin master
	$(GIT) tag -a v$(VERSION) -m "Version $(VERSION)"
	$(GIT) push origin v$(VERSION)

.SILENT: check-local-repo-clean
check-local-repo-clean:
	if [ "$(GIT_DIRTY)" ]; then echo "Uncommitted files: $(GIT_DIRTY)"; exit 1; fi

# Clean up the distribution build
clean:
	$(RM) $(SOURCES_GENERATED) $(SOURCES_DIST_DIR) $(PACKAGENAME).egg-info dist $(SOURCES_DOC_BUILD_DIR) $(SOURCES_DOC_ZIP)

# Clean up everything, including the computational environment (which is expensive to rebuild)
reallyclean: clean
	$(RM) $(VENV)


# ----- Generated files -----

# Manifest for the package
MANIFEST: Makefile
	echo $(SOURCES_EXTRA) $(SOURCES_GENERATED) $(SOURCES_CODE_INIT) $(SOURCES_CODE) | $(TR) ' ' '\n' >$@

# The setup.py script
setup.py: $(SOURCES_SETUP_IN) $(REQUIREMENTS) Makefile
	$(CAT) $(SOURCES_SETUP_IN) | $(SED) -e 's|VERSION|$(VERSION)|g' -e 's|REQUIREMENTS|$(PY_REQUIREMENTS)|g' >$@

# The source distribution tarball
$(DIST_SDIST): $(SOURCES_GENERATED) $(SOURCES_CODE_INIT) $(SOURCES_CODE) Makefile
	$(ACTIVATE) && $(RUN_SETUP) sdist

# The binary (wheel) distribution
$(DIST_WHEEL): $(SOURCES_GENERATED) $(SOURCES_CODE_INIT) $(SOURCES_CODE) Makefile
	$(ACTIVATE) && $(RUN_SETUP) bdist_wheel

# The tags file
TAGS:
	$(ETAGS) -o TAGS $(SOURCES_CODE)



# ----- Usage -----

define HELP_MESSAGE
Available targets:
   make env          create a documentation virtual environment
   make doc          build the API documentation using Sphinx
   make release      make a release
   make clean        clean-up the build
   make reallyclean  clean up the virtualenv as well

endef
export HELP_MESSAGE

usage:
	@echo "$$HELP_MESSAGE"
