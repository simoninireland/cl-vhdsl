# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'VHDSL'
copyright = '2024, Simon Dobson'
author = 'Simon Dobson'
release = '0.1'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "sphinxcontrib.bibtex",
    "sphinxcontrib.cldomain",
    "sphinxcontrib.hyperspec",
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# roswell may be installed locally
from os import environ
environ["PATH"] += f":{environ['HOME']}/.local/bin"

# Common Lisp ASDF systems and packages
from os.path import join, dirname, realpath, expandvars

pygments_style = 'sphinx'
highlight_language = 'common-lisp'
cl_debug = False
todo_include_todos = False

cl_systems = [{"name": "cl-vhdsl",
               "path": join(dirname(realpath(__file__)), "../")}]

# PAckages to grab symbols from (in the core image)
cl_packages = ["common-lisp"]

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This patterns also effect to html_static_path and html_extra_path
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# Master bibfile
bibtex_bibfiles = ['bibliography.bib']

# Style
bibtex_reference_style = 'label'
bibtex_default_style = 'alpha'


# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
html_static_path = ['_static']
