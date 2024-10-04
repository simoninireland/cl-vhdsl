Installation
============

In a nutshell
-------------

**Lisps**: Steel Bank Common Lisp v.24.0 or later

**Operating systems**: Linux, OS X

**License**: `GNU General Public License v3 or later (GPLv3) <http://www.gnu.org/licenses/gpl.html>`_

**Repository**: https://github.com/simoninireland/cl-vhdsl

**Maintainer**: `Simon Dobson <mailto:simoninireland@gmail.com>`_


Installation with ASDF
----------------------

At the moment CL-VHDSL requires manual installation. Clone the
repository into a directory that ASDF will find. Then from Lisp
execute:

.. code-block:: lisp

   (asdf:load-system "cl-vhdsl")
