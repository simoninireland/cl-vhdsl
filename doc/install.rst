Installation
============

In a nutshell
-------------

**Lisps**: Steel Bank Common Lisp v.24.0 or later

**Operating systems**: Linux, OS X

**License**: `GNU General Public License v3 or later (GPLv3) <http://www.gnu.org/licenses/gpl.html>`_

**Repository**: https://github.com/simoninireland/cl-vhdsl

**Maintainer**: `Simon Dobson <mailto:simoninireland@gmail.com>`_


Installation with ASDF and QuickLisp
------------------------------------

At the moment ``CL-VHDSL`` requires manual installation.

All the dependencies of ``CL-VHDSL`` are in QuickLisp -- with one
exception, ``CL-BITFIELDS``, which at present needs to be installed
manually. The easiest way to do this is to clone its repo directly
into QuickLisp's local projects directory:

.. code-block:: shell

   cd ~/quicklisp/local-projects
   git clone --depth 1 https://github.com/simoninireland/cl-bitfields.git

Do the same thing for ``CL-BITFIELDS`` itself:

.. code-block:: shell

   git clone --depth 1 https://github.com/simoninireland/cl-vhdsl.git

(You can also clone either or both repos anywhere the your local
QuickLisp installation will find them.)

Then start your Lisp and run:

.. code-block:: lisp

   (asdf:load-system "cl-vhdsl")

``CL-VHDSL`` is developed on, and is known to work on, SBCL on Linux
and OS X. It *should* work on any other standards-compliant Common
Lisp implementation.
