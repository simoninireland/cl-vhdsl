.. _def-implementation-overview:

Overview
========

The definition package ``cl-vhdsl/def`` is intended for defining
components of chips and circuits. The idea is that these component
definitions provide a better, more modular, more re-usable, and
(especially) more automated approach to specifying hardware
functionality that HDLs like Verilog or RTLisp.


The implementation of components
--------------------------------

A component is simply a CLOS class that has ``component`` as a base
class and ``synthesisable-component`` as a metaclass.


The ``synthesisable-component`` metaclass
-----------------------------------------

CLOS classes bring together slots and are used to provide classes that
can be used to specialise generic functions. Each CLOS slot takes a
sequence of options: using a specialised metaclass lets us add further
slot options, and also add extra top-level options that adhere to the
metaclass as a whole.

The extra slot options are:

- ``:width``, the number of bits for representing a variable
- ``:exported``, a flag indicating that the slot is part of the
  exported pin interface of the component
- ``:as``, the representation used for the slot when synthesised
- ``:direction``, the direction of data passing into and/or out of the
  component, for slots in the pin interface
- ``:role``, the role that the slot plays in the design

The metaclass also adds extra meaning to the standard ``:type``
options that specifies a type for values in the slot. The other
standard options like ``:initarg``, ``:reader``, and so on have their
usual meaning.


Sub-components
--------------

Sub-components are other components that are instanciated as part of
their containing component. They are either given the representation
``:as :subcomponent`` or a ``:type`` that is a sub-class of the
``component`` class to ensure that they are synthesisable.


Wiring
------

Components can also be given a ``:wiring`` option that specifies a
wiring diagram for the component. A wiring diagram is formed from one
or more wires, each of which is a list defining the ways in which
slots (either in the component itself or in its sub-components) are
connected.

In implementation terms a wiring diagram is used to populate slots
with the values that indicate how they're wired together.

Values in slots
---------------

When defined, slots are by default unbound. (They can be given an
initial value using the ``:initform`` option.) They are filled-in when
the component is being synthesied.

Slots that represent exported registers or wires are given a value
that represents the wire or register they are being bound to. Slots
representing sub-components, if unbound, are filled by instanciating a
component of their given type.
