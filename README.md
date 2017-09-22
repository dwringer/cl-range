# cl-range
This package (loosely?) emulates the xrange() function of Python 2,
and range() of Python 3, with the crucial difference that these ranges
are just like any other Common Lisp struct and are coerced to lists
using the (range-to-list) function.  Slices are made (similar to
Python's [::] slicing) using the (range-slice) and (range-subseq)
functions, which respectively return a range and list by default.

Full elaboration on the interface is not provided: if you aren't
expecting to use this based on familiarity with the Python functions,
I would advise against using this package unless you familiarize
yourself with the way indexing/slicing works on Python lists.

## range (start &optional stop (step 1))
"Creates a new RANGE as described by the function parameters"

### range! (start &optional stop (step 1))
"Create a RANGE, then directly instantiate all values to a list"

### range-elt (r idx)
"Generate the element of a given RANGE designated by the given index"

### range-index (r value)
"Find, in a given RANGE, the index of a specified value"

### range-reverse (r)
"Return a RANGE that yields the same elements as a given range, in reverse"

### range-to-list (r)
"Build a list from the given RANGE"

### range-slice (r &optional from to by to-list?)
"Get the specified sub-RANGE of the given RANGE"

### range-subseq (r start &optional end step)
"Get a sequence representing the specified portion of the given RANGE"
