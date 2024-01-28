# Overview

This is an implementation of 3-ProtoLisp, a procedural reflective variant of prototypical Lisp.

I implemented this in 2008 and published it in the paper "Reflection for the Masses" which includes the source code.

# Licensing

BSD-2 license

# Installation

I have tested this source code on Lispworks 8.0 for Linux. It should run.

It has been recovered from a very old backup disk, please also check the publication.

Instructions:

1. Open the file "config.lisp" and edit the variable *3-proto-lisp-location* to point to the location of the 3-ProtoLisp source code

2. Type in the CL repl: (asdf/operate:load-system '3-proto-lisp) 

3. Start the 3-ProtoLisp repl by (read-normalize-print)

There are some tests in the file tests.lisp.

# Citation

Please cite the following paper:

Reflection for the Masses. Charlotte Herzeel, Pascal Costanza, Theo D'Hondt. Self-Sustaining Systems: First Workshop, S3 2008 Potsdam, Germany, May 15-16, 2008 Revised Selected PapersMay 2008 Pages 87–122 https://doi.org/10.1007/978-3-540-89275-5_6
 
