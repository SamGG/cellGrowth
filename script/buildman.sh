#!/bin/bash
#
# Builds the manual
#

rm -f `dirname $0`/../man/*.Rd

#LANG="en.ASCII"
R --no-save --args `dirname $0`/.. <`dirname $0`/buildman.R 

rm -f `dirname $0`/../man/A4.Rd
rm -f `dirname $0`/../man/B4.Rd
rm -f `dirname $0`/../man/gamm.Rd

#rm -Rf mgsa.pdf mgsa-internal.pdf
#R CMD Rd2dvi --no-preview --pdf -o mgsa.pdf `dirname $0`/..
#R CMD Rd2dvi --internals --pdf -o mgsa-internal.pdf `dirname $0`/..
