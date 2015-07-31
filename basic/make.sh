#!/bin/sh

set -e

TARGET=obasic
FLAGS="-use-menhir -lib extlib -lib unix -cflags -I,/usr/local/lib/ocaml/site-lib/extlib -lflags -I,/usr/local/lib/ocaml/site-lib/extlib -yaccflag --explain"
OCAMLBUILD=ocamlbuild

ocb()
{
	$OCAMLBUILD $FLAGS $*
}

rule () {
	case $1 in
		clean) ocb -clean;;
		native) ocb $TARGET.native;;
		byte) ocb $TARGET.byte;;
		all) ocb $TARGET.native $TARGET.byte;;
		depend) echo "Not needed.";;
		*) echo "Unknown action $1";;
	esac;
}

if [ $# -eq 0 ]; then
	rule all
else
	while [ $# -gt 0 ]; do
		rule $1;
		shift
	done
fi
