#!/bin/sh

cd bin
LD_LIBRARY_PATH=. TERMINFO=terminfo ./steamsky $@
