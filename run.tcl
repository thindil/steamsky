#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {$tcl_platform(os) == "Linux"} {
   set executable [pwd]/bin/steamsky
} else {
   set executable [pwd]\bin\steamsky.exe
}

cd bin

if {$argc > 0} {
   exec $executable [list $argv] >@stdout
} else {
   exec $executable >@stdout
}
