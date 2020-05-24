#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {[file exists steamsky.gpr] == 0} {
   puts {This script must be run in the directory where steamsky.gpr file is}
   return
}

cd [file join tests driver]

for {set i 1} {$i <= [lindex $argv 0]} {incr i} {
   set result [exec [file join [pwd] test_runner]]
   puts "$i: $result"
   if {[string first FAILED $result] > -1 || [string first CRASHED $result] > -1} {
      exit 1;
   }
}
