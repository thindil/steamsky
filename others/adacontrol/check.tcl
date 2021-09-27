#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

# Check if the script is run from the project's main directory
if {![file exists steamsky.gpr]} {
   puts {This script must be run in the directory where steamsky.gpr file is}
   return
}

# Check if the user entered at least name of the file with AdaControl rules
if {[llength $argv] == 0} {
   puts {You have to specify at least file with rules to use}
   return
}

set rootdir [pwd]
set logfile "[file join $rootdir adacontrol.log]"

exec gprclean -P steamsky.gpr >@stdout
file delete $logfile
cd [file join obj]
if {$argc == 1} {
   set adaoptions "-r steamsky-tcl-cargv-chelper-unicode-sax-dom-input_sources"
} else {
   set adaoptions "[file join $rootdir src [lindex $argv 1]]"
}
if {[catch {exec adactl -f [file join $rootdir others adacontrol [lindex $argv 0]] -p [file join $rootdir steamsky.gpr] -o $logfile -w $adaoptions} results options]} {
   if {[file exists $logfile]} {
      if {[file size $logfile] > 1} {
         return -options $options -level 0 $results
      } else {
         file delete $logfile
      }
   } else {
      return -options $options -level 0 $results
   }
}
