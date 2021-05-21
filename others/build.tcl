#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

# Check if the script was started from the proper location (root directory)
if {[file exists steamsky.gpr] == 0} {
   puts {This script must be run in the directory where steamsky.gpr file is}
   return
}

# Set the target for the compilation. If no arguments, use system default
if {$argc == 0} {
   if {$tcl_platform(os) == "Linux"} {
      set target x86_64-linux-gnu
   } else {
      set target x86_64-windows
   }
} else {
   set target [lindex $argv 0]
}

# Check if correct target was set
if {$target != "x86_64-linux-gnu" && $target != "x86_64-windows"} {
   puts {Invalid compilation target. Allowed options are x86_64-linux-gnu and x86_64-windows}
   return
}

# Clean and compile the game
exec gprclean -P steamsky.gpr --target=$target >@stdout
if {$target == "x86_64-linux-gnu"} {
   exec gprbuild -p -P steamsky.gpr -XMode=release -XOS=Unix --target=$target >@stdout
   set dirname steamsky-linux
   set extension {}
} else {
   exec gprbuild -p -P steamsky.gpr -XMode=release -XOS=Windows --target=$target >@stdout
   set dirname steamsky-windows
   set extension .exe
}

# Copy all files and directories to release directory
puts -nonewline {Copying files and directories ... }
file mkdir [file join release $dirname]
file copy [file join bin steamsky$extension] [file join release $dirname steamsky$extension]
file copy [file join bin data] [file join release $dirname data]
file delete -force [file join release $dirname data mods] [file join release $dirname data saves] [file join release $dirname data themes]
file copy [file join bin doc] [file join release $dirname doc]
file copy README.md [file join release $dirname doc]
puts {done}
exec gprclean -P steamsky.gpr --target=$target >@stdout
