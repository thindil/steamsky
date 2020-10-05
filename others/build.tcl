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
exec gprbuild -p -P steamsky.gpr -XMode=release --target=$target >@stdout
puts -nonewline {Copying files and directories ... }
if {$target == "x86_64-linux-gnu"} {
   file mkdir usr/bin usr/share/metainfo
   file copy share/fonts usr/share/
   file copy bin/steamsky usr/bin
   file copy bin/data usr/share/
   file delete -force usr/share/data/mods usr/share/data/saves usr/share/data/themes
   file copy bin/doc usr/share/
   file copy README.md usr/share/doc
   file copy others/steamsky.appdata.xml usr/share/metainfo
} else {
   file mkdir release
   file copy bin [file join release bin]
   file copy README.md [file join release bin doc]
}
puts {done}
exec gprclean -P steamsky.gpr --target=$target >@stdout
