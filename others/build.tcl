#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {[file exists steamsky.gpr] == 0} {
   puts {This script must be run in the directory where steamsky.gpr file is}
   return
}

if {$tcl_platform(platform) == "unix"} {
   exec gprbuild -p steamsky.gpr -XMode=release >@stdout
   puts -nonewline {Copying files and directories ... }
   file mkdir usr/bin usr/share/metainfo
   file copy share/fonts usr/share/
   file copy bin/steamsky usr/bin
   file copy bin/data usr/share/
   file delete -force usr/share/data/mods usr/share/data/saves usr/share/data/themes
   file copy bin/doc usr/share/
   file copy README.md usr/share/doc
   file copy others/steamsky.appdata.xml usr/share/metainfo
   puts {done}
}
