#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

# Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

proc fixdocs {dir} {
   foreach filename [glob -directory $dir *.html] {
      # read whole file to list of lines
      set fp [open $filename r]
      set data [read $fp]
      close $fp
      # write a new version of file
      set fp [open $filename w]
      set check false
      lmap line [split $data \n] {
         if [string equal [string range $line 0 19] "<pre class=\"source\">"] {
            set check true
         }
         if [string equal [string range $line 0 5] "</pre>"] {
            set check false
         }
         if $check {
            # Remove first comment sign from the line (mostly in EXAMPLE section)
            set comment [string first "<span class=\"sign\">-</span><span class=\"sign\">-</span>" $line]
            if {$comment > -1 } {
               set line [string replace $line $comment [expr $comment + 54]]
            }
            # If there are still any comment sign, treat line as a comment (mostly in EXAMPLE section)
            set comment [string first "<span class=\"sign\">-</span><span class=\"sign\">-</span>" $line]
            if {$comment > -1} {
               set line [string replace $line $comment [expr $comment + 54] "<span class=\"comment\">-- "]
               regsub -all {<span class="(keyword|sign|quote|squote)">} $line "" line
               regsub -all {</span>} $line "" line
               set line "$line</span>"
            }
            # Normal code line
            if [regexp {<span class="squote">'\w+'*} $line result] {
               if {[string equal [string index $result end] "'"] == 0} {
                  set keyword [string range $result 21 end]
                  regsub {<span class="squote">'\w+'*} $line "<span class=\"keyword\">$keyword</span>" line
               }
            }
            regsub -all {(?!\">)(\)|:=|:|\(|&gt;|&lt;|;|,)+(?!</span>)} $line "<span class=\"sign\">&</span>" line
            regsub -all {(?!\">)(\mis\M|\mpragma\M|\mreturn\M|\mconstant\M|\mwith\M|\maccess\M|\mfunction\M|\mprocedure\M|\min\M|\mabstract\M)+(?!</span>)} $line "<span class=\"keyword\">&</span>" line
            if [regexp {</span>\"\w+\"} $line result] {
               set quote [string range $result 7 end]
               regsub {</span>\"\w+\"} $line "</span><span class=\"quote\">$quote</span>" line
            }
         }
         puts $fp $line
      }
      close $fp
   }
   # fix documenation in any subdirectory too
   set subdirs [glob -type d -directory $dir -nocomplain *]
   foreach dirname $subdirs {
      fixdocs $dirname
   }
}

set docsdir docs
set configfile [file join others robodocada.rc]
if {$argc > 0} {
   set configfile [lindex $argv 0]
}
if {$argc > 1} {
   set docsdir [lindex $argv 1]
}

if [file exists $docsdir ] {
   file delete -force $docsdir
}
exec robodoc --rc $configfile
fixdocs $docsdir
