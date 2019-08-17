#!/usr/bin/env python
# Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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
""" Generate documentation and fix syntax highlightning for Ada """

import glob
import re
import subprocess
import sys

def fixdocs(directory):
    """ Fix all generated documentation in selected directory. """
    for filename in glob.glob(directory + "/*.html"):
        # read whole file to list of lines
        with open(filename) as fn:
            content = fn.readlines()
        for i in range(len(content)):
            result = re.search("<span class=\"squote\">'\w+'*", content[i])
            if result is not None:
                keyword = result.group()[21:]
                if keyword[len(keyword) - 1] != "'":
                    content[i] = re.sub("<span class=\"squote\">'\w+", "<span class=\"keyword\">" + keyword + "</span>", content[i]);
                k = i
                while content[k].strip() != "</pre>":
                    result2 = re.search("(?!\">)(\)|;|:=|:|\()+(?!</span>)", content[k])
                    while result2 is not None:
                        endline = content[k][result2.span()[1]:]
                        content[k] = content[k][:result2.span()[0]] + "<span class=\"sign\">"
                        for j in range(len(result2.group())):
                            content[k] += result2.group()[j]
                        content[k] += "</span>" + endline
                        result2 = re.search("(?!\">)(\)|;|:=|:|\()+(?!</span>)", content[k])
                    result2 = re.search("(?!\">)(\sis\s|\spragma\s|\sreturn\s|\sconstant\s)+(?!</span>)", content[k])
                    while result2 is not None:
                        endline = content[k][result2.span()[1]:]
                        content[k] = content[k][:result2.span()[0]] + "<span class=\"keyword\">"
                        for j in range(len(result2.group())):
                            content[k] += result2.group()[j]
                        content[k] += "</span>" + endline
                        result2 = re.search("(?!\">)(\sis\s|\spragma\s|\sreturn\s|\sconstant\s)+(?!</span>)", content[k])
                    k += 1
                    if k == len(content):
                        break
        # save fixed file
        with open(filename, "w") as newfile:
            for line in content:
                newfile.write("%s" % line)
        # fix documentation in any subdirectory too
    for dirname in glob.glob(directory + "*/"):
        fixdocs(dirname)

docsdir = "docs"
configfile = "others/robodocada.rc"

if len(sys.argv) > 1:
    configfile = sys.argv[1]
    if len(sys.argv) > 2:
        docsdir = sys.argv[2]

subprocess.call(["robodoc", "--rc", configfile])
fixdocs(docsdir)
