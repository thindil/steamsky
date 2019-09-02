@ECHO OFF
set PATH=%PATH%;C:/GNAT/2019/bin
gprbuild -P steamsky.gpr -XMode=release
XCOPY /S bin others\Output\release\bin\
XCOPY README.md others\Output\release\bin\doc\
gprclean -P steamsky.gpr
