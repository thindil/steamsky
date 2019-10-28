@ECHO OFF
set PATH=%PATH%;C:/GNAT/2019/bin
gprbuild -P steamsky.gpr -XMode=release
XCOPY /S bin release\bin\
XCOPY README.md release\bin\doc\
gprclean -P steamsky.gpr
