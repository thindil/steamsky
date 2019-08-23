@ECHO OFF
set PATH=%PATH%;C:/GNAT/2019/bin
gprbuild -P steamsky.gpr -XMode=release
COPY /B bin\steamsky.exe others\Output\release\bin\steamsky.exe
gprclean -P steamsky.gpr
