#!/bin/sh

buildmode="debug"
comm="gprbuild"

# find did we use gprbuild or gnatmake
command -v $comm > /dev/null 2>&1 || comm=gnatmake
# if user enters build mode, use it
if [ $1 ] 
then
   buildmode=$1
fi
# run compilation and get all errors to variable, normal output to terminal
error=$($comm -P steamsky.gpr -XMode=$buildmode 2>&1 > /dev/tty)
# if any error occurs
if [ -n "$error" ]
then
   # if compiler can't find adacurses project file
   if [ "${error#*"steamsky.gpr:1:06:"}" != "$error" ]
   then
      # replace name of project file
      sed -i '1s/adacurses/ncursesada/' steamsky.gpr
      # if we are in git repository
      if [ -d ".git" ]
      then
         #lock local changes in project file
         git update-index --assume-unchanged steamsky.gpr
      fi
      # run compilation again
      $comm -P steamsky.gpr -XMode=$buildmode
   else
      # show all compilation errors
      echo "$error"
   fi
fi
