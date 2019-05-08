#!/bin/sh

prefix=$(dirname "$0")
cd "$prefix" || exit
prefix=$(pwd)

if [ -d lib ]; then
   # distributed
   export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$prefix/lib
   export GDK_PIXBUF_MODULE_FILE=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
   export GDK_PIXBUF_MODULEDIR=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders/
   export FONTCONFIG_FILE=$prefix/etc/fonts/fonts.conf
   export XDG_DATA_DIRS=$XDG_DATA_DIRS:$prefix/share
   export GSETTINGS_BACKEND=memory
else
   # built-locally
   eval "$(gtkada-env.sh --print-only)"
fi

cd bin || exit
export GTK_THEME=Adwaita
export RUNFROMSCRIPT=1

./steamsky "$@"
