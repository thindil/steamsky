#!/bin/bash
prefix=`dirname $0`

# Redhat 5.1 did not support /bin/pwd -P (although later versions
# like 5.10 do have it). So we also try the shell's built-in
prefix=`cd $prefix; /bin/pwd -P 2>/dev/null || pwd -P`

if [ -d "$DIRECTORY" ]; then
   env="PATH=\"$prefix/bin:\$PATH\";
   LD_LIBRARY_PATH=\"$prefix/lib:\$LD_LIBRARY_PATH\";
   PKG_CONFIG_PATH=\"$prefix/lib/pkgconfig:/usr/lib64/pkgconfig\";
   PKG_CONFIG_PATH=\"\$PKG_CONFIG_PATH:/usr/lib/pkgconfig:/usr/share/pkgconfig\";
   GDK_PIXBUF_MODULE_FILE=\"$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache\";
   GDK_PIXBUF_MODULEDIR=\"$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders/\";
   FONTCONFIG_FILE=\"$prefix/etc/fonts/fonts.conf\";
   XDG_DATA_DIRS=\"$prefix/share\";
   XDG_CONFIG_DIRS=\"$prefix/etc\";
   GSETTINGS_BACKEND=memory;
   GLADE_BASE_DIR=\"$prefix\";
   export PATH LD_LIBRARY_PATH PKG_CONFIG_PATH GDK_PIXBUF_MODULEDIR;
   export GDK_PIXBUF_MODULE_FILE FONTCONFIG_FILE XDG_DATA_DIRS;
   export GLADE_BASE_DIR GSETTINGS_BACKEND"
else
   env=`gtkada-env.sh --print-only`
fi

eval "$env"
exec bin/steamsky "$@"
