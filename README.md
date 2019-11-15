## General Info

Steam Sky is an open source roguelike with a steampunk setting. You are a
commander of a flying ship, as leader you will be traveling across floating
bases, engaging in combat, trading goods etc. There is no mandatory ending
to the game, you may freely play until your character dies. The game is
currently constant development, but is in a playable state. Steam Sky is
available for Linux and Windows 64 bit platforms.

## Game versions
There are currently 2 versions of the game:
- 4.0.x: "stable" version of game. This version will receive bug fixes but
  no new features. Source code for this version is in *4.0* branch.
- 4.x: "development" version of game, future version 5.0. This is where
  game feature updates happen. Due to new features, save compatibility
  will typically break between releases. Use this version at your own risk.
  Source code for this version is in the *master* branch. **This** version.

## Build game from sources

To build(works on Linux and Windows too) you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

  It is recommended to use GNAT GPL 2019 to compile the game on Linux.
  Game does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012

* GtkAda library which should be available in most Linux distributions. Best
  option is to use (with GNAT GPL) AdaCore version of GtkAda from:

  https://www.adacore.com/download/more

  At this moment tested version of GtkAda is 2019 and game require GTK library
  in version 3.14 (may not works with other versions).

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile game is use Gnat Programming Studio included in GNAT.
  Just run GPS, select *steamsky.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`. If you
  have installed *Bob* you can type `bob debug` for build in debug mode or
  `bob release` to prepare release for the program. If you want to only build
  release version of the game, use only `gprbuild -XMode=release` command.

### Build unit tests

Navigate to `tests/driver` directory from the main directory (where this
file is):

* From console: type `gprbuild -P test_driver.gpr`

Or if you have *Bob* installed, type `bob tests`.

## Generating code documentation

To generate (or regenerate) code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/).
If you have it, in main program directory (where this file is) enter terminal
command: `others/generatedocs.py`. For more information about this script,
please look [here](https://github.com/thindil/roboada#generatedocspy). This
version of script have set all default settings for Hunter code. If you have
*Bob* installed, you can type `bob docs`.

## Running Steam Sky

### Linux
If you use downloaded AppImage version, you don't need any additional
libraries. Just run it as any AppImage file. More informations about AppImage
files usage, you can find at:

https://docs.appimage.org/user-guide/run-appimages.html

When you trying to run build by yourself version of the game, use script
`run.sh`. Game will not works if you try to start it by binary file
`steamsky` from `bin` directory. This script generally was created for
testing purposes, may require some changes especially if you want to run
the game which was build in release mode. In that situation, probably
easier way will be decompress AppImage version of the game and replace
files inside with the new version.

### Windows
If you compiled the game or downloaded the binaries just clicking on
`steamsky.exe` in the `bin` directory should run it.

### Starting parameters
You can specify the game directories through command-line parameters.
Possible options are:

* --datadir=[directory] This is where the game data files are kept.
   Example: `./run.sh --datadir=/home/user/game/tmp`.
   Default value is *data/*

* --savedir=[directory] This is where savegames and logs are kept.
   The Game must have write permission to this directory.
   Example: `./run.sh --savedir=/home/user/.saves`.
   Default value is *data/saves/*

* --docdir=[directory] This is where the game documentation is.
   Example `./run.sh --docdir=/usr/share/steamsky/doc`.
   Default value is *doc/*.

* --modsdir=[directory] This is where mods are loaded from.
   Example:`./run.sh --modsdir=/home/user/.mods`.
   Default value is *data/mods/*

* --themesdir=[directory] This is where custom themes are loaded from.
   Example: `./run.sh --themesdir=/home/user/.mods`.
   Default value is *data/themes/*

Of course, you can set all parameters together:
`./run.sh --datadir=somedir/ --savedir=otherdir/ --docdir=anotherdir/`

Paths to directories can be absolute or relative where file `steamsky` is. For
Windows, use `steamsky.exe` instead `./run.sh`. If you use AppImage version
of the game, you can also use all of this starting parameters.

### Testing versions

Here are available also testing versions of the game. You can find them
in [Releases](https://github.com/thindil/steamsky/releases/tag/travis-dev-build).
To use them, first you must download normal release. Then, for Linux: inside
directory where the game is, type `./steamsky-x86_64.AppImage --appimage-extract`
to extract whole game to directory *squashfs-root*. And then just move files
from the archive to proper locations. To run that version, just enter
*squashfs-root* directory and type in console `./AppRun`. For Windows: just
unzip files (replace existing) to proper location where the game is installed.

* steamsky-dev-Windows-64.zip contains Windows 64-bit version of the game.

* steamsky-dev-Linux-64.tar.gz contains Linux 64-bit version of the game.

## Modding Support
For detailed informations about modifying various game elements or debugging
game, see [MODDING.md](bin/doc/MODDING.md)

## Contributing to project
For detailed informations about contributing to the project
(bugs reporting, ideas propositions, code conduct, etc),
see [CONTRIBUTING.md](bin/doc/CONTRIBUTING.md)

## Licenses
The game is made available under the [GPLv3](bin/doc/COPYING) license.

The GtkAda and XmlAda libraries distributed with game are also under the GPLv3 license.

The Gtk library distributed with game is under the LGPLv2.1 license: https://www.gtk.org/

The Licensing for the fonts distributed with the game is as follows:

* Font Amarante is under SIL Open Font License: https://fonts.google.com/specimen/Amarante
* Font Awesome is under SIL Open Font License: https://fontawesome.com
* Font Rye is under Open Font License: https://fonts.google.com/specimen/Rye
* Font Hack Nerd Font is under MiT license: https://nerdfonts.com/
* Font Roboto is under Apache v2.0 license: https://fonts.google.com/specimen/Roboto


The changelog and a copy of the GPLv3 license can be found in the [doc](bin/doc) directory.


That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
