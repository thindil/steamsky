## General Info

Steam Sky is an open source roguelike with a steampunk setting. You are a
commander of a flying ship, as leader you will be traveling across floating
bases, engaging in combat, trading goods etc. There is no mandatory ending
to this game, you may freely play until your character die. The game is
currently under heavy development, but is in a playable state. Steam Sky is
available on Linux and Windows 64 bit platforms.

## Game versions
There are currently 2 versions of the game:
- 3.0.x: "stable" version of game. This version will receive bug fixes but
  no new features. Source code for this version is in *3.0* branch.
- 3.x: "development" version of game, future version 4.0. This is where
  game feature updates will happen. Due to new features, save compatibility
  will typically break between releases. Use this version at your own risk.
  Source code for this version is in the *master* branch. **This** version.

## Build game from sources

To build(works on Linux and Windows too) you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

  It is recommended to use GNAT GPL 2018 to compile the game on Linux.
  Game does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012

* GtkAda library which should be available in most Linux distributions. Best
  option is to use (with GNAT GPL) AdaCore version of GtkAda from:

  https://www.adacore.com/download/more

  At this moment tested version of GtkAda is 2018 and game require GTK library
  in version 3.14 (may not works with other versions).

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile game is use Gnat Programming Studio included in GNAT.
  Just run GPS, select *steamsky.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`


## Running Steam Sky

### Linux
If you use downloaded AppImage version, you don't need any additional
libraries. Just run it as any AppImage file. More informations about AppImage
files usage, you can find at:

https://docs.appimage.org/user-guide/run-appimages.html

When you trying to run build by yourself version of the game, use script
`steamsky.sh`. Game will not works if you try to start it by binary file
`steamsky` from `bin` directory.

### Windows
If you use downloaded binaries just clicking on `steamsky.exe` in `bin`
directory should run it or start the game by menu entry.

### Starting parameters
You can set game directories by starting parameters. Possible options are:

* --datadir=[directory] set directory where all game data files (and
  directories like ships, items, etc.) are. Example: `./steamsky.sh
  --datadir=/home/user/game/tmp`. Default value is *data/*

* --savedir=[directory] set directory where game (or logs) will be saved. Game
  must have write permission to this directory. Example: `./steamsky.sh
  --savedir=/home/user/.saves`. Default value is *data/saves/*

* --docdir=[directory] set directory where game documentation is (at this
  moment important only for license and changelog files). Example `./steamsky.sh
  --docdir=/usr/share/steamsky/doc`. Default value is *doc/*.

* --modsdir=[directory] set directory where game modifications are. Example:
  `./steamsky.sh --modsdir=/home/user/.mods`. Default value is *data/mods/*

* --themesdir=[directory] set directory where game themes are. Example:
  `./steamsky.sh --themesdir=/home/user/.mods`. Default value is *data/themes/*

Of course, you can set all parameters together: `./steamsky.sh --datadir=somedir/
--savedir=otherdir/ --docdir=anotherdir/`

Paths to directories can be absolute or relative where file `steamsky` (Linux)
or `steamsky.exe` (Windows) is. For Windows, use `steamsky.exe` instead of
`./steamsky.sh`. For Linux AppImage, use `steamsky-dev-x86_64.AppImage`
instead of `./steamsky.sh`.

## Modding Support
For detailed informations about modifying various game elements or debugging
game, see [MODDING.md](bin/doc/MODDING.md)

## Contributing to project
For detailed informations about contributing to the project (bugs reporting,
ideas propositions, code conduct, etc), see
[CONTRIBUTING.md](bin/doc/CONTRIBUTING.md)

## Licenses
Game is available under [GPLv3](bin/doc/COPYING) license.

GtkAda and XmlAda libraries distributed with game are under GPLv3 license.

Gtk library distributed with game is under LGPLv2.1 license:

https://www.gtk.org/

Font Roboto is under Apache v2.0 license:

https://fonts.google.com/specimen/Roboto

Font Awesome is under SIL Open Font License:

https://fontawesome.com

Font Z003 is under AGPL v3 license:

https://github.com/ArtifexSoftware/urw-base35-fonts

Font Rye is under Open Font License:

https://fonts.google.com/specimen/Rye

Font Hack Nerd Font is under MiT license:

https://nerdfonts.com/



More documentation about game (changelog, license) you can find in
[doc](bin/doc) directory.

----

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
