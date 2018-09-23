## General Info

Steam Sky is an open source roguelike with a steampunk setting. You are a 
commander of a flying ship, as leader you will be traveling across floating 
bases, engaging in combat, trading goods etc... There is no ending to this 
game, the game continues until your character dies. The game is currently 
under heavy development, but is in a playable state. Steam Sky stable edition 
is only developed for Linux 64-bit.

## Game versions
There are currently 2 versions of the game:
- 2.0.x: "stable" version of game. This version will receive bug fixes but
  no new features. Source code for this version is in *2.0* branch.
- 2.x: "development" version of game, future version 3.0. This is where 
  game feature updates will happen. Due to new features, save compatibility 
  will typically break between releases. Use this version at your own risk. 
  Source code for this version is in the *master* branch.

## Build the stable version

To build you need:

* compiler - GCC with enabled Ada support or GNAT from: 
  
  http://libre.adacore.com/download/

  Build testing has been done using both GCC 7.1 and GNAT GPL 2017.
  Game does not work with old compilers (like GCC 4.9) since it 
  lacks full support for Ada 2012

* ncurses Ada binding (should be available in most distributions or with ncurses 
  package or as standalone package). If not you can download it from:
  
  http://invisible-island.net/ncurses/ncurses-Ada95.html

* optional, but highly recommended:  gprbuild program - should be available in most 
  distributions, if not download from: 
  
  http://libre.adacore.com/download/


If you have all, in main source code directory (where this file is):

* Best option, is to use build.sh script. It can detect did you have gprbuild
  installed and use it or gnatmake automatically. Additionally, if you use
  Debian or Debian based distribution (like Ubuntu, etc.) it can fix
  *steamsky.gpr* file automatically. This is recommended for first time
  build. Type `./build.sh` for debug build or `./build.sh release` for release
  version.

* if you don't have gprbuild: type `gnatmake -P steamsky.gpr` for the debug build 
  or for the release version: `gnatmake -P steamsky.gpr -XMode=release`

* if you have gprbuild: type `gprbuild` for debug mode build or for release 
  mode: `gprbuild -XMode=release`


## Running Steam Sky
If you build game from sources you need only ncurses library to run game. Enter
*bin* directory and type `./steamsky`.
If you use released binaries, you don't need any additional libraries, just in
main game directory (where this file is) type `./steamsky`.
The Game only works through the terminal.

### Starting parameters
You can set game directories by using starting parameters. Possible options are:

* --datadir=[directory] set directory where all game data files (and
  directories like ships, items, etc.) are. Example: `./steamsky
  --datadir=/home/user/game/tmp`. Default value is *data/*

* --savedir=[directory] set directory where game (or logs) will be saved. Game
  must have write permission to this directory. Example: `./steamsky
  --savedir=/home/user/.saves`. Default value is *data/*

* --docdir=[directory] set directory where game documentation is (at this
  moment important only for license and changelog files). Example `./steamsky
  --docdir=/usr/share/steamsky/doc`. Default value is *doc/*.

Of course, you can set all parameters together: `./steamsky --datadir=somedir/
--savedir=otherdir/ --docdir=anotherdir/`

Paths to directories can be absolute or relative where file `steamsky` is. 

### Troubleshooting

#### One background color for whole map
If you have whole game map in one color (no difference between discovered and
unknown parts of map) you may try to run the game with changed environmental
variable TERM: `TERM=xterm-256color ./steamsky`. Some Linux distributions set
this variable for each terminal emulator what can cause problems. More info
about this you can find at: https://github.com/thindil/steamsky/issues/28

#### Different colors in game
If you have different colors in game that this which are on screenshots: if you
changed default colors for terminal emulator, the game will be used them
instead of standard. This is limitation of terminal applications. More info
about this you can find at: https://github.com/thindil/steamsky/issues/28 and
https://github.com/thindil/steamsky/issues/15

## Modding Support
For detailed informations about modifying various game elements or debugging
the game, see [MODDING.md](bin/doc/MODDING.md)

## Contributing to project
For detailed informations about contributing to the project (bugs reporting, 
ideas propositions, code conduct, etc), see 
[CONTRIBUTING.md](bin/doc/CONTRIBUTING.md)


More documentation about game (changelog, license) you can find in
[doc](bin/doc) directory.

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
