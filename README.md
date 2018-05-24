## General Info

Steam Sky is open source, roguelike game in steampunk theme. Your role is to 
command flying ship and its crew, traveling between floating bases, fighting 
with enemies, trade in goods, etc. Here are no end goal for game, you can play
as long as your character not die. Game is under heavy development, but 
generally is playable. Now game is available on Linux 64-bit and Windows (only
development version).

## Game versions
At this moment are available 2 game versions:
- 2.0.x: "stable" version of game. This version receive only fixes of bugs but
  no new features. Source code for this version is in *2.0* branch.
- 2.x: "development" version of game, future version 3.0. This is main place
  where game development happen. It often break save compatibilities between
  releases so use at your own risk. Source code for this version is in *master*
  branch.

## Build game from sources

To build it (works on Linux and Windows too), you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from: 
  
  https://www.adacore.com/download/

  At this moment tested compiler (on Linux) is GNAT GPL 2017.
  Game not works with old compilers (like GCC 4.9) due to lack of full support
  for Ada 2012.

* GtkAda library which should be available in most Linux distributions. Best
  option is to use (with GNAT GPL) AdaCore version of GtkAda from:
  
  https://www.adacore.com/download/more

  At this moment tested version of GtkAda is 2017 and game require GTK library
  in version 3.14 (may not works with other versions).

If you have all, in main source code directory (where this file is):

* Easiest way to compile game is use Gnat Programming Studio included in GNAT. 
  Just run GPS, select *steamsky.gpr* as a project file and select option `Build
  All`.

* If you prefer using console: in main source code directory type `gprbuild` 
  for debug mode build or for release mode: `gprbuild -XMode=release`


## Running game

### Linux
If you use downloaded binaries, you don't need any additional libraries. Just
run `steamsky` program to start game.

### Windows
If you compiled game by self, just clicking on `steamsky.exe` should be enough 
to run game.

### Starting parameters
You can set game directories by starting parameters. Possible options are:

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

Paths to directories can be absolute or relative where file `steamsky` is. For
Windows, use `steamsky.exe` instead `./steamsky`.

## Modify game
For detailed informations about modifying various game elements or debugging
game, see [MODDING.md](bin/doc/MODDING.md)

## Contributing to project
For detailed informations about contributing to project (bugs reporting, ideas
propositions, code conduct, etc), see [CONTRIBUTING.md](bin/doc/CONTRIBUTING.md)

## License
Game is available under [GPLv3](bin/doc/COPYING) license.

More documentation about game (changelog, license) you can find in
[doc](bin/doc) directory.

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
