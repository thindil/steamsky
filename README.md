## General Info

Steam Sky is open source, roguelike game in steampunk theme. Your role is to 
command flying ship and its crew, traveling between floating bases, fighting 
with enemies, trade in goods, etc. Here are no end goal for game, you can play
as long as your character not die. Game is under heavy development, but 
generally is playable. Now game is available (and tested) only on Linux 
64-bit.

## Game versions
At this moment are available 2 game versions:
- 1.0.x: "stable" version of game. This version receive only fixes of bug but
  no new features. Source code for this version is in *1.0* branch.
- 1.x: "development" version of game, future version 2.0. This is main place
  where game development happen. It often break save compatibilities between
  releases so use at your own risk. Source code for this version is in *master*
  branch.

## Build game from sources
To build it, you need:

* compiler - GCC with enabled Ada support or GNAT from: 
  http://libre.adacore.com/download/

* ncurses Ada binding (should be available in most distributions or with ncurses 
  package or as standalone package). If not, you can download it from:
  http://invisible-island.net/ncurses/ncurses-Ada95.html

* optional, but highly recommended:  gprbuild program - should be available in most 
  distributions, if not, download from: http://libre.adacore.com/download/


If you have all, in main source code directory (where this file is) type: 

* if you don't have gprbuild: `gnatmake -Psteamsky.gpr` for debug build or for
  release version: `gnatmake -Psteamsky.gpr -XMode=release`

* if you have gprbuild: `gprbuild` for debug mode build or for release mode: 
  `gprbuild -XMode=release`


## Running game
To run game need only ncurses library, available in all Linux distribution.
Enter *bin* directory (if you build game from sources) or in main game 
directory (if you use released binary) and type `./steamsky`. Game works 
only in terminal.

## Modify game
For detailed informations about modifying various game elements or debugging
game, see [MODDING.md](bin/doc/MODDING.md)

## Contributing to project
For detailed informations about contributing to project (bugs reporting, ideas
propositions, code conduct, etc), see [CONTRIBUTING.md](bin/doc/CONTRIBUTING.md)


More documentation about game (changelog, license) you can find in
[doc](bin/doc) directory.

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
