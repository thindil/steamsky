## General Info

Steam Sky is open source roguelike steampunk game. Your role is to command flying 
ship with crew in sky, traveling between floating bases, fighting with enemies, trade in 
goods, etc. Game is in early stage of development, so at this moment most functions 
are not implemented yet. Now game is available (and tested) only on Linux 64-bit and 
Raspberry Pi with 32-bit Linux.

## Build game

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

If you want to run game in debug mode (some data will be logged to file
*data/debug.log*) type `./steamsky --debug=everything`.

## Modify game
For detailed informations about modifying various game elements, see
[MODDING.md](bin/docs/MODDING.md)

## Contributing to project
For detailed informations about contributing to project (bugs reporting, ideas
propositions, code conduct, etc), see [CONTRIBUTING.md](bin/docs/CONTRIBUTING.md)


That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
