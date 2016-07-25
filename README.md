## General Info

Steam Sky is (or better, will be) open source roguelike steampunk game. Your
role is to command flying ship with crew in sky, traveling between floating
bases, fighting with enemies, trade in goods, etc. Game is in very early stage
of development, so at this moment most functions are not implemented yet. Now
game is available (and tested) only on Linux 64-bit systems.

## Build game

To build it, you need:

* Any Ada language compiler, for example GCC with enabled Ada support or GNAT: 
  http://libre.adacore.com/download/

* gprbuild program - should be available in most distributions, if not, download
  from: http://libre.adacore.com/download/

* ncurses Ada binding (should be available in most distributions or with ncurses 
  package or as standalone package). If not, you can download it from:
  http://invisible-island.net/ncurses/ncurses-Ada95.html

If you have all, in main source code directory type: `gprbuild steamsky.gpr` for debug mode build 
or for release mode: `gprbuild steamsky.gpr -XMode=release`

## Running game
To run game need only ncurses library, available in all Linux distribution.
Enter *bin* directory (if you build game from sources) or in main game directory (if you use 
released binary) and type `./steamsky`. Game works only in terminal.

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
