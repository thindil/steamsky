## General Info

Steam Sky is (or better, will be) opensource roguelike steampunk game. Your
role is to command flying ship with crew in sky, traveling between floating
bases, fighting with enemies, trade in goods, etc. Game is in very early stage
of development, so at this moment most functions are not implemented yet. Now
game is available (and tested) only on Linux systems.

## Build game

At this moment here no available binary packages, only source code. To build
it, you need any Ada language compiler (GCC with enabled Ada support or GNAT), 
gprbuild program and ncurses Ada bingind (should be available in most
distributions or with ncurses package or as stand alone package). If you have all, 
in main source code directory type: `gprbuild steamsky.gpr` for debug mode build 
or for release mode: `gprbuild steamsky.gpr -XMode=release`

## Running game
To run game need only ncurses library, available in all Linux distribution.
Enter *bin* directory and type `./steamsky`. Game works only in terminal.

That's all for now, as usual, probably I forgot about something important ;)

Bartek thindil Jasicki
