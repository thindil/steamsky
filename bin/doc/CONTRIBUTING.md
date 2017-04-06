## Bugs reporting

Bugs are not only problems or game crashes, but typos too. If you find any bug
in game, please report it at <https://github.com/thindil/steamsky/issues> or if
you prefer, on mail <thindil@laeran.pl>

### Some general hints about reporting bugs

- In "Title" field try write very short but not too general description of
  problem. Good example: "Game crashed when entering base". Bad example: "Game
  crashes often."
- In body/comment field try write that much informations about problem as
  possible. In most cases more informations is better than less. General rule
  of good problem report is give enough informations which allow to reproduce
  problem by other people. It may be in form of steps which are needed for
  cause problem.
- If game crashed, in most cases it should create file *error.log* in *data*
  directory. It will be a lot of help if you can attach that file to bug
  report. Each bug information in this file contains: date when crash happens,
  version of game used and memory address of lines of code which caused crash.
  You can check this last information by using command `addr2line` in directory
  where *steamsky* executable file is. Example:

  `addr2line -e steamsky [here full list of memory addresses from error.log]`

  This command may not works if you use "release" version of game due to
  removed some debug informations from executable file.

### Example of bug report:

Title: "Game crashed when entering base"

Body: 

1. Dock to base
2. Open base actions menu
3. Select option "Trade" from menu with arrows keys
4. Press enter
5. Game crashing

## Features propositions

At this moment, please, don't give any propositions about new game features or
mechanic. I have my own long TODO list and your propositions can duplicate or
be against my ideas. Of course, if you really want, you can always start
discussion about new feature, just I'm afraid, it may take long time to
implement it into game.
If you want to talk/propose changes in any existing in game feature or 
mechanic, feel free to contact me via issues tracker or mail (addresses of 
both you can find at top of this file). General rule about propositions is 
same as for bugs reports - please, try write that much informations as 
possible. This help us all better understand purpose of your changes.

## Code propositions

### General informations

If you want start help in game development, please consider starts from
something easy like fixing bugs. Before you been want to add new feature to
game, please contact with me by issues tracker or mail, addresses of both are
at top of this file. Same as with features proposition - your code may
"collide" with my work and it this moment you may just lost time by working on
it. So it is better that we first discuss your proposition. In any other case,
fell free to fix my code.

### Coding standard

When you write your own code, feel free to use any coding standard you want.
But before you send your changes to project, please use command `gnatpp` which
automatically format source code to project coding standard. Proper `gnatpp`
command usage (in main project directory, where *steamsky.gpr* file is):

`gnatpp -P steamsky.gpr --incremental`

### Code submission

Preferred way to submit your code is clone repository and then open new pull
proposal at <https://github.com/thindil/steamsky/compare>. But if you prefer,
you can send your code by mail too (email address is at top of this file). In
that situation, please append to your mail patch file with changes.
