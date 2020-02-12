## Bugs reporting

Bugs are not only problems or game crashes, but typos too. If you find any bug
in the game, please report it at <https://github.com/thindil/steamsky/issues>
or if you prefer, on mail <thindil@laeran.pl>

### Some general hints about reporting bugs

- In the "Title" field try to write very short but not too general description
  of the problem. Good example: "Game crashed when entering base". Bad example:
  "Game crashes often."
- In the body/comment field try to write as much information about the problem
  as possible. In most cases, more information is better than less. General
  rule of good problem report is to give enough information which allow to
  reproduce the problem by other people. It may be in form of steps which are
  needed for recreate this problem.
- If the game crashed, in most cases it should create file *error.log* in
  *data* directory. It will be a lot of help if you can attach that file to the
  bug report. Each bug information in this file contains: date when the crash
  happens, version of the game used, the source code file and line in this file.
  If game can't discover the source code file, it write memory address instead.
  You can check this last information by using command `addr2line` in the
  directory where *steamsky* executable file is. Example:

  `addr2line -e steamsky [here full list of memory addresses from error.log]`

### Example of bug report:

Title: "Game crashed when entering base"

Body:

1. Dock to the base
2. Open the base actions menu
3. Select option "Trade" from the menu with arrows keys
4. Press enter
5. Game crashing

## Features propositions

At this moment, please, don't give any propositions about new game features or
mechanic. I have my own long TODO list and your propositions can duplicate or
be against my ideas. Of course, if you really want it, you can always start
discussion about new feature, just I'm afraid, it may take long time to
implement it into the game.
If you want to talk/propose changes in any existing in the game feature or
mechanic, feel free to contact me via issues tracker or mail (addresses of
both you can find at top of this file). General rule about propositions is
same as for bugs reports - please, try to write that much information as
possible. This help us all better understand purpose of your changes.

## Code propositions

### General information

If you want start help in the game development, please consider starts from
something easy like fixing bugs. Before you been want to add new feature to
the game, please contact with me by issues tracker or mail, addresses of both are
at top of this file. Same as with features proposition - your code may
"collide" with my work and in this moment you may just lost time by working on
it. So it is better that we first discuss your proposition. In any other case,
fell free to fix my code.

### Coding standard

When you write your own code, feel free to use any coding standard you want.
But before you send your changes to the project, please use command `gnatpp`
which automatically format the source code to the project coding standard.
Proper `gnatpp` command usage (in the main project directory, where
*steamsky.gpr* file is):

`gnatpp -P steamsky.gpr`

#### Code comments formatting

The game uses [ROBODoc](https://rfsber.home.xs4all.nl/Robo/) for generating
code documentation. When you write your own code, please add proper header
documentation to it. If you use Vim/NeoVim, easiest way is to use plugin
[RoboVim](https://github.com/thindil/robovim). Example of documentation
header:

    1 -- ****f* Utils/GetRandom
    2 -- FUNCTION
    3 -- Return random number from Min to Max range
    4 -- PARAMETERS
    5 -- Min - Starting value from which generate random number
    6 -- Max - End value from which generate random number
    7 -- RESULT
    8 -- Random number between Min and Max
    9 -- SOURCE
    10 function GetRandom(Min, Max: Integer) return Integer;
    11 -- ****

1 - Documentation header. Steam Sky uses `-- ****[letter]* [package]/[itemname]`
format for documentation headers.

2-9 - Documentation. For all available options, please refer to ROBODoc
documentation. Steam sky uses `-- ` for start all documentation lines.

10 - Source code of item.

11 - Documentation footer. Steam Sky uses `-- ****` for closing documentation.

How to generate the code documentation is described in main *README.md* file.

### Code submission

Preferred way to submit your code is clone repository and then open new pull
proposal at <https://github.com/thindil/steamsky/compare>. But if you prefer,
you can send your code by mail too (email address is at top of this file). In
that situation, please append to your mail the patch file with changes.

## Additional debugging options

### Code analysis

To enable check for `gcov` (code coverage) and `gprof` (code profiling) compile
the game with mode `analyze` (in the main project directory, where
*steamsky.gpr* file is):

`gprbuild -XMode=analyze`

Or, if you prefer use `build.sh` command:

`./build.sh analyze`

More information about code coverage and profiling, you can find in the proper
documentation for both programs.

#### Generating reports

After running the game in `analyze` mode, you can generate reports by using
`build.sh` command:

- `./build.sh gcov` for generate report for the code coverage
- `./build.sh gprof` for generate report for the code profiling

### Generating unit tests

To generate (or regenerate) unit tests use command `gnattest` which generate
skeletons code for tests units (in the main project directory, where
*steamsky.gpr* file is):

`gnattest -P steamsky.gpr`

Tests are generated only for this subprograms which have explicitly declared
tests cases in declarations. Thus if here are no tests cases declared in the
game code, there will be no unit tests generated.

### Running unit tests

First, you must build all tests. How to do it, is described in main
*README.md* file. Then, in console type: `./run.sh tests`

More information about GnatTest (how to create unit test, etc.) you can find
[here](http://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/gnat_utility_programs.html#the-unit-test-generator-gnattest).
