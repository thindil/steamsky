## Bugs reporting

Bugs and game crashes are not the only problems, but typos too. If you find any bugs
in the game, please report it at <https://github.com/thindil/steamsky/issues>
or if you prefer, on mail <thindil@laeran.pl>

### Some general hints about reporting bugs

- If you use Github issues, it is recommended to use the bug report template.
  It contains advice for the desired information included in the report.
- In the "Title" field try to write short but not a too vague description
  of the problem. Good example: "Game crashed when entering base". Bad example:
  "Game crashes often."
- In the body/comment field try to write as much informations about the problem
  as possible. In most cases, more informations is better than less. General
  rule of a good problem report is to give enough information to allow other people
  to reproduce the problem. It may be in the form of the steps which are
  needed for recreating this problem.
- If the game crashed, in most cases it should create file *error.log* in
  *data* directory. It will be a lot of help if you can attach that file to the
  bug report. Every bug information in this file contains: Date when the crash
  occured, version of the game used, the source code file and line in this file.
  If game can't discover the source code file, it write memory address instead.
  You can check this last information by using command `addr2line` in the
  directory where *steamsky* executable file is. Example:

  `addr2line -e steamsky [here full list of memory addresses from error.log]`

### Example of bug report:

Title: "Game crashed when entering a base"

Body:

1. Dock to the base
2. Open the base actions menu
3. Select option "Trade" from the menu with arrows keys
4. Press enter
5. Game crashed

## Features propositions

At this moment, please, don't give any propositions about new game features or
mechanics. I have my own long TODO list and your propositions may be duplicates or
go against my ideas. Of course, if you really want it, you can always start
discussion about a new feature, just I'm afraid, it may take a long time to
implement it into the game.

If you want to talk/propose changes to any existing features/mechanics in the game, 
feel free to contact me via issues tracker or mail (addresses that
you can find at top of this file). General rule about propositions is
same as for bugs reports - please, try to write as much information as
possible. This help us better understand the purpose of your changes.

List of things which I wish to add to the game, can be found [here](https://github.com/thindil/steamsky/projects/1)
Please read carefully the description on how to discuss or how they will be
implemented in the game.

## Code propositions

### General informations

If you want start helping in the development of the gane, please consider starting with
something easy like fixing bugs. Before you begin to add new feature to
the game, please contact with me by issues tracker or mail, addresses are
at top of this file. Same as with features proposition - your code may
"collide" with my work and in this moment you may just lose time by working on
it. So it is better that we first discuss your proposition. In any other case,
fell free to fix and or improve my code.

### Coding standard

When you write your own code, feel free to use any coding standard you want.
But before you send your changes to the project, please use command `gnatpp`
which automatically format the source code to the project coding standard.
Proper `gnatpp` command usage (in the main project directory, where
*steamsky.gpr* file is):

`gnatpp -P steamsky.gpr`

and for unit tests:

`gnatpp -P tests/prettytests.gpr`

If you prefer (and you have installed), use [Bob](https://github.com/thindil/bob):

`bob pretty`

for format the game and unit tests code.

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

The Preferred way to submit your code is to clone repository and then open new pull
proposal at <https://github.com/thindil/steamsky/compare>. But if you prefer,
you can send your code by mail too (email address is at top of this file). In
that situation, please append to your mail the patch file with changes.

## Additional debugging options

### Code analysis

To enable check for `gcov` (code coverage) and `gprof` (code profiling) compile
the game with mode `analyze` (in the main project directory, where
*steamsky.gpr* file is):

`gprbuild -XMode=analyze`

or, if you prefer (and you have installed), use [Bob](https://github.com/thindil/bob):

`bob analyze`

More informations about code coverage and profiling, you can find in the proper
documentation for both programs.

#### Generating reports

After running the game in `analyze` mode, you can generate reports by using
command:

`gprof bin/steamsky gmon.out` for generate report for the code profiling

or, if you prefer (and you have installed), use [Bob](https://github.com/thindil/bob):

`bob gprof`

### Generating unit tests

To generate (or regenerate) unit tests use command `gnattest` which generate
skeletons code for tests units (in the main project directory, where
*steamsky.gpr* file is):

`gnattest -P steamsky.gpr`

or, if you prefer (and you have installed), use [Bob](https://github.com/thindil/bob):

`bob createtests`

Tests are generated only for this subprograms which have explicitly declared
tests cases in declarations. Thus if here are no tests cases declared in the
game code, there will be no unit tests generated.

### Running unit tests

First, you must build all tests. How to do it, is described in main
*README.md* file. Then, in console, in the main project directory, type:
`others/tests.sh [amount]`

or, if you prefer (and you have installed), use [Bob](https://github.com/thindil/bob):

`bob tests [amount]`

Where `[amount]` is how many times the tests should be run. It is recommended
to run them few times in a row to catch all problems. Tests will stops if there
will be any problem. At this moment unit tests are available only on Linux.

More information about GnatTest (how to create unit test, etc.) you can find
[here](http://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/gnat_utility_programs.html#the-unit-test-generator-gnattest).
