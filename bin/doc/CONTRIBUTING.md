## Bugs reporting

Bugs and game crashes are not the only problems, but typos too. If you find any bugs
in the game, please report it at options available at [contact page](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=Contact).

### Some general hints about reporting bugs

* In the "Title" field try to write short but not a too vague description
  of the issue. Good example: "Game crashed when entering base". Bad example:
  "Game often crashes."
* In the body/comment field try to write as much information about the issue
  as possible. In most cases, more information is better than less. General
  rule of a good report is to give enough information to allow other people
  to reproduce the issue. It may be in the form of the steps which are
  needed for recreating this issue.
* If the game crashed, in most cases it should save the game. It will be a lot
  of help if you can attach that file to the bug report. Also, it will be
  helpfull, if you attach copied and pasted information contained in error report
  window from "Technical information" section or attach `error.log` file from the
  game's directory with saved games.

### Example of bug report:

Title: "Game crashed when entering a base"

Body:

1. Dock to the base
2. Open the base actions' menu
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
feel free to contact with me via options available at [contact page](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=Contact).
General rule about propositions is same as for bugs reports - please, try to
write as much information as possible. This help us better understand the
purpose of your changes.

List of things which I wish to add to the game, can be found [here](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=To-Do)
Please read carefully the description on how to discuss or how they will be
implemented in the game.

## Code propositions

### General information

If you want start helping in the development of the gane, please consider starting with
something easy like fixing bugs. Before you begin to add new feature to
the game, please contact with me options available at [contact page](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=Contact).
Same as with features proposition - your code may "collide" with my work and
at this moment you may just lose time by working on it. So it is better that
we first discuss your proposition. In any other case, fell free to fix and or
improve my code.

### Coding standard

The full description of coding style used by the project, you can find on the
[Coding Standard](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=Coding%20Standard) page.
On the page [Testing the Project](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=Testing%20the%20Project) you will
find information how to test your code, so it will be compliant with the
project standards.

### Code submission
A preferred way to submit your code is to use [tickets](https://www.laeran.pl.eu.org/repositories/steamsky/ticket)
on the project page. Please attach to that ticket file with diff changes,
the best if done with command `fossil patch`. Another diff program will
work too. In that situation, please add information which program was used to
create the diff file. If you prefer you can also use other options from
[contact page](https://www.laeran.pl.eu.org/repositories/steamsky/wiki?name=Contact).

## Additional debugging options

### Running unit tests

In console, in the main project directory, type: `others/tests.nims [amount]`

The `[amount]` is how many times the tests should be run. It is recommended
to run them few times in a row to catch all problems. Tests will stop if there
will be any issues. At this moment unit tests are available only on Linux.
