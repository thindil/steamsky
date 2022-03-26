Various files which are related to AdaControl checks:

* rules.aru  - configuration file with rules for check code with AdaControl.
               All rules in one file. Running it can take a lot of time
* rules1.aru - configuration file with rules for check code with AdaControl.
               Contains only rules related to the syntax checking.
* rules2.aru - configuration file with rules for check code with AdaControl.
               Contains rules related to the code checking.
* rules3.aru - configuration file with rules for check code with AdaControl.
               Contains only rule Global_References as it is the longest to
               check.
* check.tcl  - script to run AdaControl check for the code. **Important:**
               run this script from the main project directory, not
               from here.
