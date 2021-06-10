Various files which are not necessary needed for development:

* robodocada.rc     - configuration script for ROBODoc to generate the code
                      documentation of the game
* robodoc.css       - CSS style for the code documentation of the game
* generatedocs.tcl  - script to generate code documentation. **Important:** run
                      this script from the main project directory, not from
                      this.
* build.tcl         - script to build the stable release for the game. Build
                      the game in release mode and copy all files to the proper
                      location. **Important**: run this script from the main
                      project directory, not from this.
* tests.tcl         - script to run unit tests X times. **Important**: run this
                      script from the main project directory, not from this.
                      If you don't enter amount how many times repeat tests,
                      they will run only once.
* rules.aru         - configuration file with rules for check code with
                      AdaControl.
* check.tcl         - script to run AdaControl check for the code. **Important:**
                      run this script from the main project directory, not
                      from here.
