Various files which are not necessary needed for development:

- steamsky.iss      - Inno Setup script for creating Windows installer of the
                      game
- AppRun            - starting script for AppImage
- steamsky-icon.png - icon for AppImage
- steamsky.desktop  - desktop file for AppImage
- robodocada.rc     - configuration script for ROBODoc to generate the code
                      documentation of the game
- robodoc.css       - CSS style for the code documentation of the game
- generatedocs.tcl  - script to generate code documentation. **Important:** run
                      this script from the main project directory, not from
                      this.
- build.cmd         - script used to build Windows version of the game. Build
                      the game in release mode and copy all files to the
                      proper location. **Important**: run this script from
                      the main project directory, not from this.
- build.sh          - script to build Linux release for the game. Build the
                      game in release mode and copy all files to the proper
                      location. **Important**: run this script from the main
                      project directory, not from this.
- tests.sh          - script to run unit tests X times on Linux. **Important**:
                      run this script from the main project directory, not
                      from this. If you don't enter amount how many times
                      repeat tests, they will run only once.
- steamsky.appdata.xml - AppStream meta data for the AppImage.
