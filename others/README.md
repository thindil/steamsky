Various files which are not necessary needed for development:

- steamsky.iss      - Inno Setup script for creating Windows installer of the
                      game
- AppRun            - starting script for AppImage
- steamsky-icon.png - icon for AppImage
- steamsky.desktop  - desktop file for AppImage
- robodocada.rc     - configuration script for ROBODoc to generate the code
                      documentation of the game
- robodoc.css       - CSS style for the code documentation of the game
- generatedocs.py   - script to generate code documentation. **Important:** run
                      this script from the main project directory, not from
                      this.
- build.cmd         - script used to build Windows version of the program on
                      Linux. You will probably have to change PATH variable
                      inside if you want o run it.
- build.sh          - script to build release for the program. Build the
                      program in release mode and copy all files to proper
                      location.
