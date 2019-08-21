#!/usr/bin/env sh

case $1 in
   release)
      gprclean -P steamsky.gpr
      gprbuild -p steamsky.gpr -XMode=release
      mkdir -p others/Output/release/bin
      cp -r share others/Output/release
      cp bin/steamsky others/Output/release/bin
      cp -r bin/data others/Output/release/share
      rm -r others/Output/release/share/data/mods
      rm -r others/Output/release/share/data/saves
      rm -r others/Output/release/share/data/themes
      cp -r bin/doc others/Output/release/share
      cp README.md others/Output/release/share/doc
      gprclean -P steamsky.gpr
      ;;
   debug)
      gprclean -P steamsky.gpr
      gprbuild -P steamsky.gpr
      ;;
   createtests)
      gnattest -P steamsky.gpr
      ;;
   tests)
      gprbuild -P tests/driver/test_driver.gpr
      ;;
   docs)
      ./generatedocs.py
      ;;
   windows)
      wineconsole build.cmd
      ;;
   help)
      echo "release       - Build the game in release mode"
      echo "debug         - Build the game in debug mode"
      echo "createtests   - Regenerate unit tests"
      echo "tests         - Build unit tests"
      echo "docs          - Generate code documentation"
      echo "windows       - Build the game in release mode for Windows"
      echo "help          - This screen"
      ;;
   *)
      echo "Unknown command, possible options are: release, debug, createtests, tests, docs, windows, help"
      ;;
esac
