#!/usr/bin/env sh

case $1 in
   release)
      releasedir="usr"
      gprclean -P steamsky.gpr
      gprbuild -p steamsky.gpr -XMode=release
      mkdir -p "$releasedir"/bin
      cp -r share "$releasedir"
      cp bin/steamsky "$releasedir"/bin
      cp -r bin/data "$releasedir"/share
      rm -r "$releasedir"/share/data/mods
      rm -r "$releasedir"/share/data/saves
      rm -r "$releasedir"/share/data/themes
      cp -r bin/doc "$releasedir"/share
      cp README.md "$releasedir"/share/doc
      gprclean -P steamsky.gpr
      ;;
   debug)
      gprclean -P steamsky.gpr
      gprbuild -P steamsky.gpr
      ;;
   analyze)
      gprclean -P steamsky.gpr
      gprbuild -p steamsky.gpr -XMode=analyze
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
      echo "analyze       - Build the game in analyze mode for gcov and gprof"
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
