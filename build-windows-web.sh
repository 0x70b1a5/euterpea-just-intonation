#!/bin/bash
# Build script for Windows executable with web interface

echo "Building Windows executable with web interface..."
cabal build --with-ghc=x86_64-w64-mingw32-ghc --with-ghc-pkg=x86_64-w64-mingw32-ghc-pkg test-web

# Copy the executable to the root folder for easy access
cp $(find dist-newstyle -name "test-web.exe" -type f | head -n 1) ./just-intonation-tracker-web.exe

echo "Build complete! Run just-intonation-tracker-web.exe to start the application."
echo "The web interface will open automatically in your default browser."