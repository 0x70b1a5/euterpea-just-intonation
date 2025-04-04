#!/bin/bash
# Build script for Windows executable with web interface

echo "Building Windows executable with web interface..."
# Use our cross-compilation configuration
cp cabal.project.win cabal.project.local

# Build with specific flags for Windows
cabal build --enable-shared --enable-executable-dynamic test-web

# Copy the executable to the root folder for easy access
cp $(find dist-newstyle -name "test-web.exe" -type f | head -n 1) ./just-intonation-tracker-web.exe

echo "Build complete! Run just-intonation-tracker-web.exe to start the application."
echo "The web interface will open automatically in your default browser."