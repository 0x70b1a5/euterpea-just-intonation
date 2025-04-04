#!/bin/bash

# This script builds a Windows executable for the Just Intonation Music Generator

echo "Building Windows executable for Just Intonation Music Generator"
echo "=============================================================="

# Check for required tools
if ! command -v cabal &> /dev/null; then
    echo "Error: cabal is not installed. Please install it with:"
    echo "  sudo apt install cabal-install"
    exit 1
fi

if ! command -v x86_64-w64-mingw32-gcc &> /dev/null; then
    echo "Error: MinGW is not installed. Please install it with:"
    echo "  sudo apt install mingw-w64"
    exit 1
fi

# Setup cabal project file for cross-compilation
echo "Setting up cross-compilation..."
cp cabal.project.win cabal.project.local

# Build the project
echo "Building for Windows target..."
cabal build --enable-shared --enable-executable-dynamic euterpea2-project

# Check if build succeeded
if [ $? -ne 0 ]; then
    echo "Build failed. Trying native build instead..."
    rm cabal.project.local
    cabal build
    echo ""
    echo "Native executable built successfully!"
    echo "To run the application: cabal run"
    echo ""
    echo "For a Windows build, please see DEVELOPER_README.md for other options."
    exit 1
fi

# Create distribution directory
echo "Creating distribution package..."
mkdir -p dist/windows

# Find the executable
EXE_PATH=$(find dist-newstyle -name "*.exe" -type f | head -1)

if [ -z "$EXE_PATH" ]; then
    echo "Error: Could not find the Windows executable."
    echo "The build may have succeeded but the .exe file was not created."
    echo "Please see DEVELOPER_README.md for alternative build methods."
    exit 1
fi

# Copy the files to the distribution directory
cp "$EXE_PATH" dist/windows/JustIntonationMusic.exe
cp WINDOWS_README.md dist/windows/README.md

# Check if wine is installed for testing
if command -v wine &> /dev/null; then
    echo "You can test the executable with: wine dist/windows/JustIntonationMusic.exe"
fi

echo ""
echo "Windows executable built successfully!"
echo "The executable is located at: dist/windows/JustIntonationMusic.exe"
echo ""
echo "Create a ZIP file with these contents to distribute to your friend."