# Developer README: Building Windows Executables

This document explains how to build a Windows executable for the Just Intonation Music Generator.

## Option 1: Building on Windows (Recommended)

The simplest and most reliable approach is to build directly on Windows:

1. Install [GHC](https://www.haskell.org/ghcup/) on Windows:
   - Download and run the GHCup installer from https://www.haskell.org/ghcup/
   - Choose to install GHC and Cabal during setup
   - Make sure to let the installer add GHC to your PATH

2. Install required dependencies:
   ```
   cabal update
   cabal install --lib Euterpea
   cabal install --lib UISF
   cabal install --lib HSoM
   ```

3. Clone or copy this project to the Windows machine

4. Build the project:
   ```
   cabal build
   ```

5. Create the executable:
   ```
   cabal install --installdir=dist\windows exe:euterpea2-project
   ```

6. The executable will be in `dist\windows\euterpea2-project.exe` - you can rename it to `JustIntonationMusic.exe`

## Option 2: Cross-compiling from Linux

For cross-compilation from Linux to Windows:

1. Install the required tools:
   ```
   # Install mingw-w64 (Windows cross-compiler toolchain)
   sudo apt install mingw-w64
   
   # Install wine for testing
   sudo apt install wine
   ```

2. Set up GHC for cross-compilation:
   ```
   # Install cabal-install if you don't have it
   sudo apt install cabal-install
   
   # Update cabal
   cabal update
   
   # Install cabal-plan tool
   cabal install cabal-plan
   ```

3. Modify the cabal.project file to add Windows target:
   ```
   echo "packages: ." > cabal.project.local
   echo "with-compiler: ghc" >> cabal.project.local
   echo "target-platform: x86_64-w64-mingw32" >> cabal.project.local
   ```

4. Build the project:
   ```
   cabal build
   ```

5. Find the executable in dist-newstyle directory:
   ```
   find dist-newstyle -name "*.exe" -type f
   ```

6. Copy the executable to a distribution folder:
   ```
   mkdir -p dist/windows
   cp $(find dist-newstyle -name "*.exe" -type f | head -1) dist/windows/JustIntonationMusic.exe
   ```

## Option 3: Using a Virtual Machine

If cross-compilation is challenging:

1. Install VirtualBox or VMware
2. Create a Windows VM
3. Install GHC on the VM
4. Copy the source code to the VM
5. Build from within the VM

## Option 4: Using GitHub Actions (Easiest)

You can use GitHub Actions to easily build Windows executables without needing Windows or setting up cross-compilation:

1. Push your code to a GitHub repository

2. Create a file `.github/workflows/windows-build.yml` with the following content:
   ```yaml
   name: Build Windows Executable

   on:
     push:
       branches: [ main, master ]
     pull_request:
       branches: [ main, master ]
     workflow_dispatch:  # Allows manual triggering

   jobs:
     build:
       runs-on: windows-latest
       steps:
       - uses: actions/checkout@v3
       
       - name: Setup Haskell
         uses: haskell-actions/setup@v2
         with:
           ghc-version: '9.4.8'
           cabal-version: '3.10.2.0'
       
       - name: Update Cabal package list
         run: cabal update
       
       - name: Install dependencies
         run: |
           cabal install --lib Euterpea
           cabal install --lib UISF
           cabal install --lib HSoM
       
       - name: Build
         run: cabal build
       
       - name: Create executable directory
         run: mkdir -p dist/windows
       
       - name: Install executable
         run: cabal install --installdir=dist/windows exe:euterpea2-project
       
       - name: Rename executable
         run: |
           cd dist/windows
           ren euterpea2-project.exe JustIntonationMusic.exe
       
       - name: Copy README
         run: copy WINDOWS_README.md dist/windows/README.md
       
       - name: Upload artifacts
         uses: actions/upload-artifact@v3
         with:
           name: JustIntonationMusic
           path: dist/windows/
   ```

3. Go to your GitHub repository and:
   - Click on the "Actions" tab
   - Select the "Build Windows Executable" workflow
   - Click "Run workflow"

4. After the workflow completes, you can download the built executable from the "Artifacts" section

## Creating a Distributable Package

For your friend:

1. Create a ZIP file containing:
   - The executable (JustIntonationMusic.exe)
   - The WINDOWS_README.md file (rename to README.md)
   
2. Optional additions:
   - Example WAV files
   - Any DLLs required for Windows (may be needed for audio)

## Testing on Windows

Before distributing:

1. Test the executable on a clean Windows install
2. Verify it runs without installing Haskell or other dependencies
3. Check that it successfully creates and plays WAV files
4. Test on various Windows versions (10, 11) if possible

## Troubleshooting Windows Builds

Common issues:

1. **Missing DLLs**: If the executable fails with missing DLL errors, use a tool like `Dependency Walker` to identify which DLLs are missing and include them in your distribution.

2. **Permission Issues**: Ensure the executable has the correct permissions.

3. **Anti-virus Blocking**: Some anti-virus software may block newly compiled executables. Consider signing the executable or adding instructions for users.

4. **Audio Device Access**: Confirm the executable can access audio devices on Windows.

## Extending the Application

For future enhancements:

1. Add a proper GUI using a cross-platform toolkit like wxWidgets or Qt
2. Include playback capabilities directly in the application
3. Add visual representations of the music (waveforms, notation)
4. Create a composition editor with more options

## Resources

- [Haskell on Windows](https://www.haskell.org/platform/windows.html)
- [Cabal User Guide](https://cabal.readthedocs.io/en/stable/index.html)
- [Cross-compiling Haskell](https://gitlab.haskell.org/ghc/ghc/-/wikis/cross-compilation)
- [Euterpea Documentation](http://euterpea.com/)