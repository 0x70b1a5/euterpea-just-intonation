# Developer README: Building Windows Executables

This document explains how to build a Windows executable for the Just Intonation Music Generator.

## Option 1: Building on Windows (Recommended)

The simplest and most reliable approach is to build directly on Windows:

1. Install [GHC](https://www.haskell.org/ghcup/) on Windows:
   - Download and run the GHCup installer from https://www.haskell.org/ghcup/
   - Choose to install GHC and Cabal during setup
   - Make sure to let the installer add GHC to your PATH

2. Download official SDL2 binaries:
   - Download SDL2 from https://github.com/libsdl-org/SDL/releases/download/release-2.28.5/SDL2-2.28.5-win32-x64.zip
   - Download SDL2_ttf from https://github.com/libsdl-org/SDL_ttf/releases/download/release-2.20.2/SDL2_ttf-2.20.2-win32-x64.zip
   - Extract both to a folder (e.g., C:\SDL2)
   - Add the lib/x64 folder to your PATH environment variable

3. Configure Cabal to build SDL2 without pkgconfig:
   ```
   echo "package sdl2" > cabal.project.local
   echo "  flags: -pkgconfig" >> cabal.project.local
   echo "  extra-lib-dirs: C:/SDL2/lib/x64" >> cabal.project.local
   echo "  extra-include-dirs: C:/SDL2/include" >> cabal.project.local
   
   echo "package sdl2-ttf" >> cabal.project.local
   echo "  extra-lib-dirs: C:/SDL2/lib/x64" >> cabal.project.local
   echo "  extra-include-dirs: C:/SDL2/include" >> cabal.project.local
   ```

4. Install required Haskell dependencies:
   ```
   cabal update
   cabal install --lib UISF
   cabal install --lib HSoM
   ```

5. Clone or copy this project to the Windows machine

6. Build the project:
   ```
   cabal build
   ```

7. Create the executable:
   ```
   cabal install --installdir=dist\windows exe:euterpea2-project
   ```

8. The executable will be in `dist\windows\euterpea2-project.exe` - you can rename it to `JustIntonationMusic.exe`

9. Copy required DLL files to the same directory as the executable:
   ```
   copy C:\SDL2\lib\x64\*.dll dist\windows\
   ```

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
       
       - name: Download SDL2 libraries
         run: |
           # Create SDL directory
           mkdir -p C:/SDL2

           # Download and extract SDL2
           curl -L https://github.com/libsdl-org/SDL/releases/download/release-2.28.5/SDL2-2.28.5-win32-x64.zip -o SDL2.zip
           Expand-Archive -Path SDL2.zip -DestinationPath C:/SDL2 -Force
           
           # Download and extract SDL2_ttf
           curl -L https://github.com/libsdl-org/SDL_ttf/releases/download/release-2.20.2/SDL2_ttf-2.20.2-win32-x64.zip -o SDL2_ttf.zip
           Expand-Archive -Path SDL2_ttf.zip -DestinationPath C:/SDL2 -Force
           
           # Add SDL2 to path for build tools to find
           echo "C:/SDL2/lib/x64" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
       
       - name: Install dependencies
         env:
           SDL2_DIR: C:/SDL2
           SDL2_LIB_DIR: C:/SDL2/lib/x64
           SDL2_INCLUDE_DIR: C:/SDL2/include
           LD_LIBRARY_PATH: C:/SDL2/lib/x64
         run: |
           # Create a cabal configuration for building SDL2 without pkgconfig
           echo "package sdl2" > cabal.project.local
           echo "  flags: -pkgconfig" >> cabal.project.local
           echo "  extra-lib-dirs: C:/SDL2/lib/x64" >> cabal.project.local
           echo "  extra-include-dirs: C:/SDL2/include" >> cabal.project.local
           
           echo "package sdl2-ttf" >> cabal.project.local
           echo "  extra-lib-dirs: C:/SDL2/lib/x64" >> cabal.project.local
           echo "  extra-include-dirs: C:/SDL2/include" >> cabal.project.local
           
           # Install dependencies
           cabal install --lib UISF
           cabal install --lib HSoM
       
       - name: Build
         # The cabal.project.local file created earlier is used here
         run: cabal build
       
       - name: Create executable directory
         run: |
           mkdir dist 2>nul || echo "dist already exists"
           mkdir dist\windows 2>nul || echo "dist\windows already exists"
       
       - name: Install executable
         # The cabal.project.local file created earlier is used here
         run: cabal install --installdir=dist/windows exe:euterpea2-project
       
       - name: Rename executable
         run: |
           cd dist/windows
           ren euterpea2-project.exe JustIntonationMusic.exe
       
       - name: Copy DLL dependencies
         run: |
           # Copy all SDL2 DLLs
           dir C:\SDL2\lib\x64\
           copy C:\SDL2\lib\x64\*.dll dist\windows\
       
       - name: Copy README
         run: copy WINDOWS_README.md dist/windows/README.md
       
       - name: Upload artifacts
         uses: actions/upload-artifact@v4
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
   - Required SDL2 DLLs (from the artifacts created by GitHub Actions):
     - SDL2.dll
     - SDL2_ttf.dll
     - And any other DLLs from the SDL2 lib/x64 directory
   
2. Optional additions:
   - Example WAV files
   - Additional font files (if using custom fonts)

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