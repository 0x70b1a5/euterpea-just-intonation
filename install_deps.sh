#!/bin/bash

# This script installs dependencies for building the project
# including cross-compilation to Windows

echo "Installing dependencies for building Just Intonation Music Generator"
echo "=================================================================="

# Create cabal configuration
echo "Setting up cabal configuration..."

mkdir -p ~/.cabal

# Cabal config
cat > ~/.cabal/config << EOF
repository hackage.haskell.org
  url: http://hackage.haskell.org/
  secure: True

remote-repo-cache: $HOME/.cabal/packages
world-file: $HOME/.cabal/world
extra-prog-path: $HOME/.cabal/bin
build-summary: $HOME/.cabal/logs/build.log
remote-build-reporting: anonymous
jobs: \$ncpus
EOF

# Update cabal
echo "Updating cabal package list..."
cabal update

# Create a cabal.project file to handle PortMidi for Windows
echo "Creating cabal project configuration..."
cat > cabal.project << EOF
packages: .

package PortMidi
  flags: -PkgConfig
  extra-include-dirs: /usr/local/include
  extra-lib-dirs: /usr/local/lib
EOF

# We need to specify that we want to build both executables
echo "Creating project.local for Windows cross-compilation..."
cat > cabal.project.win << EOF
packages: .

-- Target Windows platform
target-platform: x86_64-w64-mingw32
with-compiler: x86_64-w64-mingw32-ghc
with-hc-pkg: x86_64-w64-mingw32-ghc-pkg

-- Configure PortMidi
package PortMidi
  flags: -PkgConfig
  buildable: False
  
-- Configure Euterpea to not use PortMidi
package Euterpea
  flags: -usePortMidi

-- Configure our package to not use PortMidi
package euterpea2-project
  flags: -usePortMidi
  
-- Set strip to true to reduce executable size
executable-stripping: True
library-stripping: True
EOF

echo "Dependencies configuration complete!"