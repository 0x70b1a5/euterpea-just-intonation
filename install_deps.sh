#!/bin/bash

# This script installs dependencies for building the project
# including cross-compilation to Windows

echo "Installing dependencies for building Just Intonation Music Generator"
echo "=================================================================="

# Install Haskell packages
echo "Installing Haskell dependencies..."
cabal update
cabal install --lib Euterpea
cabal install --lib HSoM
cabal install --lib UISF

# Setup Cabal config for cross-compilation
echo "Setting up cross-compilation environment..."
mkdir -p ~/.cabal
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

-- Windows cross-compilation options
executable-stripping: True
library-stripping: True
EOF

echo "Dependencies installed successfully!"