#!/bin/bash

# This script installs SDL2 and SDL2_ttf dependencies for the 
# Just Intonation Tracker application.

check_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$NAME
        echo "Detected OS: $OS"
    elif [ "$(uname)" == "Darwin" ]; then
        OS="macOS"
        echo "Detected OS: macOS"
    elif [ "$OSTYPE" == "msys" ] || [ "$OSTYPE" == "win32" ]; then
        OS="Windows"
        echo "Detected OS: Windows"
    else
        echo "Unknown operating system. Please install SDL2 and SDL2_ttf manually."
        exit 1
    fi
}

install_dependencies() {
    case $OS in
        *"Ubuntu"*|*"Debian"*)
            echo "Installing SDL2 and SDL2_ttf for Ubuntu/Debian..."
            sudo apt-get update
            sudo apt-get install -y libsdl2-dev libsdl2-ttf-dev
            ;;
        *"Fedora"*|*"Red Hat"*)
            echo "Installing SDL2 and SDL2_ttf for Fedora/RHEL..."
            sudo dnf install -y SDL2-devel SDL2_ttf-devel
            ;;
        *"Arch"*)
            echo "Installing SDL2 and SDL2_ttf for Arch Linux..."
            sudo pacman -Sy --noconfirm sdl2 sdl2_ttf
            ;;
        "macOS")
            echo "Installing SDL2 and SDL2_ttf for macOS..."
            if ! command -v brew &> /dev/null; then
                echo "Homebrew not found. Please install Homebrew first:"
                echo "Visit https://brew.sh/ for instructions"
                exit 1
            fi
            brew install sdl2 sdl2_ttf
            ;;
        "Windows")
            echo "For Windows, please download the SDL2 and SDL2_ttf libraries manually:"
            echo "1. Visit https://www.libsdl.org/download-2.0.php"
            echo "2. Download and install SDL2 for Windows"
            echo "3. Visit https://github.com/libsdl-org/SDL_ttf/releases"
            echo "4. Download and install SDL2_ttf for Windows"
            ;;
        *)
            echo "Unsupported operating system: $OS"
            echo "Please install SDL2 and SDL2_ttf manually."
            exit 1
            ;;
    esac
}

check_os
install_dependencies

echo "Dependencies installed successfully!"
echo "You can now build and run the Just Intonation Tracker application."