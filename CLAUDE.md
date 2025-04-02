# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- Build: `cabal build`
- Run: `cabal run`
- Create WAV file: `cabal run`
- Clean: `cabal clean`

## Code Style Guidelines

- Use 2-space indentation for Haskell code
- Always include type signatures for top-level functions
- Module imports should be grouped: standard library first, then Euterpea, then local modules
- Use the Arrow language extension for signal functions
- Prefer sectioned operators (`(+) x y` instead of `x + y`) for point-free style
- Document ratios with comments indicating interval names
- Limit line length to 80 characters when possible
- Place TraceMessages in debug scenarios only
- Use meaningful names for musical values (e.g., `majorChord` not `mc`)
- Represent musical pitches using the `j` function with base frequency and ratio
- Comment complex musical compositions to indicate structure