# PianoMusicGenerator
This repository contains a minimal web application built using the Yesod web framework for Haskell. 

## Members:
- Marina Seheon
- Jack Vanlyssel
- Joseph Barrela

## Prerequisites
- Haskell Stack
- GHC (Glasgow Haskell Compiler) - Installed automatically by Stack if not already present.

## Getting Started
1. clone the repo
   ```
   git clone https://github.com/your-username/yesod-hello-world.git
   cd yesod-hello-world
   ```
2. Build and setup the project
   ```
   stack setup
   stack build
   ```
3. Run app
   ```
   stack run
   ```
4. Access the Application
   open a web browser and navigate to http://localhost:3000/. You should see "Hello, World!" displayed on the page.
   
## Modifying the Application
To modify the main handler, check out src/Handler/Home.hs. This is where the response for the root path (/) is defined.
