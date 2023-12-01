# PianoMusicGenerator
This repository contains a minimal web application built using the Yesod web framework for Haskell. 

## Members:
- Marina Seheon
- Jack Vanlyssel
- Joseph Barela

## Website 
Enjoy playing on the piano and generating music!
A piano is generated that you can play as well as a music generator where you can pick the time signature, beats per minute, and key signature.

## Prerequisites
- Haskell Stack
- GHC (Glasgow Haskell Compiler) - Installed automatically by Stack if not already present.

## Getting Started
1. clone the repo
   ```
   git clone https://github.com/seheonm/PianoMusicGenerator.git
   cd PianoMusicGeneratorWebApp
   ```
2. Build and setup the project
   ```
   stack setup
   stack build
   ```
   This should take a while the first time you run these commands.
3. Run app
   ```
   stack run
   ```
4. Access the Application
   open a web browser and navigate to http://localhost:3000/. You should see "Hello, World!" displayed on the page.
5. Stop app
   Ctrl + c in terminal.
   
## Modifying the Application
To modify the main handler, check out src/Handler/Home.hs. This is where the response for the root path (/) is defined.

## Euterpea Library 

- Install Euterpea from https://www.euterpea.com/ and follow the instructions
- In vscode, type 
   ```
   stack install Euterpea
   ```
- Build the project again
   ```
   stack clean
   stack build
   ```
- Run app
   ```
   stack run
   ```

## Possible bugs
- The yesod uses a library called warp that by default handles the threading by detecting how many cores your machine has and setting that number to the number of threads it uses. Getting a net::ERR_CONNECTION_REFUSED error means that the local server on some machines cannot handle the amount of server requests they are getting. This is a hardware error that we can’t solve at this point.