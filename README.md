# bLoKaML
CS 3110 (Spring 2018) Final Project
Developed by Srishti Belwariar, Kati Hsu, Sahithi Kalvakota, and Devki Trivedi

## Table of Contents
* About Game
* System Setup and Installation
* Game Instructions
* Makefile Commands
* Development Team

## About Game
bLoKaML is a OCaml terminal-based GUI game that resembles the wildly popular board game, Blokus.  Players take turns to place differnt tiles on the board.  The purpose of this two-player game bLoKaML is to have the fewest number of your tiles remaining when no remaining moves are possible.

## System Setup and Installation
#### To set up your system to play this game, please install the following:
1. OPAM (OCaml Package Manager) v1.2.2 and OCaml compiler v4.06.0: These were installed already for the purposes of this course.  However to install these, please refer to this [Installation Guide](http://www.cs.cornell.edu/courses/cs3110/2018sp/install.html).
2. OCaml's Graphics library: To install the library, run the following command: ```opam install Graphics```
	* It is highly recommended that the user runs the following command after installing the Graphics library to ensure it was installed successfully: ```opam list```.  This lists all the packages installed for the given OPAM.
3. XQuartz: This allows for the GUI to open and run on the device.  

#### To run game, please go to this game's directory on your device and run the following command: ```make play```


## Game Instructions

## Makefile Commands
* make play:  Make and run the GUI game.
* make clean: Clean up the _build directory.
* make compile: Compiles the game.
* make test: Make and run test file.



## Development Team
This project was developed by the following team members:
* [Srishti Belwariar](https://github.com/srishtibelwariar) (sb2355)
* [Kati Hsu](https://github.com/kyh24) (kyh24)
* [Sahithi Kalvakota](https://github.com/sahithi-kal) (sk2679)
* [Devki Trivedi](https://github.com/devki98) (dt395)
