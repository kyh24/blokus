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
bLoKaML is a OCaml terminal-based GUI game that resembles the wildly popular board game, Blokus.  Players take turns to place different tiles on the board.  The purpose of this two-player game bLoKaML is to have the fewest number of your tiles remaining when no remaining moves are possible.

## System Setup and Installation
#### To set up your system to play this game, please install the following:
1. OPAM (OCaml Package Manager) v1.2.2 and OCaml compiler v4.06.0: These were installed already for the purposes of this course.  However to install these, please refer to this [Installation Guide](http://www.cs.cornell.edu/courses/cs3110/2018sp/install.html).
2. OCaml's Graphics library: To install the library, run the following command: ```opam install Graphics```
	* It is highly recommended that the user runs the following command after installing the Graphics library to ensure it was installed successfully: ```opam list```.  This lists all the packages installed for the given OPAM.
3. Windowing System:  
	* The VMs installed using the Installation Guide linked above will have a preinstalled windowing system that is compatible with this application.
	* XQuartz (for Mac OS X): This allows for the GUI to open and run on the Mac OS device. This can be installed from [here](https://www.xquartz.org)

#### To run game, please go to this game's directory on your device and run the following command:
```make play```


## Game Instructions
#### Before You Start:
* Each player has 16 polyomino pieces in a distinct color of yellow or blue. Player 1's pieces are yellow while Player 2's
pieces are blue.
* Each pieces is made up of squares. The number of squares a piece has coordinates with its value, that is, the value added
to they player's score when that piece is placed.
* The goal of the game is to put as many *squares* (not pieces) on the board as possible. The winner is the person who places the most squares on the board and has the highest score of the remaining players.


#### Playing bLoKaML using the GUI :
1. When the game is first start, players are presented with a home screen. If they wish to continue and play the game, they should press the "Play Now!" Button and be taking to the game screen. If they wish to quit the game, they should press the "Quit Game button".
2. At the start of the game, both players are given 16 polyomino pieces, and empty board and scores of 0.
3. Player 1 goes first.
4. Player 1 can selected their desired tile by clicking on that tile in their respective inventory. This then moves the tile to the player's canvas.
5. Once the desired tile is in the player's canvas, they can transform it using the "Rotate Tile", "Flip X" and "Flip Y" buttons. Each button transforms the tile on the canvas in the following ways:
	* Rotate Tile : Rotates the tile on the canvas 90 degrees clockwise.
	* Flip X : Reflects the tile on the canvas over its X axis.
	* Flip Y : Reflects the tile on the canvas over its Y axis.
6. Once the tile on the canvas is transformed as the player wants it to be, they can place the tile by matching the dot on the canvas to where they would like to place the tile. If the placement is valid, the tile will be placed on the board. If not, an error message will be display in the player's status board. The following must be true for a placement to be valid :
	* A tile can never be placed off the board, even partially.
	* A tile can never be placed over another tile so that the already placed tile is covered, even partially.
	* A tile can never touch another tile of the same color at the sides, but it can touch another tile of a different color at the sides.
	* If it is a player's first turn, the tile must be placed on their respective corner, which is marked by a dot in the player's piece color.
	* For all other turns besides the player's first turn, the placed tile must touch at least one other tile of the same color at the a corner.
7. After a player has placed a piece, their turn has ended and it it the next player's turn.
8. The game end and a winner is declared when neither player is able to have a turn. A player can continue taking turns as long as they have pieces remaining and have not pressed the "Forfeit All Future Turns" button. Note that this button can be pressed at any point in a player's turn. This immediately ends the player's turn and no longer allows them to have any future turns. This button should be used when a player wants to stop playing or can no longer see any possible valid moves they can make.
9. If at any point in the game, the players want to quit the game, they can press the "Quit" button at the center of the screen.  


## Makefile Commands
* ```make play``` :  Make and run the GUI game.
* ```make clean``` : Clean up the _build directory.
* ```make compile``` : Compiles the game.
* ```make test``` : Make and run test file.



## Development Team
This project was developed by the following team members:
* [Srishti Belwariar](https://github.com/srishtibelwariar) (sb2355)
* [Kati Hsu](https://github.com/kyh24) (kyh24)
* [Sahithi Kalvakota](https://github.com/sahithi-kal) (sk2679)
* [Devki Trivedi](https://github.com/devki98) (dt395)
