# duet-haskell

Members: Jiachen Chen, Yiran Chen, Peizhen Wu

## Introduction

“With air tight controls and gameplay that’s tuned to perfection Duet provides the perfect balance between challenge and pure gaming satisfaction.” -- Duet Game. 

We would like to implement a simplified version of the [Duet Game](https://apps.apple.com/us/app/duet-game/id634235735), developed by [Kumobius Games](https://apps.apple.com/us/developer/kumobius/id449069247), in Haskell. The Duet-Haskell application is a single player game where players control two colorured orbs to dodge randomly incoming obstacles.

The stages will be generated from predefined configuration files. Once the player starts the game, the application takes the shapes and speed of the obstacles from the configuration file, adds randomness, and renders the scene. We will provide multiple configuration files with different difficulty levels that the player can start with.

![Original Duet Game screenshot](/duet_game.jpg)

## Gameplay

The rules are simple: control two orbs in sync, survive against all obstacles and earn as many points as possible.

1. Players will hold “A” and “D” (or self-defined keys in configuration file) to control the rotation of two orbs simultaneously in either clockwise or counterclockwise directions. When players release the key, the two orbs stop rotating. 
2. Players will earn points for avoiding bricks based on the difficulty and speed. 
3. The game will end once an orb crashes into an obstacle.

## Goal

1. Set up the canvas and basic shapes (e.g. orbs, bricks)
2. Implement game logic (e.g. collision, movement, earning points)
3. Define the format of configuration files, and handle the logic of reading and parsing files
4. Good to have: difference brick types, irregular movement of bricks, leaderboard
