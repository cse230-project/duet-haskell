# duet-haskell

Members: Jiachen Chen, Yiran Chen, Peizhen Wu

## Introduction

“With air tight controls and gameplay that’s tuned to perfection Duet provides the perfect balance between challenge and pure gaming satisfaction.” -- Duet Game.

We would like to implement a simplified version of the [Duet Game](https://apps.apple.com/us/app/duet-game/id634235735), developed by [Kumobius Games](https://apps.apple.com/us/developer/kumobius/id449069247), in Haskell. The Duet-Haskell application is a single player game where players control two colorured orbs to dodge randomly incoming obstacles.

In the game, players need to avoid the dropping obstacles. Once the player starts the game, the application randomly generates 100 obstacles in 4 types. The players can adjust the difficulty levels with obtacles dropping in 3 different speeds.

![Original Duet Game screenshot](/duet_game.jpg)

## Gameplay

The rules are simple: control two orbs in sync, survive against all obstacles and earn as many points as possible.

1. Players will hold Left or Right key to control the rotation of two orbs simultaneously in either clockwise or counterclockwise directions. When players release the key, the two orbs stop rotating.
2. Players will press Up key or Down key to control the speed of dropping bricks. We have 3 predefined difficulties: easy, medium and hard.
3. Players will earn points for avoiding bricks based on the difficulty and speed.
4. The game will revert to the beginning stage once an orb crashes into an obstacle.
5. The game is clear if players avoid all 100 randomly generated bricks.

## Goal

- [x] 1. Set up the canvas and basic shapes (e.g. orbs, bricks)
- [x] 2. Implement game logic (e.g. collision, movement, earning points)
- [x] 3. Implement UI to display game info: difficulty, score, highest score, game status
- [x] 4. Allow different difficulties
- [x] 5. Generate different brick types randomly
- [x] 6. Revert to beginning stage once hitting an obstacle
- [x] 7. Color the collision point

## Instructions
- Build with `stack build`
- Start game with `stack run`
- Test with `stack test`

## Milestone 2: Updates

Q: What is the architecture of your application (the key components)?

A: We use the MVC model. Model stores the components in the game: board (obstacles, vessels) and score. View displays the states of the board and score. Control handles user input such as pressing the left/right key. First, as the game starts, View displays the initial state of models. As a user presses keys to control the vessels, the states are updated in Model. Finally, the View re-renders the updated states.

---

Q: What challenges (if any) did you have so far and how did you solve them?

A: We used two emojis, 🔴 and 🔵, to represent the vessels in the game. However, each emoji takes up two letter's space in the terminal, which caused some problems when displaying the game, such as that a part of the border is not in the right place. Therefore, we use two empty characters as a square and add the color attribute to the square to represent the vessels.

Besides, due to the limitation of the brick library, we found that we don't have the ability to use the keydown and keyup event to implement the vessel movement logic when the user hold the arrow keys. Temporarily, we rely on the key repeat settings provided by the operating system and the "delay until repeat" setting also caused delays when moving the vessels.

---

Q: Do you expect to meet your goals until the deadline?

A: We are expeced to meet most of goals until the deadline, including the basic game canvas with multiple types of objects, movement of orbs using left/right keys, and key game logic (collision detection, random generation of bricks, earning scores and gameover). As for the configuration files, we still need more research on handling files in haskell. We will try to implement reading configuration files to customize settings.

---

Q: If not, how will you modify your goals?

A: We might choose to implement a few "good to have" features: different brick types, irregular movement of bricks, leaderboard. If we later find out that configuration files are insufficient for supporting our setting customization feature, we might define fewer customized settings and pass them in through the command line.
