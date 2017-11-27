# Quadratic Rush
---
**Quadratic Rush** is a *vertical scrolling game*, where the goal of the player is to survive through as many waves of walls coming towards him/her as possible. The player can survive the waves by shooting his way through the obstacles which have HP.

---

## Overview
This is a Programming Fundamentals I project carried out by **Alessandro Romanelli** and **Edoardo Lunardi**.

---

### Minimal Valuable Product
In this part you can find what the minimal product that we expect to deliver is composed of:
  - Player dies upon contact with a wall
  - Player only moves horizontally
  - Player shoots at all times
  - Player is be awarded points based on damage dealt
  - Score of the player is evaluated constantly and always shown
  - Walls is composed by blocks which have HPs

---

### Extra Features
As part of our development, once the **MVP** is delivered, we plan on expanding our project by implementing the following features:

- Blocks’ health should be randomly generated and not hardcoded;
- Blocks should change color depending on the their HP;
- Time elapsed should have an effect on difficulty (the more time passes, the harder the game gets);
- Power ups should be randomly spawned to help the player:
    – Immunity for n seconds;
    – Firing speed increased;
    – Volley fire, three or more trajectories;
    – Next shot is a one-hit kill;
- Special Attack with cooldown to destroy the entire wall, without yielding points;
- Special properties for some random walls’ blocks:
    – Blocks that reflect the player fire;
    – Undestroyable blocks;
    – For each HP, a special block yields twice the points;
- The game should be able to remember and store the highest score of the player, in order to remember it when restarting the program.
- At the end of the game, player should be prompted to insert name for record;
- Implement a game menu, where player can either:
    – Start a new game;
    – Quit the game;
    – View the leaderboard
- Store the game leaderboard into a file that can be accessed upon restarting the game to save progression.

---

### Version Nomenclature
During the initial development of our project, we will refer to **Version 0** (or **V0**), as that phase between the inception and the first delivery of our **MVP**. **Version 1** is going to be the delivered **MVP** and finally and other major patch is going to be appended with a dot notation as follows:

    v1.0     <==== Release
    V0.1     <==== Major patch
    V0.1.1   <==== Minor patch

---

### Delivery

Our **MVP** will be delivered not later than the 18<sup>th</sup> of December.

---

### Tech

The project is going to be developed using *Racket Lang* as programming language and by importing the two main libraries: *2htdp/image* and *2htdp/universe*. No external packages and libraries should be needed at this point in time but we reserve the last say when it will come the time to implement the Extra Features discussed above.

---

### Playing the game

Quadratic Rush is going to run esclusively on DrRacket. In order to run the game, open the .rkt file that can be found in the repository and run it, the rest should be easy.

---

### Development
Want to contribute? **Great**, you can't!

---

### Todos

 - Write MORE Tests
 - Add Night Mode

---

# License: MIT
