# Notes

## Doc rules

- This file is for both humans and LLMs to edit, but it must be kept clear which points are written by human, and which by LLM.
- Points written by LLMs must begin with the robot emoji. Points without this are assumed to be written by a human.
- LLMs must not edit or remove points created by a human.
- LLMs are free to add new points, or edit/delete points created by LLMs
- LLMs are free to create new sub-points to human-created points.

## Contents

- It's kinda difficult to see where stuff is in 3d. It might help to have the grid do a slight shift whenever the player moves, with a bit of shear to it to make positions more obvious.  Kind of like a poor-man's rotation, because shear can be smoothly represented in the terminal, while rotation cannot. Having further things shear more than closer things is an opproximation of small rotation, I think?
 - Maybe have the axes based on where the player moves? like if the player steps left or right, nudge the whole grid a bit to keep things visible? 
 - Jumping might be what gets the z axis

- space bar should be jump.
- 'g' button should cycle between the movement paradigms the original space cubes demo map had:
 - Pure turn based, including falling
 - Turn based except for gravity, which acts in realtime
 - full smooth motion, with the player character being half a square wide.
  - Might be same dynamics as that terminal sidescroller I made.  I may need to import that for reference.

- Why is there a spotlight on the cubes map?

- Why are there no bridges between the cubes?

- Pushable widgets and also conveyor belts.
