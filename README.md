# hyper3-solver

A Rubik's Cube solver written in oCaml
```
          __   __                                   ___  
         / /  / /                                  ___/  
        / /__/ /__    __  _______  ______  --___  ___/   
       / ___  / \ \  / / / ___  / / ____/ / ___/         
      / /  / /   \ \/ / / /__/ / / /___/ / /             
     /_/  /_/     \  / / _____/ /_____/ /_/              
                  / / / /                                
                 /_/ /_/                                 
```

## Install instructions
You can either :
* `git clone` this repo on your pc
* or download the zip from this github page

Before using the solver, **unzip `databases.zip`** into a `databases` folder.

And there you go ! Have fun with the solver :)

-> If you just wanna play around, launch `hyper3_solver.exe`
-> If you want to check out how the code works, check out the source code (`hyper3_upgrade.ml`)

## How to use the solver

Once the program has loaded the databases, you're given 3 options :
(1) Generate a random scramble of n moves where you get to chose n
(2) Input your own scramble sequence using the (conventional move notations)[https://jperm.net/3x3/moves]
(3) The most interesting one : input the state of a cube you want to solve.

Let's take a closer look at option 3.
In this repo there's a picture called `Patron Rempli.png` :
![Patron Rempli](https://github.com/user-attachments/assets/9cb81407-3fad-4f48-83fb-b437073ba50b)
An important fact about the Rubik's Cube is that the moves don't affect the center pieces' positions on the cube. You can therefore fix the orientation of the cube by fixing the positions of the center pieces relatively to the observator (for instance here the red centre is front (F), blue is right (R), white is up (U) etc).

To encode your cube state, you just need to **input the color code** (rbwyog) of **each sticker** at the index corresponding to its position on the cube.

For instance, if your corner urf (that is to say the corner intersecting the faces up, right and front) has a **blue sticker on the white face**, refering to the picture above, your cube state will have the caracter `b` at index `4` (which is the number on the corresponding sticker on the template).
Here's another example : after a F move, there's a green sticker on the white face in position 2. As a consequence, there'll be a `g` caracter at index `2`

Therefore, the encoding of the solved state is `"wwwwwwwwggggggggrrrrrrrrbbbbbbbbooooooooyyyyyyyy"`
