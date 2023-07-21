# noughtsandcrosses
An application of an algorithm to showcase the optimal moves for noughts and crosses to win or draw every time.

Prerequisites:
- R (Programming Language) 
- Ensure the tidyverse package is installed.

To begin playing a game simply decide which player goes first.
The first player will begin by typing "board" into the console upon which it should show:

![image](https://github.com/eaas933/noughtsandcrosses/assets/64386451/d78c03e6-a7d0-45b7-87e7-c6acb380bb91)

A corresponds to the first row, B the second, and C the third row.
D corresponds to the first column, E the second column, and F the third column.

To make your first move type "x_move(row, column)" into the console.
For instance if I wanted to place an "x" in the Second Row and Third Column I would input in to the console: "x_move(2, 3)"
Now the second player will input the command "o_move(row, column)" into the console.

The game is then played like traditional noughts and crosses.

Upon completion of the game your stats will be recorded onto your local R session.
For instance lets suppose "x" (Player 1) wins you will see the following:

![image](https://github.com/eaas933/noughtsandcrosses/assets/64386451/8cebc7d1-c4f2-4019-8599-cdfe9dce19a3)

You can view some of the data collected from your game played by entering the following commands into the console:

"session_summary_1"
This will showcase a pie chart which showcase the games you have played and the total number of moves made in the games.
In the following example two games were played, one which ended in 5 moves and another that ended in 8 moves.

![image](https://github.com/eaas933/noughtsandcrosses/assets/64386451/1e60b8af-c0e9-493c-b162-d3599ca0d03c)

"session_summary_2"
This will showcase a bar chart of the games you have played and the methods used to win, so the winning line made.
In the following example two games were played, one which was won using a line formed on the third column vertically 
and another which was won on the first column vertically.

![image](https://github.com/eaas933/noughtsandcrosses/assets/64386451/5cc22cfc-213f-4140-b096-6f52f7c7270c)





