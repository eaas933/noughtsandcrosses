library(tidyverse)

Outcomes <- c("Win in 5 moves", "Win in 6 moves", "Win in 7 moves", "Win in 8 moves", "Win in 9 moves", "Draw")
Games <- c(1440, 5328, 47952, 72576, 81792, 46080)
Total_Games <- sum(Games)
Games_Percentages <- (Games / Total_Games) * 100

Total_Games_Possible <- data.frame(Outcomes, Games_Percentages)
ggplot(Total_Games_Possible, aes(Outcomes, Games_Percentages, colour = Outcomes, fill = Outcomes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = c(5, 6, 7, 8, 9, "Draw")), colour = "black", vjust = -0.2, hjust = 1.1, face = "bold", size = 8) +
  labs(x = "possible game outcomes", 
       y = "% likelihood of outcome relative to the total possible games",
       title = "Noughts and Crosses - all outcomes",
       subtitle = "Symmetries are not accounted for in this dataframe") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.minor = element_line(size = 1), panel.grid.major = element_line(size = 0.5), 
        axis.title.x = element_text(colour="black", size=10, face="bold"),
        axis.title.y = element_text(colour="black", size=10, face="bold"))

# the objective is to create a script that effectively wins or draws every single game of N&T
# first we need to create a working version of tic-tac-toe

board <- matrix(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)

rownames(board) = c("A", "B", "C") 
colnames(board) = c("D", "E", "F")

move <<- 0

x_move <- function(row, column) {
  board[row, column] <<- "x"
  
  move <<- (move + 1)  
  
  print(paste("move #", move))
    
  if (board[1, 1] == board[1, 2] && board[1, 2] == board[1, 3]) {
    print(paste(board[1, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[2, 1] == board[2, 2] && board[2, 2] == board[2, 3]) {
    print(paste(board[2, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[3, 1] == board[3, 2] && board[3, 2] == board[3, 3]) {
    print(paste(board[3, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 1] == board[2, 1] && board[2, 1] == board[3, 1]) {
    print(paste(board[1, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 2] == board[2, 2] && board[2, 2] == board[3, 2]) {
    print(paste(board[1, 2], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 3] == board[2, 3] && board[2, 3] == board[3, 3]) {
    print(paste(board[1, 3], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 1] == board[2, 2] && board[2, 2] == board[3, 3]) {
    print(paste(board[1, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 3] == board[2, 2] && board[2, 2] == board[3, 1]) {
    print(paste(board[1, 3], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (move == 9) {
    print("The game has ended in a draw")
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  }
  
  board
  
}

o_move <- function(row, column) {
  board[row, column] <<- "o"
  
  move <<- (move + 1)  
  
  print(paste("move #", move))
  
  if (board[1, 1] == board[1, 2] && board[1, 2] == board[1, 3]) {
    print(paste(board[1, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[2, 1] == board[2, 2] && board[2, 2] == board[2, 3]) {
    print(paste(board[2, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[3, 1] == board[3, 2] && board[3, 2] == board[3, 3]) {
    print(paste(board[3, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 1] == board[2, 1] && board[2, 1] == board[3, 1]) {
    print(paste(board[1, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 2] == board[2, 2] && board[2, 2] == board[3, 2]) {
    print(paste(board[1, 2], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 3] == board[2, 3] && board[2, 3] == board[3, 3]) {
    print(paste(board[1, 3], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 1] == board[2, 2] && board[2, 2] == board[3, 3]) {
    print(paste(board[1, 1], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (board[1, 3] == board[2, 2] && board[2, 2] == board[3, 1]) {
    print(paste(board[1, 3], "has won!"))
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  } else if (move == 9) {
    print("The game has ended in a draw")
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
  }
  
  board
  
}

# I think an easy way to stop the columns from losing their labeling 
# you could just wrap the variable properly with brackets, like this:
# else if (move == 9) {
# print("The game has ended in a draw")
# board <<- (matrix(
#  c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE),
# rownames(board) = c("A", "B", "C"), 
# colnames(board) = c("D", "E", "F"))
# move <<- 0
# } but i cannot be bothered to go through that right now and numerical referencing
# and what not works fine
