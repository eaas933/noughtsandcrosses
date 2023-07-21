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
game_total_moves <<- c()
top_row_horizontal <<- 0
middle_row_horizontal <<- 0 
bottom_row_horizontal <<- 0
first_col_vertical <<- 0
second_col_vertical <<- 0
third_col_vertical <<- 0 
left_to_right_diagonal <<- 0
right_to_left_diagonal <<- 0
game_draw <<- 0
x_wins <<- 0
o_wins <<- 0
draw <<- 0
session_total_games <<- sum(x_wins + o_wins + draw)
session_stats <- c(x_wins, o_wins, draw, session_total_games)
names(session_stats) <- c("x_wins", "o_wins", "draw", "total")

x_move <- function(row, column) {
  
  if (board[1, 1] == 1 | board[1, 1] == "1" | board[1, 1] != "x" | board[1, 1] != "o") { 
      board[row, column] <<- "x"
      move <<- (move + 1)  
      print(paste("move #", move)) 
    } else if (board[1, 2] == 2 | board[1, 2] == "2" | board[1, 2] != "x" | board[1, 2] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[1, 3] == 3 | board[1, 3] == "3" | board[1, 3] != "x" | board[1, 3] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[2, 1] == 4 | board[2, 1] == "4" | board[2, 1] != "x" | board[2, 1] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[2, 2] == 5 | board[2, 2] == "5" | board[2, 2] != "x" | board[2, 2] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[2, 3] == 6 | board[2, 3] == "6" | board[2, 3] != "x" | board[2, 3] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[3, 1] == 7 | board[3, 1] == "7" | board[3, 1] != "x" | board[3, 1] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[3, 2] == 8 | board[3, 2] == "8" | board[3, 2] != "x" | board[3, 2] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[3, 3] == 9 | board[3, 3] == "9" | board[3, 3] != "x" | board[3, 3] != "o") { 
    board[row, column] <<- "x"
    move <<- (move + 1)  
    print(paste("move #", move)) }
    
  if (board[1, 1] == board[1, 2] && board[1, 2] == board[1, 3]) {
    print(paste(board[1, 1], "has won!"))
    top_row_horizontal <<- (top_row_horizontal + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[2, 1] == board[2, 2] && board[2, 2] == board[2, 3]) {
    print(paste(board[2, 1], "has won!"))
    middle_row_horizontal <<- (middle_row_horizontal + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[3, 1] == board[3, 2] && board[3, 2] == board[3, 3]) {
    print(paste(board[3, 1], "has won!"))
    bottom_row_horizontal <<- (bottom_row_horizontal + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 1] == board[2, 1] && board[2, 1] == board[3, 1]) {
    print(paste(board[1, 1], "has won!"))
    first_col_vertical <<- (first_col_vertical + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 2] == board[2, 2] && board[2, 2] == board[3, 2]) {
    print(paste(board[1, 2], "has won!"))
    second_col_vertical <<- (second_col_vertical + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 3] == board[2, 3] && board[2, 3] == board[3, 3]) {
    print(paste(board[1, 3], "has won!"))
    third_col_vertical <<- (third_col_vertical + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 1] == board[2, 2] && board[2, 2] == board[3, 3]) {
    print(paste(board[1, 1], "has won!"))
    left_to_right_diagonal <<- (left_to_right_diagonal + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 3] == board[2, 2] && board[2, 2] == board[3, 1]) {
    print(paste(board[1, 3], "has won!"))
    right_to_left_diagonal <<- (right_to_left_diagonal + 1)
    game_total_moves <<- append(game_total_moves, move)
    x_wins <<- (x_wins + 1)
    x_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (move == 9) {
    print("The game has ended in a draw")
    game_draw <<- (game_draw + 1)
    game_total_moves <<- append(game_total_moves, move)
    draw <<- (draw + 1)
    draw
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  }
  
  board
  
}

o_move <- function(row, column) {
  
  if (board[1, 1] == 1 | board[1, 1] == "1" | board[1, 1] != "x" | board[1, 1] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[1, 2] == 2 | board[1, 2] == "2" | board[1, 2] != "x" | board[1, 2] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[1, 3] == 3 | board[1, 3] == "3" | board[1, 3] != "x" | board[1, 3] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[2, 1] == 4 | board[2, 1] == "4" | board[2, 1] != "x" | board[2, 1] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[2, 2] == 5 | board[2, 2] == "5" | board[2, 2] != "x" | board[2, 2] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[2, 3] == 6 | board[2, 3] == "6" | board[2, 3] != "x" | board[2, 3] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[3, 1] == 7 | board[3, 1] == "7" | board[3, 1] != "x" | board[3, 1] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[3, 2] == 8 | board[3, 2] == "8" | board[3, 2] != "x" | board[3, 2] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) 
  } else if (board[3, 3] == 9 | board[3, 3] == "9" | board[3, 3] != "x" | board[3, 3] != "o") { 
    board[row, column] <<- "o"
    move <<- (move + 1)  
    print(paste("move #", move)) }
  
  if (board[1, 1] == board[1, 2] && board[1, 2] == board[1, 3]) {
    print(paste(board[1, 1], "has won!"))
    top_row_horizontal <<- (top_row_horizontal + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[2, 1] == board[2, 2] && board[2, 2] == board[2, 3]) {
    print(paste(board[2, 1], "has won!"))
    middle_row_horizontal <<- (middle_row_horizontal + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[3, 1] == board[3, 2] && board[3, 2] == board[3, 3]) {
    print(paste(board[3, 1], "has won!"))
    bottom_row_horizontal <<- (bottom_row_horizontal + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 1] == board[2, 1] && board[2, 1] == board[3, 1]) {
    print(paste(board[1, 1], "has won!"))
    first_col_vertical <<- (first_col_vertical + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 2] == board[2, 2] && board[2, 2] == board[3, 2]) {
    print(paste(board[1, 2], "has won!"))
    second_col_vertical <<- (second_col_vertical + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 3] == board[2, 3] && board[2, 3] == board[3, 3]) {
    print(paste(board[1, 3], "has won!"))
    third_col_vertical <<- (third_col_vertical + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 1] == board[2, 2] && board[2, 2] == board[3, 3]) {
    print(paste(board[1, 1], "has won!"))
    left_to_right_diagonal <<- (left_to_right_diagonal + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (board[1, 3] == board[2, 2] && board[2, 2] == board[3, 1]) {
    print(paste(board[1, 3], "has won!"))
    right_to_left_diagonal <<- (right_to_left_diagonal + 1)
    game_total_moves <<- append(game_total_moves, move)
    o_wins <<- (o_wins + 1)
    o_wins
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
  } else if (move == 9) {
    print("The game has ended in a draw")
    game_draw <<- (game_draw + 1)
    game_total_moves <<- append(game_total_moves, move)
    draw <<- (draw + 1)
    draw
    session_stats <- c(x_wins, o_wins, draw, session_total_games)
    names(session_stats) <- c("x_wins", "o_wins", "draw", "total")
    print(session_stats)
    board <<- matrix(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3, byrow = TRUE)
    rownames(board) = c("A", "B", "C") 
    colnames(board) = c("D", "E", "F")
    move <<- 0
    session_summary_1 <<- ggplot(as.data.frame(table(game_total_moves)), aes(x = "", y = Freq, fill = game_total_moves)) +
      geom_bar(width = 1, stat = "identity") +  coord_polar(theta = "y")
    winning_methods <- c(top_row_horizontal, middle_row_horizontal, bottom_row_horizontal, first_col_vertical, second_col_vertical, third_col_vertical, left_to_right_diagonal, right_to_left_diagonal, game_draw)
    winning_methods_labels <- c("top_row_horizontal", "middle_row_horizontal", "bottom_row_horizontal", "first_col_vertical", "second_col_vertical", "third_col_vertical", "left_to_right_diagonal", "right_to_left_diagonal", "game_draw")
    session_winning_method_counter <- data.frame(winning_methods_labels, winning_methods)
    session_summary_2 <<- ggplot(session_winning_method_counter, aes(x = winning_methods_labels, y = winning_methods, color = winning_methods_labels, fill = winning_methods_labels)) +
      geom_bar(stat = "identity") +   
      coord_flip() +
      theme_bw()
    
  }
  
  board
  
}

computer_1 <- function() {
    second_x_tree <<- c(board[1, 1], board[1, 3], board[3, 1], board[3, 3])
    second_x_move <<- sample(second_x_tree, 1)
    if (move == 0 & board[1, 1] != "o" & second_x_move == 1) {
      x_move(1, 1)
    } else if (move == 0 & board[1, 1] != "o" & second_x_move == 2) {
      x_move(1, 3)
    } else if (move == 0 & board[1, 1] != "o" & second_x_move == 3) {
      x_move(3, 1)
    } else if (move == 0 & board[1, 1] != "o" & second_x_move == 4) {
      x_move(3, 3)
    }
    
    if (move == 2 & board[2, 2] == "o" & board[1, 1] == "x") {
      x_move(3, 3)
    } else if (move == 2 & board[2, 2] == "o" & board[3, 3] == "x") {
      x_move(1, 1)
    } else if (move == 2 & board[2, 2] == "o" & board[1, 3] == "x") {
      x_move(3, 1)
    } else if (move == 2 & board[2, 2] == "o" & board[1, 3] == "x") {
      x_move(3, 1)
    } else if (move == 2 & board[2, 2] == "o" & board[3, 1] == "x") {
      x_move(1, 3)
    }

 # etc etc, you keep going from here i suppose
  
    board
    
}

# i think all that's left is to go through the tedious process of hard encoding the computer's
# invincible noughts & crosses algorithm
