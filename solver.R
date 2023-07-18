library(tidyverse)

Outcomes <- c("Win in 5 moves", "Win in 6 moves", "Win in 7 moves", "Win in 8 moves", "Win in 9 moves", "Draw")
Games <- c(1440, 5328, 47952, 72576, 81792, 46080)
Total_Games <- sum(Games)
Games_Percentages <- (Games / Total_Games) * 100

Total_Games_Possible <- data.frame(Outcomes, Games_Percentages)
ggplot(Total_Games_Possible, aes(Outcomes, Games_Percentages, colour = Outcomes, fill = Outcomes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Outcomes), colour = "black", vjust = -0.4, hjust = -0.04) +
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

board

board["A", "D"]