monty_hall <- function(switch = TRUE) {
  doors <- 1:3
  prize <- sample(doors, 1) # 車在哪
  choice <- sample(doors, 1) # 玩家選
  
  remaining <- doors[doors != choice & doors != prize]
  opened <- sample(remaining, 1)
  
  if (switch) {
    choice <- doors[doors != choice & doors != opened]
  }
  
  return(choice == prize)
}

x = monty_hall(TRUE)













