source("01_simulation.R")

set.seed(123)

n <- 10000

# 模擬
switch_results <- replicate(n, monty_hall(TRUE))
stay_results   <- replicate(n, monty_hall(FALSE))

# 勝率
switch_win_rate <- mean(switch_results)
stay_win_rate   <- mean(stay_results)

print(switch_win_rate)
print(stay_win_rate)