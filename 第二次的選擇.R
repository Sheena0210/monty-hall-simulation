#參賽者已經選完第一次的門，接下來選擇第二次的門
set.seed(1)
first_car <- function(n = 1){
  p <- 1/3
  u <- runif(n)
  x <- as.integer(u > 1 - p)
  return(x)
}
#(1)如果主持人知道跑車在哪，換門後是跑車的機率:0.6633(2/3)----
#mean=0.6633(2/3)
#var=0.2233554
change_door <- function(x){
  if(x == 0){
    y <- 1
  }else if(x == 1){
    y <- 0
  }
  return(y)
}

y <- c()

for(i in 1:10000){
  x <- first_car(n = 1)
  y[i] <- change_door(x)
}
mean(y)
var(y)


#(2)如果主持人不知道跑車在哪----
set.seed(1)

#first_car:1=第一次選到車 0=第一次沒選到車
first_car <- function(n = 1){
  p <- 1/3
  u <- runif(n)
  x <- as.integer(u > 1 - p)
  return(x)
}


#(2)主持人不知道車在哪裡，若第一次沒選到車，主持人選到羊p=1/2,此時換門後是車機率:0.33532(1/3)----
#mean:0.33532
#var:0.2228827

#1:換門後是車 0:換門後是羊
change_door_unknown <- function(x){
  #第一次選到車
  if(x == 1){
    y <- 0
  }else{
  #第一次沒選到車 主持人也不知道正確答案 選到羊的機率1/2
  open_goat <- rbinom(1, size = 1, prob = 1/2)
  if(open_goat == 1){
    y <- 1
    }else{
    y <- 0
    }
  }
  return(y)
}


#MC
n <- 100000
y <- numeric(n)

for(i in 1:n){
  x <- first_car()
  y[i] <- change_door_unknown(x)
}


mean(y)
var(y)


#(3)比較三種情況（r內建、inverse method、antithetic）看主持人知不知道車在哪的sample mean分佈----
#                            Mean          Var
#R內建_主持人知道         0.6666710 2.238811e-05
#Inverse_主持人知道       0.6666709 2.207536e-05
#Control var_主持人知道   0.6666532 1.110665e-05
#R內建_主持人不知道       0.3333760 2.220071e-05
#Inverse_主持人不知道     0.3333157 2.215410e-05
#Control var_主持人不知道 0.3333492 1.958587e-05
set.seed(1)
n <- 10000
B <- 10000
p <- 1/3
# 一開始選到車：inverse function
first_car <- function(n = 10000, p = 1/3){
  u <- runif(n)
  x <- as.integer(u > 1 - p)
  return(x)
}
# 一開始選到車：control variance
first_car_control_var <- function(n = 10000, p = 1/3){
  u <- runif(n / 2)
  u_2 <- 1 - u
  U <- c(u, u_2)
  x <- as.integer(U > 1 - p)
  return(x)
}
# 主持人知道車在哪：換門結果
# 1 = 換門贏，0 = 換門輸
change_known <- function(x){
  y <- 1 - x
  return(y)
}
# 主持人不知道車在哪：換門結果
# 1 = 換門贏，0 = 換門輸
change_unknown <- function(x){
  y <- numeric(length(x))
  for(i in 1:length(x)){
    if(x[i] == 1){
      y[i] <- 0
    }else{
      open_goat <- rbinom(1, size = 1, prob = 1/2)
      y[i] <- open_goat
    }
  }
  
  return(y)
}

#(3-1)如果主持人知道跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean 分布----
# R 內建 Bernoulli
R_default_known_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_known(x)
    y[i] <- mean(result)
  }
  return(c(mean(y), var(y)))
}
# inverse function
Inverse_known_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_known(x)
    y[i] <- mean(result)
  }
  return(c(mean(y), var(y)))
}
# inverse function + controlling var
Control_known_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_known(x)
    y[i] <- mean(result)
  }
  return(c(mean(y), var(y)))
}
R_default_known <- R_default_known_distribution(n, B, p)
Inverse_known <- Inverse_known_distribution(n, B, p)
Control_known <- Control_known_distribution(n, B, p)
Compare_known <- rbind(
  "R內建的函數" = R_default_known,
  "Inverse function" = Inverse_known,
  "Inverse function with controlling var" = Control_known
)
colnames(Compare_known) <- c("Mean", "Var")
Compare_known

#(3-2)如果主持人不知跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean 分布----
# R 內建 Bernoulli
R_default_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_unknown(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

# inverse function
Inverse_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_unknown(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

# inverse function + controlling var
Control_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_unknown(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

R_default_unknown <- R_default_unknown_distribution(n, B, p)
Inverse_unknown <- Inverse_unknown_distribution(n, B, p)
Control_unknown <- Control_unknown_distribution(n, B, p)

Compare_unknown <- rbind(
  "R內建的函數" = R_default_unknown,
  "Inverse function" = Inverse_unknown,
  "Inverse function with controlling var" = Control_unknown
)

colnames(Compare_unknown) <- c("Mean", "Var")

Compare_unknown


Compare_all <- rbind(
  "R內建_主持人知道" = R_default_known,
  "Inverse_主持人知道" = Inverse_known,
  "Control var_主持人知道" = Control_known,
  
  "R內建_主持人不知道" = R_default_unknown,
  "Inverse_主持人不知道" = Inverse_unknown,
  "Control var_主持人不知道" = Control_unknown
)
colnames(Compare_all) <- c("Mean", "Var")
Compare_all



#分佈圖----
#(3-1)如果主持人知道跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean----
change_known <- function(x){
  result <- 1 - x
  return(result)
}
# R 內建 Bernoulli
R_default_known_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_known(x)
    y[i] <- mean(result)
  }
  return(y)
}
R_default_known_y <- R_default_known_y(n, B, p)

hist(R_default_known_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (R Bernoulli Simulation)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(R_default_known_y),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(R_default_known_y) + 0.002,
     y = max(hist(R_default_known_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(R_default_known_y), 4)),
     pos = 4)



# Inverse function
Inverse_known_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_known(x)
    y[i] <- mean(result)
  }
  return(y)
}
Inverse_known_y <- Inverse_known_y(n, B, p)

hist(Inverse_known_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (Inverse function)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(Inverse_known_y),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(Inverse_known_y) + 0.002,
     y = max(hist(Inverse_known_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Inverse_known_y), 4)),
     pos = 4)




# Inverse function + antithetic variates
Control_known_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_known(x)
    y[i] <- mean(result)
  }
  return(y)
}
Control_known_y <- Control_known_y(n, B, p)
hist(Control_known_y,
     main = "Sampling Distribution of the Estimated Winning Probability
     (Inverse function with control variance)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(Control_known_y),
       col = "yellow",
       lty = 2,
       lwd = 2)

text(x = mean(Control_known_y) + 0.002,
     y = max(hist(Control_known_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Control_known_y), 4)),
     pos = 4)

#(3-2)如果主持人不知跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean----

# R 內建 Bernoulli
R_default_unknown_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_unknown(x)
    y[i] <- mean(result)
  }
 
  return(y)
}
R_default_unknown_y <- R_default_unknown_y(n, B, p)
hist(R_default_unknown_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (R Bernoulli Simulation)",
     xlab = "Estimated winning probability",
     probability = TRUE)

abline(v = mean(R_default_unknown_y),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(R_default_unknown_y) + 0.002,
     y = max(hist(R_default_unknown_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(R_default_unknown_y), 4)),
     pos = 4)


# Inverse function
Inverse_unknown_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_unknown(x)
    y[i] <- mean(result)
  }
  return(y)
}
Inverse_unknown_y <- Inverse_unknown_y(n, B, p)
hist(Inverse_unknown_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (Inverse function)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(Inverse_unknown_y),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(Inverse_unknown_y) + 0.002,
     y = max(hist(Inverse_unknown_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Inverse_unknown_y), 4)),
     pos = 4)


# Inverse function + antithetic variates
Control_unknown_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_unknown(x)
    y[i] <- mean(result)
  }
  return(y)
}
Control_unknown_y <- Control_unknown_y(n, B, p)
hist(Control_unknown_y,
     main = "Sampling Distribution of the Estimated Winning Probability
     (Inverse function with control variance)",
     xlab = "Estimated winning probability",
     probability = TRUE)
abline(v = mean(Control_unknown_y),
       col = "yellow",
       lty = 2,
       lwd = 2)
text(x = mean(Control_unknown_y) + 0.002,
     y = max(hist(Control_unknown_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Control_unknown_y), 4)),
     pos = 4)