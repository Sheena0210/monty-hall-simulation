#根據inverse function產生一開始選到跑車機率0.32----
set.seed(1)

first_car <- function(){
  p <- 1/3
  u <- runif(1)
  x <- as.integer(u > 1 - p)
  return(x)
}

x <- c()

for(i in 1:1000){
  x[i] <- first_car()
}

mean(x) #0.32
var(x)  #0.2178178

#驗證換門(x2)之後選到跑車的機率 0.6677----
set.seed(1)
#一開始是否選到跑車
#選到跑車=1. 沒選到跑車=0
first_car <- function(){
  p <- 1/3
  u <- runif(1)
  x <- as.integer(u <= p)
  return(x)
}
#換門後是否選到跑車
#如果一開始沒選到跑車，換門後會中
#如果一開始選到跑車，換門後會輸
change_door <- function(x){
  x2 <- ifelse(x == 0, 1, 0)
  return(x2)
}

#驗證一開始選到跑車的機率
x <- numeric(1000)
for(i in 1:1000){
  x[i] <- first_car()
}
mean(x)
var(x)

#換門x2後的勝率 0.6677
n <- 10000
x2 <- numeric(n)

for(i in 1:n){
  x <- first_car()
  x2[i] <- change_door(x)
}
mean(x2)












#利用r內建的函數生成----
#(1)比較使用antithetic variates var有無降低----
#                          mean.         var. 
#inverse method（原本）    0.3351     0.2228303
#利用antithetic variates   0.328      0.220438
#r.            
set.seed(1)
n <- 10000
B <- 10000
p <- 1/3

# 使用 inverse function 產生 Bernoulli(p = 1/3)
first_car <- function(n = 10000, p = 1/3){
  u <- runif(n)
  x <- as.integer(u > 1 - p)
  return(x)
}
# 使用antithetic variates降低random 的mc variance
first_car_control_var <- function(n = 10000, p = 1/3){
  u <- runif(n / 2)
  u_2 <- 1 - u
  U <- c(u, u_2)
  x <- as.integer(U > 1 - p)
  return(x)
}
# 驗證 first_car
x <- first_car(n, p)
mean(x)
var(x)
# 驗證 control variance
y <- first_car_control_var(n, p)
mean(y)
var(y)



#(2)比較三種sample mean分佈----
#                                         Mean          Var
#R內建的函數                           0.3333670 2.213848e-05
#Inverse function                      0.3333048 2.197391e-05
#Inverse function with controlling var 0.3333298 1.109155e-05

# R 內建 Bernoulli 的 sample mean 分布
R_default_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    y[i] <- mean(x)
  }
  
  result <- c(mean(y), var(y))
  return(result)
}

# inverse function 的 sample mean 分布
Inverse_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car(n, p)
    y[i] <- mean(x)
  }
  
  result <- c(mean(y), var(y))
  return(result)
}

# inverse function + control variance 的 sample mean 分布
Control_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    y[i] <- mean(x)
  }
  
  result <- c(mean(y), var(y))
  return(result)
}

# 執行三種方法
R_default <- R_default_distribution(n, B, p)
Inverse <- Inverse_distribution(n, B, p)
Control <- Control_distribution(n, B, p)

# 比較結果
Compare_sample_mean_distribution <- rbind(
  "R內建的函數" = R_default,
  "Inverse function" = Inverse,
  "Inverse function with controlling var" = Control
)

colnames(Compare_sample_mean_distribution) <- c("Mean", "Var")

Compare_sample_mean_distribution

#分佈圖----
#回傳是整個ｙ
set.seed(1)
n <- 10000
B <- 10000
p <- 1/3
# R 內建 Bernoulli 的 sample mean
R_default_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    y[i] <- mean(x)
  }
  return(y)
}
# inverse function 的 sample mean
Inverse_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car(n, p)
    y[i] <- mean(x)
  }
  return(y)
}

# inverse function + control variance 的 sample mean
Control_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    y[i] <- mean(x)
  }
  return(y)
}

# 執行
R_default_y <- R_default_distribution(n, B, p)
Inverse_y <- Inverse_distribution(n, B, p)
Control_y <- Control_distribution(n, B, p)

#R Bernoulli Simulation
hist(R_default_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (R Bernoulli Simulation)",
     xlab = "Estimated probability",
     probability = TRUE)
abline(v=mean(R_default_y),col="yellow",lty=2,lwd=2)
text(x = mean(R_default_y) + 0.008,
     y = max(hist(R_default_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(R_default_y), 4)),
     pos = 4)

#Inverse function
hist(Inverse_y,
     main = "Sampling Distribution of the Estimated Winning Probability 
     (Inverse function)",
     xlab = "Estimated probability",
     probability = TRUE)
abline(v=mean(Inverse_y),col="yellow",lty=2,lwd=2)
text(x = mean(Inverse_y) + 0.008,
     y = max(hist(Inverse_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Inverse_y), 4)),
     pos = 4)

#Control_y
hist(Control_y,
     main = "Sampling Distribution of the Estimated Winning Probability
     (Inverse function with control variance)",
     xlab = "Estimated probability",
     probability = TRUE)
abline(v=mean(Control_y),col="yellow",lty=2,lwd=2)
text(x = mean(Control_y) + 0.008,
     y = max(hist(Control_y, plot = FALSE)$density) * 0.9,
     labels = paste0("Mean = ", round(mean(Control_y), 4)),
     pos = 4)
