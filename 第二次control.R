###第二次控制----

r_2_control = function(n = 1000){
  
  p = 1/2 
  u = runif(n/2)
  u_2 = 1-u 
  U = c(u,u_2)
  x = as.integer(U > 1-p)
  return(x)
  
  
}

change_unknown_control = function(x){
  
  x_1 = x[x==1]
  y_1 = numeric(length(x_1))
  x_2 = x[x!=1]
  open_goat = r_2_control(n = length(x_2))
  y = c(y_1,open_goat)
  
  return(y)
  
}

##(3-2)如果主持人不知跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean 分布----
# R 內建 Bernoulli
R_default_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

# inverse function
Inverse_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car(n, p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

# inverse function + controlling var
Control_unknown_distribution <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  
  for(i in 1:B){
    x <- first_car_control_var(n, p)
    result <- change_unknown_control(x)
    y[i] <- mean(result)
  }
  
  return(c(mean(y), var(y)))
}

R_default_unknown_control <- R_default_unknown_distribution(n, B, p)
Inverse_unknown_control <- Inverse_unknown_distribution(n, B, p)
Control_unknown_control <- Control_unknown_distribution(n, B, p)

Compare_unknown <- rbind(
  "R內建的函數" = R_default_unknown_control,
  "Inverse function" = Inverse_unknown_control,
  "Inverse function with controlling var" = Control_unknown_control
)

colnames(Compare_unknown) <- c("Mean", "Var")

Compare_unknown

##畫分布圖
##(3-2)如果主持人不知跑車在哪，比較三種情況（r內建、inverse method、antithetic）sample mean----

# R 內建 Bernoulli
R_default_unknown_y <- function(n = 10000, B = 10000, p = 1/3){
  y <- numeric(B)
  for(i in 1:B){
    x <- rbinom(n, size = 1, prob = p)
    result <- change_unknown_control(x)
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
    result <- change_unknown_control(x)
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
    result <- change_unknown_control(x)
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

#variance reduction
variance_reduction_2 <- (var(Inverse_unknown_y) - var(Control_unknown_y)) / var(Inverse_unknown_y) * 100
variance_reduction_2

d1 <- density(R_default_unknown_y)
d2 <- density(Inverse_unknown_y)
d3 <- density(Control_unknown_y)
plot(d1,
     main = "Comparison of three Probability Distributions",
     xlab = "Probability",
     ylab = "Density",
     col = "red",
     lwd = 2,
     xlim = range(c(d1$x, d2$x, d3$x)),
     ylim = c(0, max(c(d1$y, d2$y, d3$y)) * 1.1))
lines(d2,
      col = "blue",
      lwd = 2,
      lty = 2)
lines(d3,
      col = "darkgreen",
      lwd = 2,
      lty = 3)
legend("topright",
       legend = c("rbinom", "inverse function", "control variance"),
       col = c("red", "blue", "darkgreen"),
       lwd = 2,
       lty = c(1, 2, 3))





##第二次控制permutation----
#change_unknown_control = function(x){
#x_1 = x[x==1]
#y_1 = numeric(length(x_1))
#x_2 = x[x!=1]
#open_goat = r_2_control(n = length(x_2))
#y = c(y_1,open_goat)
#return(y)
#}

#MC
n <- 1000
y <- numeric(n)

for(i in 1:n){
  x <- first_car_control_var(n = 1000)
  y[i] <- mean(change_unknown_control(x))
}

mean(y)
var(y)

# 不換門且主持人未知----
do_not_change_unknown_control = function(x){
  x_1 = x[x==1]
  y_1 = x_1
  x_2 = x[x!=1]
  open_goat = numeric(length(x_2))
  y = c(y_1,open_goat)
  return(y)
}

#MC
n <- 1000
y <- numeric(n)

for(i in 1:n){
  x <- first_car_control_var(n = 1000)
  y[i] <- mean(do_not_change_unknown_control(x))
}
mean(y)
var(y)

# permutation
change = function(){
  #MC
  n <- 1000
  y <- numeric(n)
  
  for(i in 1:n){
    x <- first_car_control_var(n = 1000)
    y[i] <- mean(change_unknown_control(x))
  }
  
  return(y)
}


# 不換門但主持人知道----
do_not_change = function(){
  
  #MC
  n <- 1000
  y <- numeric(n)
  
  for(i in 1:n){
    x <- first_car_control_var(n = 1000)
    y[i] <- mean(do_not_change_unknown_control(x))
  }
  
  return(y)
}

x = change()
y = do_not_change()


# HO:----
z <- c(as.numeric(x), as.numeric(y))
idx <- sample(length(z), replace = FALSE)
z1 <- z[idx[1:1000]]
z2 <- z[idx[1001:length(z)]]


# 產生分布
permutation = function(){
  result <- c()
  for (i in 1:10000){
    idx <- sample(length(z), replace = FALSE)
    z1 <- z[idx[1:1000]]
    z2 <- z[idx[1001:length(z)]]
    result[i] <- mean(z1) - mean(z2)
  }
  return(result)
}

permutation_dist = permutation()

# 計算p-value
obs <- mean(x) - mean(y)

p_value <- mean(abs(permutation_dist) >= abs(obs))
p_value

# 95%機率分布
ci <- quantile(permutation_dist, c(0.025, 0.975))
ci

# 畫圖
xrange <- range(c(permutation_dist, obs, ci))

hist(permutation_dist,
     probability = TRUE,
     breaks = 30,
     main = "Permutation Null Distribution",
     xlab = "Mean Difference",
     xlim = xrange)

lines(density(permutation_dist),
      lwd = 2,
      col = "blue")

abline(v = ci[1], col = "darkgreen", lwd = 3, lty = 2)
abline(v = ci[2], col = "darkgreen", lwd = 3, lty = 2)

abline(v = obs, col = "red", lwd = 3)

text(
  obs,
  max(density(permutation_dist)$y) * 0.9,
  labels = "p = 0",
  col = "red",
  pos = 4
)

legend("topright",
       legend = c("Null density", "95% CI", "Observed"),
       col = c("blue", "darkgreen", "red"),
       lwd = 2,
       lty = c(1, 2, 1),
       bty = "n")

# 表格
mean(permutation_dist)
mean(x - y)
sd(permutation_dist)


##bootstrap----
change_unknown_control = function(x){
  
  x_1 = x[x==1]
  y_1 = numeric(length(x_1))
  x_2 = x[x!=1]
  open_goat = r_2_control(n = length(x_2))
  y = c(y_1,open_goat)
  return(y)
}
#MC
n <- 1000
y <- numeric(n)

for(i in 1:n){
  x <- first_car_control_var(n = 1000)
  y[i] <- mean(change_unknown_control(x))
}


people = c()

for (i in 1:100){
  
  n <- 1000
  y <- numeric(n)
  
  for(j in 1:n){
    x <- first_car_control_var(n = 1000)
    y[j] <- mean(change_unknown_control(x))
  }
  
  people[i] = mean(y)
  
}

people

library(boot)
# statistic function
boot_fun <- function(data = people, index){
  mean(data[index])
}

result <- boot(
  data = people,
  statistic = boot_fun,
  R = 10000
)

result

# 畫圖
# 95% percentile CI
ci <- quantile(result$t, c(0.025, 0.975))

# 畫圖
hist(result$t,
  probability = TRUE,
  breaks = 30,
  main = "Bootstrap Distribution",
  xlab = "Bootstrap Mean",
  ylab = "Density"
  )
lines(density(result$t),lwd = 2)

# 原始 sample mean
abline(
  v = mean(people),
  col = "blue",
  lwd = 2)

# 95% CI
abline(
  v = ci,
  col = "red",
  lwd = 2,
  lty = 2
)

# legend
legend("topright",
  legend = c("Density", "Sample Mean", "95% CI"),
  col = c("black","blue","red"),
  lwd = 2,
  lty = c(1, 1, 2))

mean(people)
var(people)
stderr(people)
  


##jackknife----
#不能用quantile 而是直接加減1.96
n = length(people)
# 存 jackknife estimate
jack_mean = numeric(n)
# leave-one-out
for(i in 1:n){
  jack_sample = people[-i]
  jack_mean[i] = mean(jack_sample)
}
# 算SE
jack_se = sqrt(((n - 1) / n) *sum((jack_mean - mean(jack_mean))^2))
jack_se

# 算cI
theta_hat = mean(people)
theta_hat

ci_lower = theta_hat - 1.96 * jack_se
ci_upper = theta_hat + 1.96 * jack_se

v1 = c(ci_lower, ci_upper)
xrange <- range(c(jack_mean, v1, theta_hat))

# 畫圖
hist(jack_mean,
  probability = TRUE,
  breaks = 30,
  main = "Jackknife Distribution",
  xlab = "Jackknife Mean",
  xlim = xrange,
  ylim=c(0,300000))

# density curve
lines(density(jack_mean),
  lwd = 2,
  col = "black")

# mean:藍線
abline(v = mean(people),
  col = "blue",
  lwd = 2)

#95% CI:紅色虛線
#ci_lower
abline(
  v = v1[1],
  col = "red",
  lwd = 2,
  lty = 2
)
#ci_upper
abline(
  v = v1[2],
  col = "red",
  lwd = 2,
  lty = 2
)

legend("topright",
       legend = c("Density", "Sample Mean", "95% CI"),
       col = c("black", "blue", "red"),
       lwd = 2,
       lty = c(1, 1, 2))

# 輸出表格
jack_mean_bar <- mean(jack_mean)

jack_bias <- (n - 1) * (jack_mean_bar - theta_hat)
jack_bias









##MCMC
