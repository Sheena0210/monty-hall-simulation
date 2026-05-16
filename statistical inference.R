#statistical inference

#bootstrap----
#random variable利用inverse method生成
#mean: 0.66602
#var:0.0002256562

# Inverse method做N = 100 人，每人換門 1000 次--
# 使用 inverse_control 做 100 個人的結果
set.seed(1)
# inverse function
first_car <- function(n = 10000, p = 1/3){
  u <- runif(n)
  x <- as.integer(u <= p)
  return(x)
}
# 換門
change_door <- function(x){
  x2 <- ifelse(x == 0, 1, 0)
  return(x2)
}
# 100 個人
N <- 100
# 每人玩 1000 次
n <- 1000

# 每個人的勝率
people <- numeric(N)

for(i in 1:N){
  x <- first_car(n)
  x2 <- change_door(x)
  # 第 i 個人的換門勝率
  people[i] <- mean(x2)
}
mean(people)
var(people)

#bootstrap
library(boot)
# bootstrap function
boot_fun <- function(data, index){
  mean(data[index])
}

# bootstrap resampling:10000
boot_result <- boot(
  data = people,
  statistic = boot_fun,
  R = 10000
)
boot_result
# 取兩端 2.5% 和 97.5%
ci <- quantile(boot_result$t, c(0.025, 0.975))
hist(boot_result$t,
     probability = TRUE,
     breaks = 30,
     main = "Bootstrap Distribution",
     xlab = "Bootstrap Mean",
     ylab = "Density")
lines(density(boot_result$t),
      lwd = 2)
#quantile
abline(v = ci,
       col = "red",
       lwd = 2,
       lty = 2)
#平均數
abline(v = mean(people),
       col = "blue",
       lwd = 2)
legend("topright",
       legend = c("Density","Mean","95% CI"),
       col = c("black","blue","red"),
       lwd = 2,lty = c(1,1,2))


#jackknife 還不確定----
set.seed(1)
# inverse function
first_car <- function(n = 1000, p = 1/3){
  u <- runif(n)
  x <- as.integer(u <= p)
  return(x)
}
# 換門
change_door <- function(x){
  x2 <- ifelse(x == 0, 1, 0)
  return(x2)
}
# 100 個人，每人玩 1000 次
N <- 100
n <- 1000
# 每個人的換門勝率
people <- numeric(N)
for(i in 1:N){
  x <- first_car(n = n, p = 1/3)
  x2 <- change_door(x)
  people[i] <- mean(x2)
}

# 原始平均勝率
original_mean <- mean(people)
original_mean

# Jackknife
j <- length(people)
jack_mean <- numeric(j)

for(i in 1:j){
  jack_mean[i] <- mean(people[-i])
}
mean(jack_mean)

#se
jack_se <- sqrt((j - 1) / j * sum((jack_mean - mean(jack_mean))^2))
#bias
jack_bias <- (j - 1) * (mean(jack_mean) - original_mean)

#95% CI
jack_ci <- quantile(jack_mean, c(0.025, 0.975))

# Jackknife mean
jack_m <- mean(jack_mean)

hist(jack_mean,
     probability = TRUE,
     breaks = 20,
     main = "Jackknife Distribution",
     xlab = "Jackknife Mean",
     ylab = "Density")

lines(density(jack_mean),
      lwd = 2)
# mean:藍線
abline(v = jack_m,
       col = "blue",
       lwd = 2)

#95% CI:紅色虛線
abline(v = jack_ci,
       col = "red",
       lwd = 2,
       lty = 2)

legend("topright",
       legend = c("Density", "Mean", "95% CI"),
       col = c("black", "blue", "red"),
       lwd = 2,
       lty = c(1, 1, 2))

#permutation----
set.seed(1)
# inverse function：一開始是否選到跑車
first_car <- function(n = 1000, p = 1/3){
  u <- runif(n)
  x <- as.integer(u <= p)
  return(x)
}
# 換門後
change_door <- function(x){
  x2 <- ifelse(x == 0, 1, 0)
  return(x2)
}

# 不換門
stay_door <- function(x){
  return(x)
}

#兩個分佈相同
#換門
simulate_switch_distribution <- function(B = 1000, n = 1000){
  z <- numeric(B)
  for(k in 1:B){
    x <- first_car(n = n)
    y <- change_door(x)
    z[k] <- mean(y)
  }
  return(z)
}
#不換門
simulate_stay_distribution <- function(B = 1000, n = 1000){
  z <- numeric(B)
  for(k in 1:B){
    x <- first_car(n = n)
    y <- stay_door(x)
    z[k] <- mean(y)
  }
  return(z)
}

#兩組勝率分布
switch_dist <- simulate_switch_distribution(B = 1000, n = 1000)
stay_dist   <- simulate_stay_distribution(B = 1000, n = 1000)
mean(switch_dist) #0.666746
mean(stay_dist) #0.333708

#勝率差異 :0.333038
obs <- mean(switch_dist) - mean(stay_dist)
obs

# permutation test
B_perm <- 10000
permutation_dist <- numeric(B_perm)
z <- c(switch_dist, stay_dist)
n_switch <- length(switch_dist)

for(b in 1:B_perm){
  idx <- sample(length(z), replace = FALSE)
  z1 <- z[idx[1:n_switch]]
  z2 <- z[idx[(n_switch + 1):length(z)]]
  permutation_dist[b] <- mean(z1) - mean(z2)
}

# 單尾 p-value：檢定換門是否大於不換門 p=0
p_value <- mean(permutation_dist >= obs)
p_value


ci <- quantile(permutation_dist, c(0.025, 0.975))

xrange <- range(c(permutation_dist, obs, ci))
hist(permutation_dist,
     probability = TRUE,
     breaks = 30,
     main = "Permutation Null Distribution",
     xlab = "Mean Difference: Switch - Stay",
     ylab = "Density",
     xlim = xrange)
lines(density(permutation_dist),
      lwd = 2,
      col = "blue")

# 平均數
abline(v = mean(permutation_dist),
       col = "purple",
       lwd = 2)
# 95% CI
abline(v = ci,
       col = "red",
       lwd = 2,
       lty = 2)

# 觀察到的差異
abline(v = obs,
       col = "darkgreen",
       lwd = 2)
legend("topright",
       legend = c("Density","Permutation Mean","95% CI","Observed Difference"),
       col = c("blue","purple","red","darkgreen"),
       lwd = 2,
       lty = c(1, 1, 2, 1),
       bty = "n")

#MCMC----
set.seed(1)
n <- 1000
#step1 主持人知道車在哪，參賽者會贏的機率2/3  不知道1/3
w_known <- rbinom(n, size = 1, prob = 2/3)
w_unknown <- rbinom(n, size = 1, prob = 1/3)

#step2 mcmc利用posterior
#p機率在0~1之間
#再利用Bernoulli likelihood 的 log：
#prior設成Uniform(0,1)，所以posterior跟likelihood成正比
log_posterior <- function(p, y){
  if(p <= 0 || p >= 1){
    return(-Inf)
  }
  loglik <- sum(y * log(p) + (1 - y) * log(1 - p))
  return(loglik)
}

#step3 MH
MH <- function(y, m = 10000, proposal_sd = 0.03){
  p_chain <- numeric(m)
  p_chain[1] <- 0.5
  reject <- 0
  for(i in 2:m){
    current <- p_chain[i - 1]
    # proposal distribution
      proposal <- rnorm(1, mean = current, sd = proposal_sd)
      #計算新值跟舊值posterior差異
      #log ratio 可以轉
      log_r <- log_posterior(proposal, y) -log_posterior(current, y)
      #log ratio 轉回普通比例
      r <- exp(log_r)
      #隨機生成0到1的變數 
      u <- runif(1)
      #如果比新值好 則r>1 accept
      if(u <= min(1, r)){
        p_chain[i] <- proposal
      }else{
        p_chain[i] <- current
        reject <- reject + 1
      }
  }
  #如果拒絕，就留在原本的p 同時拒絕次數加 1
    return(list(chain = p_chain,
                reject_rate = reject / m))
}
#step4
mh_known <- MH(w_known)
mh_unknown <- MH(w_unknown)

#step5
#burn-in period:chain前2000次的抽樣先不用 ->從trace plot可知前期波動大
burnin <- 2000

#取出主持人知道情況下，第 2001 到 10000 次的p
p_known <- mh_known$chain[(burnin + 1):10000]
#取出主持人不知道情況下，第 2001 到 10000 次的p
p_unknown <- mh_unknown$chain[(burnin + 1):10000]


### Step 6：整理 posterior 結果 ###
#                   Posterior Mean   Posterior  SD        2.5%     97.5%      Reject Rate
#主持人知道車在哪        0.6653407       0.01512452   0.6355856 0.6938374      0.4978
#主持人不知道車在哪      0.3391910       0.01496857   0.3099947 0.3690889      0.5002
result <- rbind(
  "主持人知道車在哪" = c(
    mean(p_known),
    sd(p_known),
    quantile(p_known, 0.025),
    quantile(p_known, 0.975),
    mh_known$reject_rate
  ),
  
  "主持人不知道車在哪" = c(
    mean(p_unknown),
    sd(p_unknown),
    quantile(p_unknown, 0.025),
    quantile(p_unknown, 0.975),
    mh_unknown$reject_rate
  )
)
colnames(result) <- c("Posterior Mean", "Posterior SD",
                      "2.5%", "97.5%", "Reject Rate")

result
par(mfrow = c(1, 2))

hist(p_known,
     probability = TRUE,
     main = "Posterior of P-Value
     (host knows the correct answer)",
     xlab = "Winning probability p")

abline(v = mean(p_known),
       col = "red",
       lty = 2,
       lwd = 2)


hist(p_unknown,
     probability = TRUE,
     main = "Posterior of P-Value
     (host doesn't know the correct answer)",
     xlab = "Winning probability p")

abline(v = mean(p_unknown),
       col = "red",
       lty = 2,
       lwd = 2)

# Trace plot：主持人知道車在哪

plot(mh_known$chain,
     type = "l",
     main = "Trace Plot of P-Value
     (host knows the correct answer)",
     xlab = "iteration",
     ylab = "p")

abline(h = mean(p_known),
       col = "red",
       lwd = 2)

# Trace plot：主持人不知道車在哪
plot(mh_unknown$chain,
     type = "l",
     main = "Trace Plot of P-Value
     (host doesn't know the correct answer)",
     xlab = "iteration",
     ylab = "p")

abline(h = mean(p_unknown),
       col = "red",
       lwd = 2)


  
  