###以下模擬一開始選到跑車的機率###

#使用inverse function生成Bernoulli(p = 1/3)的隨機變數
set.seed(1)

n = 10000 # 變數的數量
p = 1/3 # 三門問題中，一開始選到跑車的機率是1/3
u = runif(n)
x = as.integer(u > 1-p)


#透過R內建的生成Bernoulli隨機變數的function進行驗證

x_ver = rbinom(n, size = 1, prob = p)
mean(x_ver)
var(x_ver)

result_1 = c(mean(x),var(x)) # inverse function產生的結果
result_2 = c(mean(x_ver),var(x_ver)) # 內建函數的結果

# 驗證
result = rbind(
  inverse_method = result_1,
  "R裡面內建的函數"  = result_2
)

colnames(result) = c("mean", "var")
result

###以下為結果###
# > result
# mean       var
# inverse_method  0.3367 0.2233554
# R裡面內建的函數 0.3338 0.2223998





