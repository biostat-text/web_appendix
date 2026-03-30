#------------------------------------------------------------------------------#
#-------------------------#
# 1-3 代表値              #
#-------------------------#

# ----- 1-3-1 平均値 -----#
# 表1-2のデータ
sugar <- c(6.0,3.0,0.0,9.0,5.0,5.0,3.0,8.0,9.0,2.0,5.0,6.0,3.0,1.0,9.3,3.0,8.0,
           3.0,10.0,13.0,12.0,12.0,11.0,15.0,13.0,0.0,13.0,0.0,6.0,6.4,5.0,1.0,
           0.0,0.0,0.0,0.0)
# 平均
mean_sugar <- mean(sugar)
print(mean_sugar)

# 加重平均（表1-4のデータを用いた平均）
weighted_mean <- (1.5*10+4.5*9+7.5*6+10.5*5+13.5*6)/36
print(weighted_mean)

# ----- 1-3-2 中央値 -----#
# 表1-5の6週目のデータ
acf6 <- c(1,3,5,1,2,1,1)
# 6週目の中央値
median_acf6 <- median(acf6)
print(median_acf6)

# 表1-5の12週目のデータ
acf12 <- c(3,1,2,6,0,0,4,1)
# 12週目の中央値
median_acf12 <- median(acf12)
print(median_acf12)


# ----- 1-3-3 最頻値 -----#
# 表1-5の6週目のデータ
acf6 <- c(1,3,5,1,2,1,1)
# 6週目の度数分布表
mode_acf6 <- table(acf6)
cat(
  "acf6 ", as.numeric(names(mode_acf6)), "\n",
  "度数 ", as.vector(mode_acf6), "\n"
)
# 6週目の最頻値
modes <- as.numeric(names(mode_acf6)[mode_acf6 == max(mode_acf6)])
freq  <- max(mode_acf6)
cat("acf6 =", paste(modes, collapse = ", "), ", 度数 =", freq, "\n")

# 表1-5の12週目のデータ
acf12 <- c(3,1,2,6,0,0,4,1)
# 12週目の度数分布表
mode_acf12 <- table(acf12)
cat(
  "acf12 ", as.numeric(names(mode_acf12)), "\n",
  "度数 ", as.vector(mode_acf12), "\n"
)
# 12週目の最頻値
modes <- as.numeric(names(mode_acf12)[mode_acf12 == max(mode_acf12)])
freq  <- max(mode_acf6)
cat("acf12 =", paste(modes, collapse = ", "), ", 度数 =", freq, "\n")


#------------------------#
# 1-4 散布度             #
#------------------------#

# ----- 1-4-1 分散 -----#
# 表1-2のデータ
sugar <- c(6.0,3.0,0.0,9.0,5.0,5.0,3.0,8.0,9.0,2.0,5.0,6.0,3.0,1.0,9.3,3.0,8.0,
           3.0,10.0,13.0,12.0,12.0,11.0,15.0,13.0,0.0,13.0,0.0,6.0,6.4,5.0,1.0,
           0.0,0.0,0.0,0.0)

# 分散
n <- length(sugar)
mean_sugar <- mean(sugar) 
var_sugar <- sum((sugar - mean_sugar)^2) / n
print(var_sugar)

# 標準偏差
sd_sugar <- sqrt(var_sugar)
print(sd_sugar)

# 加重分散
weighted_mean <- (1.5*10+4.5*9+7.5*6+10.5*5+13.5*6)/36
weighted_var <- (((1.5-weighted_mean)^2)*10+((4.5-weighted_mean)^2)*9+
  ((7.5-weighted_mean)^2)*6+((10.5-weighted_mean)^2)*5+
  ((13.5-weighted_mean)^2)*6)/36
print(weighted_var)

# 加重標準偏差
weighted_sd <- sqrt(weighted_var)
print(weighted_sd)

#----- 1-4-2 範囲 -----#
# 表1-5の18週目のデータ
acf18 <- c(10,6,6,7,5,7,6)
# 最小値
acf18_min <- min(acf18)
# 最大値
acf18_max <- max(acf18)
# 範囲
range_acf18 <- acf18_max - acf18_min 
print(range_acf18)

#----- 1-4-3 四分位範囲 -----#
# 表1-6の白血球数のデータ
wbc <- c(2300,750,4300,2600,6000,10500,10000,17000,5400,7000,9400,32000,35000,100000,
         100000,52000,100000,4400,3000,4000,1500,9000,5300,10000,19000,27000,28000,
         31000,26000,21000,79000,100000,100000)
# 表1-6の生存時間のデータ
time <- c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65,56,65,17,7,16,22,3,
          4,2,3,8,4,3,30,4,43)

# 四分位数と四分位範囲を算出する関数: quartiles
quartiles <- function(x) {
  x <- sort(x)
  n <- length(x)
  
  # Q2
  if (n %% 2 == 1) {
    Q2 <- x[(n + 1) / 2]
    lower <- x[1:((n - 1) / 2)]
    upper <- x[((n + 1) / 2 + 1):n]
  } else {
    Q2 <- mean(x[(n / 2):(n / 2 + 1)])
    lower <- x[1:(n / 2)]
    upper <- x[(n / 2 + 1):n]
  }
  
  # Q1, Q3
  Q1 <- median(lower)
  Q3 <- median(upper)
  
  # 四分位範囲（IQR）
  IQR <- Q3 - Q1
  
  return(list(Q1 = Q1, Q2 = Q2, Q3 = Q3, IQR = IQR))
}

# 白血球数のQ1. Q2, Q3, IQR
quartile_wbc  <- quartiles(wbc)
print(quartile_wbc)

# 生存時間のQ1. Q2, Q3, IQR
quartile_time <- quartiles(time)
print(quartile_time)

#------------------------#
# 1-5 視覚化             #
#------------------------#

# ----- 1-5-3 箱ひげ図 -----#
# 表1-2のデータ
sugar <- c(6.0,3.0,0.0,9.0,5.0,5.0,3.0,8.0,9.0,2.0,5.0,6.0,3.0,1.0,9.3,3.0,8.0,
           3.0,10.0,13.0,12.0,12.0,11.0,15.0,13.0,0.0,13.0,0.0,6.0,6.4,5.0,1.0,
           0.0,0.0,0.0,0.0)

quartiles <- function(x) {
  x <- sort(x)
  n <- length(x)
  
  # Q2（中央値）
  if (n %% 2 == 1) {
    Q2 <- x[(n + 1) / 2]
    lower <- x[1:((n - 1) / 2)]
    upper <- x[((n + 1) / 2 + 1):n]
  } else {
    Q2 <- mean(x[(n / 2):(n / 2 + 1)])
    lower <- x[1:(n / 2)]
    upper <- x[(n / 2 + 1):n]
  }
  
  # Q1, Q3
  Q1 <- median(lower)
  Q3 <- median(upper)
  
  # 四分位範囲
  IQR <- Q3 - Q1
  
  return(list(Q1 = Q1, Q2 = Q2, Q3 = Q3, IQR = IQR))
}

# sugarのQ1. Q2, Q3, IQR
quartile_sugar  <- quartiles(sugar)
print(quartile_sugar)
# 最小値
sugar_min <- min(sugar)
print(sugar_min)
# 最大値
sugar_max <- max(sugar)
print(sugar_max)