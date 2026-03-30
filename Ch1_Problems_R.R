#------------------------------------------------------------------------------#
# 第1章問題                                                                    #
#------------------------------------------------------------------------------#

#------------------------#
# 問3                    #
#------------------------#

#-------- 問3 ① ---------#
# データ
bmiq <- c(100,71,89,73,64,64,104,76,81,78,79,128,65,71,75,109,88,90,96,95)
age2iq <- c(120,131,126,120,126,125,105,130,107,104,120,120,114,122,119,102,133,95,82,136)
age4iq <- c(115,109,115,102,125,109,107,112,120,108,117,128,102,100,101,107,121,89,106,115)
age8iq <- c(109,113,113,111,114,96,106,124,109,125,114,148,112,128,102,113,115,115,105,118)
age13iq <- c(106,95,90,121,96,87,104,125,115,124,109,127,122,119,97,108,97,97,105,104)

target_name <- "bmiq" # データ名を選択："age2iq", "age4iq", "age8iq", "age13iq"
target <- bmiq  # データ名を選択：age2iq, age4iq, age8iq, age13iq

# 四分位数と四分位範囲を算出する関数
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

# Q1, Q2, Q3, IQRの抽出
qs <- quartiles(target)
q1 <- qs$Q1
q2 <- qs$Q2
q3 <- qs$Q3
iqr <- qs$IQR

# 平均・分散・標準偏差
n <- length(target)
mean <- mean(target)
var <- sum((target - mean)^2) / n
sd <- sqrt(var)

# 範囲
range <- diff(range(target))

# 結果の集計
target_stats <- data.frame(
  IQスコア = target_name,
  平均 = round(mean, 2),
  中央値 = round(q2, 2),
  分散 = round(var, 2),
  標準偏差 = round(sd, 2),
  範囲 = round(range, 2),
  Q1 = round(q1, 2),
  Q3 = round(q3, 2),
  IQR = round(iqr, 2),
  row.names = NULL
)
print(target_stats)


#-------- 問3 ② ---------#
# 階級の設定
min <- floor(min(target, na.rm = TRUE) / 10) * 10         # 階級の最小値
max <- ceiling(max(target, na.rm = TRUE) / 10) * 10       # 階級の最大値
breaks <- seq(min, max, by = 10)                          # 変数ごとに調整が必要:

# ヒストグラムデータの作成
hist_data <- hist(target, breaks = breaks, right = FALSE, plot = FALSE)
# 階級値
class_mid <- (head(hist_data$breaks, -1) + tail(hist_data$breaks, -1)) / 2

# 度数
freq <- hist_data$counts
# 相対度数
rel_freq <- round(freq / sum(freq), 2)
# 累積度数
cum_freq <- cumsum(freq)
# 累積相対度数
cum_rel_freq <- round(cumsum(freq) / sum(freq), 3)

# 結果の集計
output <- data.frame(
  `階級（以上～未満）` = paste0(head(breaks, -1), "～", tail(breaks, -1)),
  `階級値` = class_mid,
  `度数` = freq,
  `相対度数` = rel_freq,
  `累積度数` = cum_freq,
  `累積相対度数` = cum_rel_freq,
  check.names = FALSE
)
formatted_output <- data.frame(
  format(output, justify = "left", width = 25),
  check.names = FALSE
)
print(formatted_output, row.names = FALSE, right = TRUE)


#-------- 問3 ③ ---------#
# 加重平均
weighted_mean <- sum(class_mid * freq) / sum(freq)
# 加重分散
weighted_var <- sum(((class_mid - weighted_mean)^2)*freq) / sum(freq)
# 加重標準偏差
weighted_sd <- sqrt(weighted_var)
# 結果の集計
target_weighted <- data.frame(
  IQスコア = target_name,
  加重平均 = round(weighted_mean, 2),
  分散 = round(weighted_var, 2),
  標準偏差 = round(weighted_sd, 2),
  row.names = NULL
)
print(target_weighted)


#------------------------#
# 問4                    #
#------------------------#

#--------- 問4 ① --------#
# データ
calories <- c(350,350,420,490,130,370,460,370,310,420,380,320,300,420,310,480,490,410,130,280)
fat	<- c(8,9,20,19,6,14,22,14,18,25,17,12,17,21,5,18,18,24,7,2)
carb <-	c(67,64,59,75,17,47,61,55,32,39,51,53,34,57,52,70,73,46,16,56)
fiber <-	c(5,7,0,4,0,5,2,0,0,0,2,3,2,2,3,3,2,3,0,2)
protein <- c(10,6,5,7,0,6,7,6,5,7,4,6,5,5,12,7,8,6,0,10)

target_name <- "protein" # データ名を選択："calories", "fat", "carb", "fiber", "protein"
target <- protein  # データ名を選択：calories, fat, carb, fiber, protein

# 四分位数と四分位範囲を算出する関数
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

# Q1, Q2, Q3, IQRの抽出
qs <- quartiles(target)
q1 <- qs$Q1
q2 <- qs$Q2
q3 <- qs$Q3
iqr <- qs$IQR

# 平均・分散・標準偏差
n <- length(target)
mean <- mean(target)
var <- sum((target - mean)^2) / n
sd <- sqrt(var)

# 範囲
range <- diff(range(target))

# 結果の集計
target_stats <- data.frame(
  IQスコア = target_name,
  平均 = round(mean, 2),
  中央値 = round(q2, 2),
  分散 = round(var, 2),
  標準偏差 = round(sd, 2),
  範囲 = round(range, 2),
  Q1 = round(q1, 2),
  Q3 = round(q3, 2),
  IQR = round(iqr, 2),
  row.names = NULL
)
print(target_stats)


#--------- 問4 ② --------#
# 階級の設定
min <- floor(min(target, na.rm = TRUE) / 10) * 10         # 階級の最小値
max <- ceiling(max(target, na.rm = TRUE) / 10) * 10       # 階級の最大値
breaks <- seq(min, max, by = 3)                           # 変数ごとに調整が必要

# ヒストグラムデータの作成
hist_data <- hist(target, breaks = breaks, right = FALSE, plot = FALSE)
# 階級値
class_mid <- (head(hist_data$breaks, -1) + tail(hist_data$breaks, -1)) / 2

# 度数
freq <- hist_data$counts
# 相対度数
rel_freq <- round(freq / sum(freq), 2)
# 累積度数
cum_freq <- cumsum(freq)
# 累積相対度数
cum_rel_freq <- round(cumsum(freq) / sum(freq), 3)

# 結果の集計
output <- data.frame(
  `階級（以上～未満）` = paste0(head(breaks, -1), "～", tail(breaks, -1)),
  `階級値` = class_mid,
  `度数` = freq,
  `相対度数` = rel_freq,
  `累積度数` = cum_freq,
  `累積相対度数` = cum_rel_freq,
  check.names = FALSE
)
formatted_output <- data.frame(
  format(output, justify = "left", width = 25),
  check.names = FALSE
)
print(formatted_output, row.names = FALSE, right = TRUE)


#--------- 問4 ③ --------#
# 加重平均
weighted_mean <- sum(class_mid * freq) / sum(freq)
# 加重分散
weighted_var <- sum(((class_mid - weighted_mean)^2)*freq) / sum(freq)
# 加重標準偏差
weighted_sd <- sqrt(weighted_var)
# 結果の集計
target_weighted <- data.frame(
  IQスコア = target_name,
  加重平均 = round(weighted_mean, 2),
  分散 = round(weighted_var, 2),
  標準偏差 = round(weighted_sd, 2),
  row.names = NULL
)
print(target_weighted)