#------------------------------------------------------------------------------#
# 第4章問題                                                                    #
#------------------------------------------------------------------------------#

#------------------------#
# 問1                    #
#------------------------#

#-------- 問1 ① ---------#
mu <- 135                                 # 母平均
sigma <- 30                               # 母標準偏差

# 1人のLDL値が145mg/dL以上となる確率
p <- 1 - pnorm(145, mean = mu, sd = sigma)
p

#-------- 問1 ② ---------#
# 標準誤差
se <- sigma / sqrt(25)
# 標本平均が145以上になる確率
p_sample_mean <- 1 - pnorm(145, mean = mu, sd = se)
p_sample_mean

#-------- 問1 ③ ---------#
# 標準誤差
se <- sigma / sqrt(250)
# 標本平均が145以上になる確率
p_sample_mean <- 1 - pnorm(145, mean = mu, sd = se)
p_sample_mean

#-------- 問1 ④ ---------#
# 下側10%の位置にあるLDL値
qnorm(0.10, mean = mu, sd = sigma)

#-------- 問1 ⑤ ---------#
n <- 25
# 標本平均の標準誤差
se <- sigma / sqrt(n)

# 下側10%点
qnorm(0.10, mean = mu, sd = se)


#------------------------#
# 問2                    #
#------------------------#
# 男性100名のうち糖尿病疑いの割合が20%以上となる確率
p_male <- 0.197                     # 男性の母集団糖尿病割合
n_male <- 100                       # 男性の標本サイズ

p_hat_male <- 0.20
se_male <- sqrt(p_male * (1 - p_male) / n_male) # 標準誤差
z_male <- (p_hat_male - p_male) / se_male       # 標準化（Zスコア）
p_male_result <- 1 - pnorm(z_male)              # 正規分布による近似確率

# 結果の出力
cat("男性の糖尿病疑いの割合が20%以上になる確率:", round(p_male_result, 4), "\n")

# 女性100名のうち糖尿病疑いの割合が20%以上となる確率
p_female <- 0.108                    # 女性の母集団糖尿病割合
n_female <- 100                      # 女性の標本サイズ

p_hat_female <- 0.20
se_female <- sqrt(p_female * (1 - p_female) / n_female) # 標準誤差
z_female <- (p_hat_female - p_female) / se_female       # 標準化（Zスコア）
p_female_result <- 1 - pnorm(z_female)                  # 正規分布による近似確率

# 結果の出力
cat("女性の糖尿病疑いの割合が20%以上になる確率:", round(p_female_result, 4), "\n")