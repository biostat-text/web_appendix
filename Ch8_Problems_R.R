#------------------------------------------------------------------------------#
# 第8章問題                                                                    #
#------------------------------------------------------------------------------#

#------------------------#
# 問1                    #
#------------------------#

#-------- 問1 ① --------#
# データ
type_b <- c(8.3,3.8,3.9,7.8,9.1,15.4,7.7,6.5,5.7,13.6)
type_c <- c(10.2,9.2,9.6,53.8,15.8)

# Wilcoxonの順位和検定（正確検定を有効にするためにexact=TRUEを指定）
wilcox.test(type_b, type_c, 
            alternative = "two.sided",                # two.sidedで両側検定を指定
            exact = TRUE)

#-------- 問1 ② --------#
# データ
type_b <- c(8.3,3.8,3.9,7.8,9.1,15.4,7.7,6.5,5.7,13.6)
type_u <- c(5.1,12.9,13,2.6,30,20.5)

# Wilcoxonの順位和検定（正確検定を有効にするためにexact=TRUEを指定）
wilcox.test(type_b, type_u, 
            alternative = "two.sided",                 # two.sidedで両側検定を指定
            exact = TRUE)

#-------- 問1 ③ --------#
# データ
type_c <- c(10.2,9.2,9.6,53.8,15.8)
type_u <- c(5.1,12.9,13,2.6,30,20.5)

# Wilcoxonの順位和検定（正確検定を有効にするためにexact=TRUEを指定）
wilcox.test(type_c, type_u, 
            alternative = "two.sided",                  # two.sidedで両側検定を指定
            exact = TRUE)


#------------------------#
# 問2                    #
#------------------------#
# データ
sulindac <- c(6,4,16,6,142,1,27,10,6,5,8)
placebo <- c(67,5,31,20,7,347,16,20,26,45,30)

# Wilcoxonの順位和検定（正規近似検定を有効にするためにexact=FALSEを指定）
wilcox.test(sulindac, placebo, 
            alternative = "two.sided",                   # two.sidedで両側検定を指定
            exact = FALSE, 
            correct = FALSE)


#------------------------#
# 問3                    #
#------------------------#

#-------- 問3 ① --------#
# データ
good_normal <- c(50,40,117,97,36)
good_low <- c(192,158,103,197,150)

# Wilcoxonの符号付順位検定（正確検定を有効にするためにexact=TRUEを指定）
wilcox.test(good_normal, good_low,
            paired = TRUE, 
            alternative = "two.sided",                   # two.sidedで両側検定を指定    
            exact = TRUE,
            correct = FALSE)

#-------- 問3 ② --------#
# データ
poor_normal <- c(87,35,36,72,20)
poor_low <- c(166,27,70,94,52)

# Wilcoxonの符号付順位検定（正確検定を有効にするためにexact=TRUEを指定）
wilcox.test(poor_normal, poor_low,
            paired = TRUE, 
            alternative = "two.sided",                    # two.sidedで両側検定を指定
            exact = TRUE,
            correct = FALSE)


#------------------------#
# 問4                    #
#------------------------#
# データ
group1 <- c(0.7,-1.6,-0.2,-1.2,-0.1,3.4,3.7,0.8,0,2)
group2 <- c(1.9,0.8,1.1,0.1,-0.1,4.4,5.5,1.6,4.6,3.4)

# Wilcoxonの符号付順位検定（正規近似検定を有効にするためにexact=FALSEを指定）
wilcox.test(group1, group2,
            paired = TRUE, 
            alternative = "two.sided",                      # two.sidedで両側検定を指定
            exact = FALSE,
            correct = FALSE)
