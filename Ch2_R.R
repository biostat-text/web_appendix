#------------------------------------------------------------------------------#
#-----------------------------------#
# 2-3 スクリーニング検査とROC曲線   #
#-----------------------------------#

# ----- 2-3-2 ROC曲線 -----#
# Heart Diseaseデータの読み込み
heart <- read.csv("heart.csv")

# 予測変数「最大心拍数」を選択
var <- "thalach"

# 任意のカットオフ値を設定
# 表2-3の場合は130、表2-4の場合は150に設定
cutoff <- 130

# 陽性（= 1）、陰性（= 0）の予測列を作成（cutoff値以上の場合1、未満の場合0）
heart$predicted <- ifelse(heart[[var]] >= cutoff, 1, 0)

# 各セルの度数を計算
TP <- sum(heart$predicted == 1 & heart$target == 1)              # 真陽性（陽性かつ心疾患あり）
FP <- sum(heart$predicted == 1 & heart$target == 0)              # 偽陽性（陽性かつ心疾患なし）
FN <- sum(heart$predicted == 0 & heart$target == 1)              # 偽陰性（陰性かつ心疾患あり）
TN <- sum(heart$predicted == 0 & heart$target == 0)              # 真陰性（陰性かつ心疾患なし）

# 2×2の分割表
table_core <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE)  # 2×2の分割表のセル部分
rownames(table_core) <- c("陽性", "陰性")                        # 2×2の分割表の行名を変更
colnames(table_core) <- c("心疾患あり", "心疾患なし")            # 2×2の分割表の列名を変更
table_full <- cbind(table_core, 行合計 = rowSums(table_core))    # 行の合計を追加
table_full <- rbind(table_full, 列合計 = colSums(table_full))    # 列の合計を追加
table_full["列合計", "行合計"] <- sum(table_core)
print(table_full)