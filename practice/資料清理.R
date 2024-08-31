# Missing Value Treatment
# 判斷遺失值在整體資料中所佔的比例
# 5%內刪除資料列，超過5%進行遺失值處理

# 資料集準備
data("BostonHousing", package = "mlbench")
View(BostonHousing)
??BostonHousing

# 由於BostomHousing資料集中沒有遺失值
# 藉由隨機插入遺失值後預測遺失值
# 最後將預測值與實際值進行比較，評估各個遺失值處理法的效果好壞

# 載入資料集，隨機將rad和ptraio的40列變成NA
original <- BostonHousing

set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing),40),"rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing),40), "ptratio"] <- NA

# 檢視隨機插入遺失值後的前幾列
head(BostonHousing)

# 使用mice套件，檢視遺失值的數值及視覺化遺漏值的分佈
install.packages("mice")
library(mice)
md.pattern(BostonHousing) # missing data pattern

# 1. 刪除資料列法
# 當資料量夠大，模型不會失去預測能力
# 當不構成偏差，即原始目標事件比例不會因為刪除遺失值而發生改變
# 可以在建立模型時，將參數設定為 na.action=na.omit
# 雖然na.omit已經是預設值

# 使用lm 功能，直接建立線性預測模型
BHLM <- lm(medv ~ ptratio + rad, data = BostonHousing, na.action = na.omit) 
summary(BHLM)
# 示範用隨機挑選的變數關係，預測結果不準確（R平方=0.2861）
# 實務上不能用該線性模型填補遺漏值

# 可以使用Hmisc套件的impute()來填補
install.packages("Hmisc")
library(Hmisc)
impute(BostonHousing$ptratio, mean)  # 以平均數填補
impute(BostonHousing$ptratio, median)  # 以中位數填補
impute(BostonHousing$ptratio, 20)  # 以自訂值填補
# 或是手動填補
BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- mean(BostonHousing$ptratio, na.rm = T)

# 計算使用平均值填補的正確率
library(DMwR)
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- rep(mean(BostonHousing$ptratio, na.rm = TRUE), length(actuals))

regr.eval(trues = actuals, preds = predicteds)

library(DMwR)
# 將目標變數移除，進行knn預測填補
knnOutput <- knnImputation(data = BostonHousing[,!names(BostonHousing)%in% 'medv'])
anyNA(knnOutput)

actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- knnOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals, predicteds)





library(rpart)
# 預測類別變數model
class_mod <- rpart(formula = rad ~ . -medv, data = BostonHousing[!is.na(BostonHousing$rad),], method = "class", na.action = na.omit)
# 預測數值變數model
anova_mod <- rpart(formula = ptratio ~ . -medv, data = BostonHousing[!is.na(BostonHousing$ptratio),], method = "anova", na.action = na.omit)

rad_predict <- predict(object = class_mod, newdata = BostonHousing[is.na(BostonHousing$rad),])
ptratio_predict <- predict(object = anova_mod, newdata = BostonHousing[is.na(BostonHousing$ptratio),])


# 計算使用rpart決策樹法填補ptratio的精準度
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds <- ptratio_predict
regr.eval(trues = actuals, preds = predicteds)
# MAPE(平均絕對誤差百分比)改善(降低)了30%(from 0.059 to 0.041)。

# 計算使用rpart決策樹法填補rad的精準度
actuals <- original$rad[is.na(BostonHousing$rad)]
predicteds <- as.numeric(colnames(rad_predict)[apply(rad_predict, 1, which.max)])
mean(actuals != predicteds)
