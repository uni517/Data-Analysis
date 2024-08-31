# 簡單線性迴歸
# 迴歸表示式
# Y = a + bX + e
# a是常數、b是相關係數、e是誤差值（希望是常態分佈）
# 認為 X 跟Y 有關時
# 利用 cars 資料集 包含的速度、煞車距離（兩個欄位）


install.packages("ggplot2")
library(ggplot2)
# 先畫散步圖觀察
ggplot(cars, aes(x = speed, y = dist)) + geom_point(shape = 10
                                                    , size = 5)
# 訓練模型
# x = 速度，y = 煞車距離
# lm(y~x)

carsLM <- lm(dist ~ speed, data = cars)
# 散步圖 加上模型預測區域
ggplot(cars, aes(x = speed, y = dist)) + geom_point(shape = 10, size = 5) + geom_smooth(method = lm) + labs(x= "速度", y = "煞車距離")

# 方程式係數取得
summary(carsLM)

# Adjust R-squared = 0.6438
# 越接近1，解釋力越強大
# 可以從模型摘要中取得方程式中的參數
# 常數a=-17.5791 係數b=3.9324
# 假設速度20
# Y= -17.5791 + 3.9324 * 20 = 61.0689
# 透過 predict() 功能，自動計算（預測）以上結果
new <- data.frame(speed = 20)
result <- predict(carsLM, newdata = new)
result
# 預測結果為61.06908 

# 把預測座標用geom_point點在圖上，並用紅色標記
ggplot(cars, aes(x = speed, y = dist)) + geom_point(shape = 10, size = 5) + geom_point(x = new$speed, y = result, size = 10, shape = 17, color = "red") + geom_smooth(method = lm) + labs(x = "速度", y = "煞車距離")
