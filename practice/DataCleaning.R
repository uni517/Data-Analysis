library(dplyr)
library(tidyr)

# 變數與觀測值
# 變數指的是資料集中的欄位
# 觀測值指的是資料集中的列表

# 1.2 清理資料
# 1.2.1	選擇變數（欄位）

# 啟動dplyr套件: 包含select、filter、mutate、summarize、group_by功能
library("dplyr")

# 本章節使用dplyr套件附帶的資料集「星際大戰(Star Wars)」進行相關操作
# 該資料集包含13個欄位(characteristics)及87個列表(Characters)
# 使用 help或 ? 功能，查詢其他功能或資料集的說明
# 查詢starwar資料集的說明
?starwars
help("starwars")

# 使用 View 功能檢索資料集內容
View(starwars)

# 運用select功能，選定子集合內所包含資料集中的特定變數（欄位）
# 創建子集合，保留星戰電影資料集中的欄位：角色名字、身高與體重
subset_base <- select(starwars, name, height, mass)
subset_base
# 創建子集合，保留星戰電影中的角色名字、以及選定的連續範圍
# 選定的範圍從頭髮顏色到眼睛顏色
subset_range <- select(starwars, name, hair_color:eye_color)
subset_range

# 創建子集合，去除星戰電影中的性別與種族後的所有資料
subset_remove <- select(starwars, -sex, -species)
subset

# 1.2.2 選擇觀測值（列表）
# 運用filter功能，過濾子集合內包含資料集中的特定觀測值（列表）
# 與函數結合，增加限定的條件，例如：|(OR) 、 & (AND)及判定符號(>, <)
library(dplyr)

# 創建子集合，選定所有男性角色
subset_man <- filter(starwars, sex == "male")
subset_man

# 創建子集合，選定所有男性角色，且有褐色眼睛
subset_man_eyes_brown <- filter(starwars,  sex == "male" & eye_color == "brown")
subset_man_eyes_brown

# 創建子集合，選定所有角色，眼睛顏色分別是以下條件
# 藍色、褐色、黃色
subset_all_eyes <- filter(starwars, eye_color == "blue" |
                           eye_color == "brown"|
                           eye_color == "yellow")
subset_all_eyes

# 使用 %in% 函數包含上述所有條件，使程式碼更簡潔
subset_in <- filter(starwars, eye_color %in% c("blue", "brown", "yellow"))
subset_in
# 1.2.3 創建/重新編碼變數
# 運用mutate功能，創建新的或轉換舊有的變數
library(dplyr)

# 創建子集合，將星戰角色的身高單位從公分轉換為公尺
# 同時將體重單位從公斤轉換為臺斤
subset_unit_change <- mutate(starwars, 
                 height = height /100,
                 mass = mass*1.66667)
subset_unit_change

# 使用ifelse功能，將判定結果紀錄到子集合中的新的變數
# 參數：ifelse(條件, 條件為真的情況下執行, 條件為否的情況下執行)
library(dplyr)
# 條件：體重高於50公斤
# 條件為真的情況下執行：紀錄為 適中
# 條件為否的情況下執行：紀錄為 偏瘦
subset_kg <- mutate(starwars, record = ifelse(mass < 50, 
                                           "normal", 
                                           "thin"))
subset_kg

# 創建子集合，轉換星戰角色中的特定種族
# 將非Human, Droid, Rodian的種族轉換為other
subset_race <- mutate(starwars, species = ifelse(species %in% c("Human", "Droid", "Rodian"), 
                                            species,
                                            "other"))
subset_race

# 創建子集合，轉換星戰角色中體重高於75或小於25的數值為不適用
subset_kg_75 <- mutate(starwars, mass = ifelse(mass < 25 | height > 75, NA, height))
subset_kg_75

# 1.2.4 總匯數據
# 使用summarize 功能，將大量的數值資料轉換為單一數值
# summarize 功能經常與by_group 功能一起使用，進行統計運算
# 在功能參數中給定 na.rm=TRUE，使功能執行排除遺漏值的影響
library(dplyr)

# 1.2.5 功能操作: pipes
# dplyr 及 tidyr 套件提供 pipe功能
# 呼叫pipe功能 %>% 使程式更加簡潔

library(dplyr)
# 創建子集合，計算星戰角色中男性的平均體重且依照皮膚顏色分類
subset_male <- filter(starwars, 
                 sex == 'male')
subset_skin_color <- group_by(subset, skin_color)
subset_color_from_kg <- summarize(subset,
                    mean_wt = mean(mass, na.rm = TRUE), .groups = 'drop')
subset_color_from_kg

# 運用 pipe (%>%) 功能，將前者輸出的結果，帶入後者程式的第一參數
subset <- starwars %>%
  filter(sex == "male") %>%
  group_by(species) %>%
  summarize(mean_wt = mean(mass, na.rm = TRUE))
subset

# 1.2.6 重塑資料
# 運用 tidyr 套件對欄位進行重塑
# 當功能需要比較少的欄位資訊時，使用gather功能縮減欄位
# 將特定原始欄位的內容建構到縮減後的欄位下，使子集合中列表資訊量變多
library(tidyr)

# 將星戰資料集中的欄位從頭髮顏色到星際戰艦包含的資訊，
# 建構到variable以及value的欄位下
long_subset <- gather(starwars,
                      key = "variable",
                      value = "value",
                      hair_color:starships)
long_subset

# 將前面建構的長子集合重塑回寬集合
wide_subset <- spread(long_subset, variable, value)
# !重塑回寬集合後，可能遺失原有的資料排序！
wide_subset

# 1.2.7 遺漏值處理
# 資料可能因為某些因素無法收集到完整資料
# 使得資料純在遺漏值(NA)，影響分析結果
# 透過以下方式處理遺漏值

# 1.2.7.1 從欄位處理
# 找出最多遺漏值的欄位先行處理
# 使用mice套件的md.pattern功能
library(mice)
md.pattern(starwars) 

# 先計算每個欄位遺漏值的總數量
# 除以觀測值的總數量取得平均值（取到小數點第二位）

# 標記星戰資料集中所有欄位資訊下的遺漏值
# 創建子集合，運用 is.na功能 將資料集遺漏值標記為TRUE，其餘為FALSE
subset_na <- is.na(starwars)
# 創建新的子集合，計算前述子集合中每個欄位遺漏值是TRUE的總數
subset_na_sum <- colSums(subset_na)
subset_na_sum
# 除以觀測值的總數量，取得遺漏值在每欄中的整體平均值
# 運用nrow功能，計算觀測值得總數量
starwarsmiss <- subset/nrow(starwars)
starwarsmiss 
# 運用round功能，取平均後兩位
round(starwarsmiss, 2)
# 從計算結果發現，birth_year欄位有51%的遺漏值存在
# 高達五成的角色出身年份為遺漏值
# 評估是否移除該欄位或進一步處理遺漏值

# 1.2.7.2 從列表處理
# 檢查每列中是否存在遺漏值並直接移除該列

# 建立一個子集合選定要判斷的遺漏值欄位
# 範例中選擇所有欄位
# 運用 na.omit功能移除欄位子集合中出現遺失值的列表
subset_na <- select(starwars, name:starships)%>%na.omit()
subset_na

# 執行後的結果從87筆資料列表，降為29筆資料
# 遺失58筆資料，該方法會造成資訊遺失（information loss）

# 1.2.7.3	以「平均數」或「第一四分位數」手動填補遺漏值

# 以該欄位的平均數填補遺漏值
subset_avg <- starwars
mean.7 <- colMeans(subset_avg[, 7], na.rm = TRUE)
na.rows <- is.na(subset_avg[, 7])
subset_avg[na.rows, 7] <- mean.7
subset_avg
# ！請忽略以下警告訊息！
# Warning message:
# The `i` argument of ``[`()` can't be a matrix as of tibble 3.0.0.
# Convert to a vector.
# 由於starwars資料集為dataframe格式，R將自動轉換該操作為Vector格式

# 以該欄位的第一四分位數填補遺漏值
subset_quantile <- starwars
quantile.7 <- quantile(subset_quantile[, 7], na.rm = TRUE, 0.25)
na.rows <- is.na(subset_quantile[, 7])
subset_quantile[na.rows, 7] <- quantile.7
subset_quantile

# 1.2.7.4	使用功能執行插補：平均數、中位數、眾數
# 當資料集變化較小，或變化對應的作用範圍較小
# 使用這種粗略的近似值插補遺漏值，是可以被接受的
# 且可能會得出滿意的結果

# 使用Hmisc套件中的 impute功能，進行遺漏值插補
install.packages("Hmisc")
library(Hmisc)
# 創建子集合，將星戰資料集中的生日年份以平均值插補
subset_birth_avg <- impute(starwars$birth_year, mean)
# 創建子集合，將星戰資料集中的生日年份以中位數插補
subset_birth_mid <- impute(starwars$birth_year, median)
# 創建子集合，將星戰資料集中的生日年份以平眾數插補
subset_birth_mod <- impute(starwars$birth_year, mode)
# 創建子集合，將星戰資料集中的生日年份以特定值（30）插補
subset_birth_30 <- impute(starwars$birth_year, 30)

# 1.2.7.5	預測遺漏值
# 使用VIM套件進行插補


# 1.2.7.5.1	K-近鄰演算法（kNN Imputation）
# kNN（k-Nearest Neighbours）

# 將starwars內的資料皆轉換為datafram 做處理
# for 迴圈 處理 film (12)欄位將list資料轉為data.frame的字串
starwars2 <- subset(starwars, select = 1:11)
for (x in c(12:14)) {
  for (i in c(1:87)) {
    unlistdata <-as.character(unlist(starwars[i, x])) 
    data <- unlistdata %>% strsplit(.,split = '/') %>% unlist(.,recursive = F) %>% paste(.,collapse = '/') 
    starwars2[i, x] <- data
  }
}
starwars2
# 重新命名欄位
colnames(starwars)
colnames(starwars2)
require(dplyr)
starwars2 <- dplyr::rename(starwars2, 
     films = ...12,
     vehicles = ...13,
     starships = ...14
)
starwars2
# 使用5個最接近的鄰居推算星戰資料集中的遺漏值
library(dplyr)
library(VIM)
require(DMwR)
require(class)
install.packages("kknn")
require(kknn)


?kNN
starwars2_1<-starwars2[-1,]
starwars2_1
training_idx <- c(sample(1:30,14), sample(31:60,14), sample(61:86,15))
starwars2.training<-starwars2_1[training_idx,]
starwars2.test<-starwars2_1[-training_idx,]

?kNN
imputeData <- VIM::kNN(starwars2, k=5)
imputeData

length(starwars2.training[,7])

# 多重插補
# 數值型資料
# PMM相當於某一指標的平均值作為插補，會出現插補值重複的問題
library(mice)
imputed_Data <- mice(starwars2[, 2:3], m=5, maxit = 50, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data,5)
completeData

#build predictive model
fit <- with(data = starwars2, exp = lm(height ~ mass)) 

#combine results of all 5 models
combine <- mice::pool(fit)
summary(combine)



# 預測類別變數model
library(rpart)
class_mod <- rpart(formula = birth_year ~ ., data = starwars2[!is.na(starwars$birth_year), 5:11], method = "class", na.action = na.omit)
birth_year_predict <- predict(object = class_mod,newdata = starwars2[is.na(starwars$birth_year),]) 
# 預測數值變數model
anova_mod <- rpart(formula = birth_year ~ ., data = starwars[!is.na(starwars$birth_year), 5:11], method = "anova", na.action = na.omit)
birth_year_predict <- predict(object = anova_mod, newdata = starwars[is.na(starwars$birth_year),])


