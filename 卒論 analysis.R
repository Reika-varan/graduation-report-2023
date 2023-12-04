rm(list=ls(all=TRUE)) ## clean R

## ファイルの読み込み(エラーが出ない方を選ぶ)
warddata2 <- read.csv("Ward_AllData2.csv", encoding = "Shift_JIS")
attach(warddata2)

#####packages install####
install.packages("DataExplorer")
install.packages("inspectdf")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("summarytools")
install.packages("rempsyc")
install.packages("flextable")
install.packages("stargazer")
install.packages("knitr")
install.packages("ggrepel")
install.packages("corrplot")


#####data check#####
summary(warddata2)
dfSummary(warddata2) #https://www.xquartz.org/ のダウンロード
summarytools::view(dfSummary(warddata2))

####パートナーシップ、選択的夫婦別姓、給食無償化####
#histgram
######parnershipのヒストグラム#####
#female politicians
par(mfrow=c(1,2))
hist1 <- 
  hist(FemalePoliticians_2015[Partnership2016=="0"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#ff000050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2015\n(Without Partnership system)",
     xlab="The proportion of female politicians \nin the assembly2015")
abline(v = median(FemalePoliticians_2015[Partnership2016=="0"]),  # Add line for median
       col = "red",
       lwd = 3)
text(x = median(FemalePoliticians_2015[Partnership2016=="0"]) * 1.5,  # Add text for median
     y = median(FemalePoliticians_2015[Partnership2016=="0"]) * 30,
     paste("Median =", median(FemalePoliticians_2015[Partnership2016=="0"])),
     col = "red",
     cex = 0.9)

hist2 <- 
  hist(FemalePoliticians_2015[Partnership2016=="1"],xlim=c(0,0.5), ylim =c(0,10),
       freq=T, col="#ff000050", cex.main=1,
       main="The proportion of female politicians \nin the assembly2015\n(With Partnership system)",
       xlab="The proportion of female politicians \nin the assembly2015", 
       ylab= "Frequency")
abline(v = median(FemalePoliticians_2015[Partnership2016=="1"]),  # Add line for median
       col = "red",
       lwd = 3)
text(x = median(FemalePoliticians_2015[Partnership2016=="1"]) * 1.5,  # Add text for median
     y = median(FemalePoliticians_2015[Partnership2016=="1"]) * 28,
     paste("Median =\n", median(FemalePoliticians_2015[Partnership2016=="1"])),
     col = "red",
     cex = 0.9)
##独立2群なt検定
install.packages("broom")
library(broom)
library(magrittr)
t.test(FemalePoliticians_2015[Partnership2016=="0"],FemalePoliticians_2015[Partnership2016=="1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2015[Partnership2016=="0"],FemalePoliticians_2015[Partnership2016=="1"], 
       paired = FALSE,var.equal = TRUE) %>%
  tidy()

hist3 <- 
  hist(FemalePoliticians_2022[Partnership=="0"],xlim=c(0,0.5), ylim =c(0,10),
       freq=T, col="#00ff0050", cex.main=1,
       main="The proportion of female politicians \nin the assembly2022\n(Without Partnership system)",
       xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median(FemalePoliticians_2022[Partnership=="0"]),  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.40,  # Add text for median
     y = 8 ,
     paste("Median =\n", median(FemalePoliticians_2022[Partnership=="0"])),
     col = "seagreen",
     cex = 0.9)

hist4 <- 
  hist(FemalePoliticians_2022[Partnership=="1"],xlim=c(0,0.5), ylim =c(0,10),
       freq=T, col="#00ff0050", cex.main=1,
       main="The proportion of female politicians \nin the assembly2022\n(With Partnership system)",
       xlab="The proportion of female politicians \nin the assembly2022", 
       ylab= "Frequency")
abline(v = median(FemalePoliticians_2022[Partnership=="1"]),  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 8 ,
     paste("Median =\n", median(FemalePoliticians_2022[Partnership=="1"])),
     col = "seagreen",
     cex = 0.9)
##独立2群なt検定
t.test(FemalePoliticians_2022[Partnership=="0"],FemalePoliticians_2022[Partnership=="1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2022[Partnership=="0"],FemalePoliticians_2022[Partnership=="1"], 
       paired = FALSE,var.equal = TRUE) %>%
  tidy()


######surnameのヒストグラム#####
par(mfrow=c(1,2))
hist(FemalePoliticians_2015[Surnames2016=="0"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#ff000050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2015\n(Without separate surnames \npetition)",
     xlab="The proportion of female politicians \nin the assembly2015")
abline(v = median(FemalePoliticians_2015[Surnames2016=="0"]),  # Add line for median
       col = "red",
       lwd = 3)
text(x = 0.4,  # Add text for median
     y = 6,
     paste("Median =\n", median(FemalePoliticians_2015[Surnames2016=="0"])),
     col = "red",
     cex = 0.9)

hist(FemalePoliticians_2015[Surnames2016=="1"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#ff000050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2015\n(With separate surnames \npetition)",
     xlab="The proportion of female politicians \nin the assembly2015")
abline(v = median(FemalePoliticians_2015[Surnames2016=="1"]),  # Add line for median
       col = "red",
       lwd = 3)
median(FemalePoliticians_2015[Surnames2016=="1"])
text(x = 0.4,  # Add text for median
     y = 5.8,
     paste("Median =\n", median(FemalePoliticians_2015[Surnames2016=="1"])),
     col = "red",
     cex = 0.9)
##独立2群なt検定
t.test(FemalePoliticians_2015[Surnames2016=="0"],FemalePoliticians_2015[Surnames2016=="1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2015[Surnames2016=="0"],FemalePoliticians_2015[Surnames2016=="1"], 
       paired = FALSE,var.equal = TRUE)%>%
  tidy()


hist(FemalePoliticians_2022[Surnames=="0"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#00ff0050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2022\n(Without separate surnames \npetition)",
     xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median(FemalePoliticians_2022[Surnames=="0"]),  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 8,
     paste("Median =\n", median(FemalePoliticians_2022[Surnames=="0"])),
     col = "darkgreen",
     cex = 0.9)

hist(FemalePoliticians_2022[Surnames=="1"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#00ff0050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2022\n(With separate surnames \npetition)",
     xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median(FemalePoliticians_2022[Surnames=="1"]),  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 8,
     paste("Median =\n", median(FemalePoliticians_2022[Surnames=="1"])),
     col = "darkgreen",
     cex = 0.9)
##独立2群なt検定
t.test(FemalePoliticians_2022[Surnames=="0"],FemalePoliticians_2022[Surnames=="1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2022[Surnames=="0"],FemalePoliticians_2022[Surnames=="1"], 
       paired = FALSE,var.equal = TRUE) %>%
  tidy()

######unanimousのヒストグラム#####
par(mfrow=c(1,2))
median_value <- median(FemalePoliticians_2015[Surnames2016 == "1" & Surnames_unanimous2016 == "0"], na.rm = TRUE)
print(median_value)
hist(FemalePoliticians_2015[Surnames2016=="1"][Surnames_unanimous2016=="0"],xlim=c(0,0.5), ylim =c(0,6),
     freq=T, col="#ff000050", cex.main=1,
     main="The proportion of female politicians \n in the assembly2015\n(absolute majority)",
     xlab="The proportion of female politicians \n in the assembly2015")
abline(v = median_value,  # Add line for median
       col = "red",
       lwd = 3)
text(x = median_value * 1.6,  # Add text for median
     y = median_value * 10,
     paste("Median =\n", median_value),
     col = "red",
     cex = 0.9)

median_value0 <- median(FemalePoliticians_2015[Surnames2016 == "1" & Surnames_unanimous2016 == "1"], na.rm = TRUE)
print(median_value0)
hist(FemalePoliticians_2015[Surnames2016=="1" & Surnames_unanimous2016=="1"],
     breaks=seq(0,0.5,0.05), ylim =c(0,6),
     freq=T, col="#ff000050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2015\n(unanimous)",
     xlab="The proportion of female politicians \nin the assembly2015")
abline(v = median_value0,  # Add line for median
       col = "red",
       lwd = 3)
text(x = median_value0 * 1.4,  # Add text for median
     y = median_value0 * 7.3,
     paste("Median =\n", median_value0),
     col = "red",
     cex = 0.9)
##独立2群なt検定
t.test(FemalePoliticians_2015[Surnames2016=="1"][Surnames_unanimous2016=="0"],
       FemalePoliticians_2015[Surnames2016=="1" & Surnames_unanimous2016=="1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2022[Surnames=="0"],FemalePoliticians_2022[Surnames=="1"], 
       paired = FALSE,var.equal = TRUE) %>%
  tidy()


subset_data <- FemalePoliticians_2022[Surnames == "1" & Surnames_unanimous == "0"]
subset_data
median_value1 <- median(subset_data, na.rm = TRUE) # Calculate the median of the 'subset_data'
print(median_value1) # Print the median

hist(FemalePoliticians_2022[Surnames == "1" & Surnames_unanimous == "0"],xlim=c(0,0.5), ylim =c(0,6),
     freq=T, col="#00ff0050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2022\n(absolute majority)",
     xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median_value1,  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 5,
     paste("Median =\n", median_value1),
     col = "darkgreen",
     cex = 0.9)

subset_data2 <- FemalePoliticians_2022[Surnames == "1" & Surnames_unanimous == "1"]
subset_data2
median_value2 <- median(subset_data2, na.rm = TRUE) # Calculate the median of the 'subset_data'
print(median_value2) # Print the median

hist(FemalePoliticians_2022[Surnames == "1" & Surnames_unanimous == "1"],
     breaks=seq(0,0.5,0.05), ylim =c(0,6),
     freq=T, col="#00ff0050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2022\n(unanimous)",
     xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median_value2,  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 5,
     paste("Median =\n", median_value2),
     col = "darkgreen",
     cex = 0.9)
##独立2群なt検定
t.test(FemalePoliticians_2022[Surnames == "1" & Surnames_unanimous == "0"],
       FemalePoliticians_2022[Surnames == "1" & Surnames_unanimous == "1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2022[Surnames=="0"],FemalePoliticians_2022[Surnames=="1"], 
       paired = FALSE,var.equal = TRUE) %>%
  tidy()


######submissionの散布図#####
##2015
ggplot(warddata2, aes(x = FemalePoliticians_2015, 
                      y = Surnames_submission2016)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    x = "The proportion of female politicians in the assembly 2015",
    y = "Number of submissions of the petitions",
    title = "Number of female politicians and the petitions submitted (2015)",
    subtitle = "Petitions:Implementation of petitions of separate surnames for a married couple",
  ) +
  theme_minimal()

#ward名を追加
ggplot(warddata2, aes(x = FemalePoliticians_2015, 
                      y = Surnames_submission2016)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggrepel::geom_label_repel(aes(label = city),size = 2)+
  labs(
    x = "The proportion of female politicians in the assembly 2015",
    y = "Number of submissions of the petitions",
    title = "Number of female politicians and the petitions submitted (2015)",
    subtitle = "Petitions:Implementation of petitions of separate surnames for a married couple"
    ) +
  theme_minimal()

##2022
ggplot(warddata2, aes(x = FemalePoliticians_2022, 
                      y = Surnames_submission)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    x = "The proportion of female politicians in the assembly 2022",
    y = "Number of submissions of the petitions",
    title = "Number of female politicians and the petitions submitted (2022)",
    subtitle = "Petitions:Implementation of petitions of separate surnames for a married couple",
  ) +
  theme_minimal()

#ward名を追加
ggplot(warddata2, aes(x = FemalePoliticians_2022, 
                      y = Surnames_submission)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggrepel::geom_label_repel(aes(label = city),size = 2)+
  labs(
    x = "The proportion of female politicians in the assembly 2022",
    y = "Number of submissions of the petitions",
    title = "Number of female politicians and the petitions submitted (2022)",
    subtitle = "Petitions:Implementation of petitions of separate surnames for a married couple"
  ) +
  theme_minimal()


######2015と2022を重ねる：無理だわああ#####
ggplot(warddata2, aes(x = FemalePoliticians_2015, 
                      y = Surnames_submission2016)) +
  geom_point() + ylim(0,3)+
  labs(
    x = "The proportion of female politicians in the assembly 2015, 2022",
    y = "Number of submissions of the petitions",
    title = "Number of female politicians and the petitions submitted (2015, 2022)",
    subtitle = "Petitions:Implementation of petitions of separate surnames for a married couple"
  ) 

par(new=T)
ggplot(warddata2, aes(x = FemalePoliticians_2022, 
                      y = Surnames_submission)) +
  geom_point(colour = "green") +ylim(0,3)+
  labs(
    x = "The proportion of female politicians in the assembly 2015, 2022",
    y = "Number of submissions of the petitions",
    title = "Number of female politicians and the petitions submitted (2015, 2022)",
    subtitle = "Petitions:Implementation of petitions of separate surnames for a married couple"
  ) +

  
ggplot(warddata2, aes(x = FemalePoliticians_2015, y = Surnames_submission2016) +
geom_point() +
geom_point(aes(x = FemalePoliticians_2022, y = Surnames_submission), color = "green") +
labs(
  x = "The proportion of female politicians in the assembly 2015, 2022",
  y = "Number of submissions of the petitions",
  title = "Number of female politicians and the petitions submitted (2015, 2022)",
  subtitle = "Petitions: Implementation of petitions of separate surnames for a married couple"
)) 
theme_minimal()


######school lunchのヒストグラム#####
par(mfrow=c(1,2))

subset_data3 <- FemalePoliticians_2022[FreeSchoolLunch=="0"]
subset_data3
median_value3 <- median(subset_data3, na.rm = TRUE) # Calculate the median of the 'subset_data'
print(median_value3) # Print the median
hist(FemalePoliticians_2022[FreeSchoolLunch=="0"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#00ff0050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2022\n(Without Free School Lunch)",
     xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median_value3,  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 8,
     paste("Median =\n", median_value3),
     col = "darkgreen",
     cex = 0.9)

subset_data4 <- FemalePoliticians_2022[FreeSchoolLunch=="1"]
subset_data4
median_value4 <- median(subset_data4, na.rm = TRUE) # Calculate the median of the 'subset_data'
print(median_value4) # Print the median
hist(FemalePoliticians_2022[FreeSchoolLunch=="1"],xlim=c(0,0.5), ylim =c(0,10),
     freq=T, col="#00ff0050", cex.main=1,
     main="The proportion of female politicians \nin the assembly2022\n(With Free School Lunch)",
     xlab="The proportion of female politicians \nin the assembly2022")
abline(v = median_value4,  # Add line for median
       col = "seagreen",
       lwd = 3)
text(x = 0.42,  # Add text for median
     y = 8,
     paste("Median =\n", median_value4),
     col = "darkgreen",
     cex = 0.9)
##独立2群なt検定
t.test(FemalePoliticians_2022[FreeSchoolLunch=="0"],
       FemalePoliticians_2022[FreeSchoolLunch=="1"], 
       paired = FALSE,var.equal = TRUE)
t.test(FemalePoliticians_2022[Surnames=="0"],FemalePoliticians_2022[Surnames=="1"], 
       paired = FALSE,var.equal = TRUE) %>%
  tidy()

####各政策の歳出費　構成比####
######単回帰分析######
#お試し: https://onl.bz/VeNtDC6 4.3 単回帰＋単回帰＝重回帰？
Model2016_GeneralAdministration <- lm(GeneralAdministration_2016 ~ FemalePoliticians_2015, data = warddata2)
summary(Model2016_GeneralAdministration)
stargazer(Model2016_GeneralAdministration, type = "html") 
stargazer(Model2016_GeneralAdministration) 

#loop
rm(resFrame2)
resFrame1 <- data.frame()
resFrame2 <- data.frame()
wardL <- as.data.frame(warddata2)

#FemalePoliticians_2015
for(i in c(75:83)){
  result <- lm(wardL[,i]~FemalePoliticians_2015)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame1 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame2 <- rbind(resFrame2, resFrame1)
}
resFrame2
write.csv(resFrame2, "Ward_SimpleRegression_expenditure_FemaleRate2015~2020.csv")

#FemalePoliticians_2016
for(i in c(84:92)){
  result <- lm(wardL[,i]~FemalePoliticians_2016)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame1 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame2 <- rbind(resFrame2, resFrame1)
}
resFrame2
write.csv(resFrame2, "Ward_SimpleRegression_expenditure_FemaleRate2015~2020.csv")

#FemalePoliticians_2017
for(i in c(93:101)){
  result <- lm(wardL[,i]~FemalePoliticians_2017)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame1 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame2 <- rbind(resFrame2, resFrame1)
}
resFrame2
write.csv(resFrame2, "Ward_SimpleRegression_expenditure_FemaleRate2015~2020.csv")

#FemalePoliticians_2018
for(i in c(102:110)){
  result <- lm(wardL[,i]~FemalePoliticians_2018)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame1 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame2 <- rbind(resFrame2, resFrame1)
}
resFrame2
write.csv(resFrame2, "Ward_SimpleRegression_expenditure_FemaleRate2015~2020.csv")

#FemalePoliticians_2019
for(i in c(111:119)){
  result <- lm(wardL[,i]~FemalePoliticians_2019)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame1 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame2 <- rbind(resFrame2, resFrame1)
}
resFrame2
write.csv(resFrame2, "Ward_SimpleRegression_expenditure_FemaleRate2015~2020.csv")

#FemalePoliticians_2020
for(i in c(120:128)){
  result <- lm(wardL[,i]~FemalePoliticians_2020)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame1 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame2 <- rbind(resFrame2, resFrame1)
}
resFrame2
write.csv(resFrame2, "Ward_SimpleRegression_expenditure_FemaleRate2015~2020.csv")

#######重回帰分析#######
#お試し: https://onl.bz/VeNtDC6 4.3 単回帰＋単回帰＝重回帰？
Model <- lm(GeneralAdministration_2016 ~ FemalePoliticians_2015+FemaleMayor_2015+MayorsPartisanship_2015
            +Age_2015+AssemblyPartisanship_2015
            +FinancialCapability_2015+AgingPopulation_2015+PopulationDensity_2015, data = warddata2)
summary(Model)
stargazer(Model, type = "html") 
stargazer(Model) 

#参考：https://ides.hatenablog.com/entry/2017/06/25/161020
#2020年の土木費
lm0 <- lm(PublicWorks_2020 ~ FemalePoliticians_2019) 
lm1 <- update(lm0, ~. + FemaleMayor_2019) 
lm2 <- update(lm1, ~. + MayorsPartisanship_2019) 
lm3 <- update(lm2, ~. + Age_2019) 
lm4 <- update(lm3, ~. + AssemblyPartisanship_2019) 
lm5 <- update(lm4, ~. + FinancialCapability_2019) 
lm6 <- update(lm5, ~. + AgingPopulation_2019) 
lm7 <- update(lm6, ~. + PopulationDensity_2019) 
stargazer(lm0, lm1, lm2, lm3, lm4,lm5,lm6,lm7,
          title            = "Explaining the relation between the rate of female politicians and civil engineering:Multiple Regression",　# タイトルを入れる
          dep.var.labels   = "The expenditure of Civil engineering(budget composition 2020) "
          ,type = "html") #HTML形式

#2016年の教育費
lm10 <- lm(Education_2016 ~ FemalePoliticians_2015) 
lm11 <- update(lm10, ~. + FemaleMayor_2015) 
lm12 <- update(lm11, ~. + MayorsPartisanship_2015) 
lm13 <- update(lm12, ~. + Age_2015) 
lm14 <- update(lm13, ~. + AssemblyPartisanship_2015) 
lm15 <- update(lm14, ~. + FinancialCapability_2015) 
lm16 <- update(lm15, ~. + AgingPopulation_2015) 
lm17 <- update(lm16, ~. + PopulationDensity_2015) 
stargazer(lm10, lm11, lm12, lm13, lm14,lm15,lm16,lm17,
          title            = "Explaining the relation between the rate of female politicians and education:Multiple Regression",　# タイトルを入れる
          dep.var.labels   = "The expenditure of education(budget composition 2016) "
          ,type = "html") #HTML形式
stargazer(lm10, lm11, lm12, lm13, lm14,lm15,lm16,lm17,
          title            = "Explaining the relation between the rate of female politicians and education:Multiple Regression",　# タイトルを入れる
          dep.var.labels   = "The expenditure of education(budget composition 2016) ") 

#loop
#FemalePoliticians_2015
rm(resFrame4)
resFrame3 <- data.frame()
resFrame4 <- data.frame()
wardL <- as.data.frame(warddata2)
for(i in c(75:83)){
  result <- lm(wardL[,i]~FemalePoliticians_2015+FemaleMayor_2015+MayorsPartisanship_2015
               +Age_2015+AssemblyPartisanship_2015
               +FinancialCapability_2015+AgingPopulation_2015+PopulationDensity_2015 )
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame3 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame4 <- rbind(resFrame4, resFrame3)
}
resFrame4
write.csv(resFrame4, "Ward_MultipleRegression_expenditure_FemaleRate2015.csv")


#FemalePoliticians_2016
rm(resFrame6)
resFrame5 <- data.frame()
resFrame6 <- data.frame()
wardL <- as.data.frame(warddata2)
for(i in c(84:92)){
  result <- lm(wardL[,i]~FemalePoliticians_2016 +FemaleMayor_2016 +MayorsPartisanship_2016
               +Age_2016 +AssemblyPartisanship_2016
               +FinancialCapability_2016 +AgingPopulation_2016 +PopulationDensity_2016 )
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame5 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame6 <- rbind(resFrame6, resFrame5)
}
resFrame6
write.csv(resFrame6, "Ward_MultipleRegression_expenditure_FemaleRate2016.csv")

#FemalePoliticians_2017
resFrame7 <- data.frame()
resFrame8 <- data.frame()
wardL <- as.data.frame(warddata2)
for(i in c(93:101)){
  result <- lm(wardL[,i]~FemalePoliticians_2017 +FemaleMayor_2017 +MayorsPartisanship_2017
               +Age_2017 +AssemblyPartisanship_2017
               +FinancialCapability_2017 +AgingPopulation_2017 +PopulationDensity_2017 )
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame7 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame8 <- rbind(resFrame8, resFrame7)
}
resFrame8
write.csv(resFrame8, "Ward_MultipleRegression_expenditure_FemaleRate2017.csv")

#FemalePoliticians_2018
rm(resFrame10)
resFrame9 <- data.frame()
resFrame10 <- data.frame()
wardL <- as.data.frame(warddata2)
for(i in c(102:110)){
  result <- lm(wardL[,i]~FemalePoliticians_2018 +FemaleMayor_2018 +MayorsPartisanship_2018
               +Age_2018 +AssemblyPartisanship_2018
               +FinancialCapability_2018 +AgingPopulation_2018 +PopulationDensity_2018 )
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame9 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame10 <- rbind(resFrame10, resFrame9)
}
resFrame10
write.csv(resFrame10, "Ward_MultipleRegression_expenditure_FemaleRate2018.csv")

#FemalePoliticians_2019
rm(resFrame12)
resFrame11 <- data.frame()
resFrame12 <- data.frame()
wardL <- as.data.frame(warddata2)
for(i in c(111:119)){
  result <- lm(wardL[,i]~FemalePoliticians_2019 +FemaleMayor_2019 +MayorsPartisanship_2019
               +Age_2019 +AssemblyPartisanship_2019
               +FinancialCapability_2019 +AgingPopulation_2019 +PopulationDensity_2019 )
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame11 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame12 <- rbind(resFrame12, resFrame11)
}
resFrame12
write.csv(resFrame12, "Ward_MultipleRegression_expenditure_FemaleRate2019.csv")

#FemalePoliticians_2020
rm(resFrame14)
resFrame13 <- data.frame()
resFrame14 <- data.frame()
wardL <- as.data.frame(warddata2)
for(i in c(120:128)){
  result <- lm(wardL[,i]~FemalePoliticians_2020 +FemaleMayor_2020 +MayorsPartisanship_2020
               +Age_2020 +AssemblyPartisanship_2020
               +FinancialCapability_2020 +AgingPopulation_2020 +PopulationDensity_2020)
  
  estimate <- summary(result)$coefficients[,1]
  Std.Error <- summary(result)$coefficients[,2]
  tvalue <- summary(result)$coefficients[,3]
  pvalue <- summary(result)$coefficients[,4]
  name <- names(wardL)[i]
  
  resFrame13 <- data.frame(name, estimate, Std.Error, tvalue, pvalue)
  resFrame14 <- rbind(resFrame14, resFrame13)
}
resFrame14
write.csv(resFrame14, "Ward_MultipleRegression_expenditure_FemaleRate2020.csv")

######corrplot政策ごと######



######corrplot年度######
#2015
par(mfrow=c(1,1))
resFrame20 <- data.frame(FemalePoliticians_2015, warddata2[75:83])
resFrame20
cor2015 <- cor(resFrame20)
cor2015
#diag=FALSEでcor=1の当たり前部分を消す
corrplot(cor2015, method = 'circle', type = 'upper',addCoef.col ='black', 
         number.cex = 0.9, diag=FALSE) 

#2016
resFrame21 <- data.frame(FemalePoliticians_2016, warddata2[84:92])
resFrame21
cor2016 <- cor(resFrame21)
cor2016
#diag=FALSEでcor=1の当たり前部分を消す
corrplot(cor2016, method = 'circle', type = 'upper',addCoef.col ='black', 
         number.cex = 0.9, diag=FALSE) 

#2017
resFrame22 <- data.frame(FemalePoliticians_2017, warddata2[93:101])
resFrame22
cor2017 <- cor(resFrame22)
cor2017
#diag=FALSEでcor=1の当たり前部分を消す
corrplot(cor2017, method = 'circle', type = 'upper',addCoef.col ='black', 
         number.cex = 0.9, diag=FALSE) 

#2018
resFrame23 <- data.frame(FemalePoliticians_2018, warddata2[102:110])
resFrame23
cor2018 <- cor(resFrame23)
cor2018
#diag=FALSEでcor=1の当たり前部分を消す
corrplot(cor2018, method = 'circle', type = 'upper',addCoef.col ='black', 
         number.cex = 0.9, diag=FALSE) 

#2019
resFrame24 <- data.frame(FemalePoliticians_2019, warddata2[111:119])
resFrame24
cor2019 <- cor(resFrame24)
cor2019
#diag=FALSEでcor=1の当たり前部分を消す
corrplot(cor2019, method = 'circle', type = 'upper',addCoef.col ='black', 
         number.cex = 0.9, diag=FALSE) 

#2020
resFrame25 <- data.frame(FemalePoliticians_2020, warddata2[120:128])
resFrame25
cor2020 <- cor(resFrame25)
cor2020
#diag=FALSEでcor=1の当たり前部分を消す
corrplot(cor2020, method = 'circle', type = 'upper',addCoef.col ='black', 
         number.cex = 0.9, diag=FALSE) 

######各expenditureの散布図#####
#GeneralAdministration？？？？
ggplot(
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    x = "correlation coefficient",
    title = "Correlation between female politicians and expenditure of General Administration",
  ) +
  theme_minimal())

GA2015 <-cor(FemalePoliticians_2015, GeneralAdministration_2016)
GA2016 <-cor(FemalePoliticians_2016, GeneralAdministration_2017)
GA2017 <-cor(FemalePoliticians_2017, GeneralAdministration_2018)
GA2018 <-cor(FemalePoliticians_2018, GeneralAdministration_2019)
GA2019 <-cor(FemalePoliticians_2019, GeneralAdministration_2020)
GA2020 <-cor(FemalePoliticians_2020, GeneralAdministration_2021)
resFrame30 <- data.frame(GA2015, GA2016, GA2017, GA2018, GA2019, GA2020)
resFrame30


