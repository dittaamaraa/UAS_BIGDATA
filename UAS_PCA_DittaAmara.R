library(readxl)
View(dataset)
library(readxl)
UAS_PCA_dan_MLR <- read_excel("mod/UAS_PCA_dan_MLR.xlsx")
View(UAS_PCA_dan_MLR)
str(UAS_PCA_dan_MLR)

UAS_PCA_dan_MLR$Saving <- (UAS_PCA_dan_MLR$Saving - min(UAS_PCA_dan_MLR$Saving))/(max(UAS_PCA_dan_MLR$Saving) - min(UAS_PCA_dan_MLR$Saving))
UAS_PCA_dan_MLR$Deposit <- (UAS_PCA_dan_MLR$Deposit - min(UAS_PCA_dan_MLR$Deposit))/(max(UAS_PCA_dan_MLR$Deposit) - min(UAS_PCA_dan_MLR$Deposit))
UAS_PCA_dan_MLR$KK <- (UAS_PCA_dan_MLR$KK - min(UAS_PCA_dan_MLR$KK))/(max(UAS_PCA_dan_MLR$KK) - min(UAS_PCA_dan_MLR$KK))
UAS_PCA_dan_MLR$Tab_Bisnis <- (UAS_PCA_dan_MLR$Tab_Bisnis - min(UAS_PCA_dan_MLR$Tab_Bisnis))/(max(UAS_PCA_dan_MLR$Tab_Bisnis) - min(UAS_PCA_dan_MLR$Tab_Bisnis))
UAS_PCA_dan_MLR$Limit_Kredit_Mortgage <- (UAS_PCA_dan_MLR$Limit_Kredit_Mortgage - min(UAS_PCA_dan_MLR$Limit_Kredit_Mortgage))/(max(UAS_PCA_dan_MLR$Limit_Kredit_Mortgage) - min(UAS_PCA_dan_MLR$Limit_Kredit_Mortgage))

head(UAS_PCA_dan_MLR)
summary(UAS_PCA_dan_MLR)

myPr <- prcomp(UAS_PCA_dan_MLR[, 7:13])
prcomp(~Income + Tab_Bisnis, data = UAS_PCA_dan_MLR)
plot (UAS_PCA_dan_MLR$Income, UAS_PCA_dan_MLR$Tab_Bisnis)
plot (scale(UAS_PCA_dan_MLR$Income), scale(UAS_PCA_dan_MLR$Tab_Bisnis))
myPr
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0)
