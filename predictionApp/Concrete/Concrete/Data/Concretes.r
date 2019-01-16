#install.packages("caret")
library(caret)
#install.packages("e1071")
#install.packages("randomForest")
library(randomForest)
concrete<-data.frame(read.csv2("../../Data/Concrete_Data.csv"))
colnames(concrete) <- c("Cement",
                        "Blast.Furnace.Slag", 
                        "Fly.Ash","Water","Superplasticizer", 
                        "Coarse.Aggregate","Fine.Aggregate",
                         "Age", 
                         "Concrete.compressive.strength")
concrete<-concrete[-c(51, 46, 682, 556, 506, 483),]
#Назначаем категории и избавляемся от категорий, где данных мало
concrete$Concrete.compressive=concrete$Concrete.compressive.strength%/%10
concrete<-concrete[(concrete$Concrete.compressive<7) & (concrete$Concrete.compressive>0),]
concrete$Concrete.compressive <- 
  factor(c("A1", "A2", "A3", "A4", "A5", "A6")[concrete$Concrete.compressive])
concrete$Concrete.compressive.strength <- NULL

## Чистим данные исходя из графиков
concrete<- concrete[((concrete$Cement <400 &
                        (concrete$Fine.Aggregate <910 & concrete$Fine.Aggregate >650) &
                        (concrete$Coarse.Aggregate >825) &
                        (concrete$Water >125) & concrete$Blast.Furnace.Slag <250) & concrete$Concrete.compressive == "A1") |
                      (concrete$Concrete.compressive == "A2" & concrete$Age<30 & 
                         concrete$Fine.Aggregate<930 & concrete$Fine.Aggregate>600 &
                         concrete$Water >130 & concrete$Water <230) |
                      (concrete$Concrete.compressive == "A3" & concrete$Age<80 & concrete$Fine.Aggregate <910) |
                      (concrete$Concrete.compressive == "A4" & concrete$Age <200 & concrete$Fine.Aggregate <950) |
                      (concrete$Concrete.compressive == "A5" & concrete$Age < 150 & concrete$Fine.Aggregate < 950 & concrete$Water <220 ) |
                      (concrete$Concrete.compressive == "A6" & concrete$Age < 101),]


control <- trainControl(method="cv", number=10)
metric <- "Accuracy" 
# # #RandomForest
set.seed(13)
fit.rf<-train(y = concrete$Concrete.compressive, x = concrete[,1:8], method="rf", metric=metric, trControl=control)
