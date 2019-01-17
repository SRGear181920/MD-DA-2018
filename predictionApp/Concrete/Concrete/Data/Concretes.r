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
#Назначаем категории и избавляемся от категорий, где данных мало
m<-mean(concrete$Concrete.compressive.strength)
concrete$Concrete.compressive<-ifelse(concrete$Concrete.compressive.strength > m, 1, 2)

concrete$Concrete.compressive <- 
  factor(c("GOOD", "BAD")[concrete$Concrete.compressive])
concrete$Concrete.compressive.strength <- NULL
concrete[-c(1,46, 51, 84, 274, 491 ),]
good<-concrete[concrete$Concrete.compressive == "GOOD",]
bad<-concrete[concrete$Concrete.compressive == "BAD",]

## Чистим данные исходя из графиков
bad<-bad[bad$Fine.Aggregate <910 & bad$Fine.Aggregate>650 &
           bad$Age<50,]
good<-good[good$Fine.Aggregate<950 & good$Age <200 & 
             good$Blast.Furnace.Slag<310,]

concreteNew <- rbind(bad,good) 


control <- trainControl(method="cv", number=10)
metric <- "Accuracy" 
# # #RandomForest
set.seed(13)
fit.rf<-train(y = concrete$Concrete.compressive, x = concrete[,1:8], method="rf", metric=metric, trControl=control)
