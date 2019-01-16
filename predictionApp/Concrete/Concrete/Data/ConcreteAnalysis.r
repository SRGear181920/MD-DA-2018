library(caret)
concrete<-data.frame(read.csv2("Concrete_Data.csv"))
summary(concrete)
colnames(concrete) <- c("Cement",
                         "Blast.Furnace.Slag", 
                         "Fly.Ash","Water","Superplasticizer", 
                         "Coarse.Aggregate","Fine.Aggregate",
                         "Age", 
                         "Concrete.compressive.strength")



concrete$Concrete.compressive=concrete$Concrete.compressive.strength%/%10
concrete<-concrete[(concrete$Concrete.compressive<7) & (concrete$Concrete.compressive>0),]
concrete$Concrete.compressive <- 
  factor(c("A1", "A2", "A3", "A4", "A5", "A6")[concrete$Concrete.compressive])
concrete$Concrete.compressive.strength <- NULL


percentage <- prop.table(table(concrete$Concrete.compressive)) * 100
cbind(freq=table(concrete$Concrete.compressive), percentage=percentage)



A1<-concrete[concrete$Concrete.compressive =="A1",]
featurePlot(x=A1[,1:8], y = A1[,9], plot = "box")


A2<-concrete[concrete$Concrete.compressive =="A2",] 
featurePlot(x=A2[,1:8], y = A2[,9], plot = "box")


A3<-concrete[concrete$Concrete.compressive =="A3",] 
featurePlot(x=A3[,1:8], y = A3[,9], plot = "box")

A4<-concrete[concrete$Concrete.compressive =="A4",]
featurePlot(x=A4[,1:8], y = A4[,9], plot = "box")

A5<-concrete[concrete$Concrete.compressive =="A5",]
featurePlot(x=A5[,1:8], y = A5[,9], plot = "box")

A6<-concrete[concrete$Concrete.compressive =="A6",]
featurePlot(x=A6[,1:8], y = A6[,9], plot = "box")

concreteNew <- concrete[((concrete$Cement <400 &
                          (concrete$Fine.Aggregate <910 & concrete$Fine.Aggregate >650) &
                          (concrete$Coarse.Aggregate >825) &
                          (concrete$Water >125) & concrete$Blast.Furnace.Slag <250) & concrete$Concrete.compressive == "A1") |
                          (concrete$Concrete.compressive == "A2" & concrete$Age<30 & 
                             concrete$Fine.Aggregate<930 & concrete$Fine.Aggregate>600 &
                             concrete$Water >130 & concrete$Water <230) |
                          (concrete$Concrete.compressive == "A3" & concrete$Age<80 & concrete$Fine.Aggregate <910) |
                          (concrete$Concrete.compressive == "A4" & concrete$Age <200 & concrete$Fine.Aggregate <950) |
                          (concrete$Concrete.compressive == "A5" & concrete$Age < 150 & concrete$Fine.Aggregate < 950 & concrete$Water <220 ) |
                          (concrete$Concrete.compressive == "A6" & concrete$Age < 101)
                          ,]
                
featurePlot(x=concrete[,1:8], y = concrete[,9], plot = "box")
featurePlot(x=concreteNew[,1:8], y = concreteNew[,9], plot = "box")



hist(concrete$Cement)
hist(concrete$Blast.Furnace.Slag)
hist(concrete$Fly.Ash)
hist(concrete$Water)
hist(concrete$Superplasticizer)
hist(concrete$Coarse.Aggregate)
hist(concrete$Fine.Aggregate)
hist(concrete$Age)

#Выборка данных для проверки
validation_index <- createDataPartition(concreteNew$Concrete.compressive, p=0.80, list=FALSE)
validation <- concreteNew[-validation_index,]
concreteNew <- concreteNew[validation_index,]

#Анализ: будем запускать все алгоримы и проерять кроссвалидацией(cv) через 10 блоков
control <- trainControl(method="cv", number=10)
# Контролируема метрика
metric <- "Accuracy" 
#LDA
set.seed(13)
fit.lda <- train(Concrete.compressive~., data=concreteNew, method="lda", metric=metric, trControl=control)
#CART
set.seed(13)
fit.cart <- train(Concrete.compressive~., data=concreteNew, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(13)
fit.knn <- train(Concrete.compressive~., data=concreteNew, method="knn", metric=metric, trControl=control)
# SVM
set.seed(13)
fit.svm <- train(Concrete.compressive~., data=concreteNew, method="svmRadial", metric=metric, trControl=control)
#RandomForest
set.seed(13)
fit.rf <- train(Concrete.compressive~., data=concreteNew, method="rf", metric=metric, trControl=control)

#Получим оценки конролируемой метрики для каждого алгоритма
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
#визуализируем
dotplot(results)
#лучше всех rf 
print(fit.rf)
#Её и будем использовать
#для этого у нас подготовлен валидационный набор
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions, validation$Concrete.compressive)