library(caret)
concrete<-data.frame(read.csv2("Concrete_Data.csv"))
summary(concrete)
colnames(concrete) <- c("Cement",
                         "Blast.Furnace.Slag", 
                         "Fly.Ash","Water","Superplasticizer", 
                         "Coarse.Aggregate","Fine.Aggregate",
                         "Age", 
                         "Concrete.compressive.strength")

m<-mean(concrete$Concrete.compressive.strength)
concrete$Concrete.compressive<-ifelse(concrete$Concrete.compressive.strength > m, 1, 2)

concrete$Concrete.compressive <- 
  factor(c("GOOD", "BAD")[concrete$Concrete.compressive])
concrete$Concrete.compressive.strength <- NULL
good<-concrete[concrete$Concrete.compressive == "GOOD",]
bad<-concrete[concrete$Concrete.compressive == "BAD",]

featurePlot(x=bad[,1:8], y = bad[,9], plot = "box")
bad<-bad[bad$Fine.Aggregate <910 & bad$Fine.Aggregate>650 &
           bad$Age<50,]
featurePlot(x=good[,1:8], y = good[,9], plot = "box")
good<-good[good$Fine.Aggregate<950 & good$Age <200 & 
             good$Blast.Furnace.Slag<310,]

concreteNew <- rbind(bad,good) 
featurePlot(x=concrete[,1:8], y = concrete[,9], plot = "box")
featurePlot(x=concreteNew[,1:8], y = concreteNew[,9], plot = "box")

percentage <- prop.table(table(concreteNew$Concrete.compressive)) * 100
cbind(freq=table(concreteNew$Concrete.compressive), percentage=percentage)
plot(concreteNew$Concrete.compressive)

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