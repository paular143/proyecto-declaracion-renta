load('base_con_na')

#Entrenar con 2015-2016 y predecir en 2014
train = data
test = read.csv('base2014_sin_ingreso.csv')
test = test[which(test$p6040>=18),] #Quitar menores de edad

test_declarantes = test[which(test$renta==1),]
test_no_declarantes = test[which(test$renta==0),]
library(caTools)
set.seed(123)
split = sample.split(test_no_declarantes$renta, SplitRatio=0.05 )
test_no_declarantes = subset(test_no_declarantes,split==T)
test = rbind(test_declarantes,test_no_declarantes)


test = test[,names(test)%in%names(train)]
train = train[,names(train)%in%names(test)]
test[,sapply(train,is.factor)] = lapply(test[,sapply(train,is.factor)], as.factor)

factors = sapply(train, is.factor)
distintas = 1:258
for(i in 1:258){
    if(factors[i]==TRUE){
        distintas[i]=prod(as.numeric(levels(test[,i])%in%levels(train[,i])))}
    else{
        distintas[i]=1}}

quitar = which(distintas==0)
quitar = names(test)[quitar]
train = train[,!names(train)%in%quitar]
test = test[,!names(test)%in%quitar]

#==================================================================================================
#Sensibilidad, Especificidad y Exactitud
sens <- function(pred,x){
    pred = data.frame(as.numeric(as.character(pred)))
    sum(as.numeric((data.frame(as.numeric(as.character(x)))+pred)==2))/sum(data.frame(as.numeric(as.character(x))))
}

espec <- function(pred,x){
    pred = data.frame(as.numeric(as.character(pred)))
    sum(as.numeric((data.frame(as.numeric(as.character(x)))+pred)==0))/(length(test$renta)-sum(data.frame(as.numeric(as.character(x)))))
}

exact <- function(pred,x){
    pred = data.frame(as.numeric(as.character(pred)))
    sum(as.numeric(!(data.frame(as.numeric(as.character(x)))+pred)==1))/length(test$renta)
}

#==================================================================================================
#CART
library(rpart)
library(rpart.plot)
m_cart = rpart(renta ~ ., data = train, method = "class" )
rpart.plot(m_cart, type = 3 ,digits = 3, fallen.leaves = TRUE)



pred_cart = predict(m_cart,test[,-dim(test)[2]],type="class")
sens_cart = sens(pred_cart,test$renta)
espec_cart = espec(pred_cart,test$renta)
exact_cart = exact(pred_cart,test$renta)


library(pROC)
library(ROCR)
cart.prob = predict(m_cart,test[,-dim(test)[2]],type="prob")
pred = prediction(cart.prob[, 2], test$renta)
plot(performance(pred, "tpr", "fpr"),col="blue")
abline(0, 1, lty = 2)
auc.cart = auc(test$renta,cart.prob[,2])


#==================================================================================================
#Boosting de Arboles

test = lapply(test, function(x) as.numeric(as.character(x)))
train = lapply(train, function(x) as.numeric(as.character(x)))

library(gbm)
trees=2000
shrinkage=0.1


boost.data=gbm(renta~., data=train, n.trees=trees, shrinkage = shrinkage, distribution = 'bernoulli')
boost.prob=predict.gbm(boost.data,test[,-247],n.trees = 2000,type='response')

library(ROCR)
library(pROC)
pred = prediction(boost.prob, test$renta)
plot(performance(pred, "tpr", "fpr"),col='magenta')
abline(0, 1, lty = 2)
auc.boost = auc(test$renta,boost.prob)

boost.pred=ifelse(boost.prob>0.1,1,0)
sens_boost = sens(boost.pred,test_declarantes$renta)
espec_boost = espec(boost.pred,test$renta)
exact_boost = exact(boost.pred,test$renta)

#==================================================================================================
#Random Forest
library(missForest)
mforest = randomForest(renta ~ ., data = train, ntree=1000, nodesize = 5)


forest.prob = predict(mforest,test[,-131],type="response")
pred = prediction(forest.prob, test$renta)
plot(performance(pred, "tpr", "fpr"),col=90)


forest.pred = ifelse(forest.prob>0.1,1,0)
sens_forest = sens(forest.pred,test$renta)
espec_forest = espec(forest.pred,test$renta)
exact_forest = exact(forest.pred,test$renta)
#==================================================================================================
#Naive Bayes
library(naivebayes)
mnaive = naive_bayes(renta ~ ., data = train)

prob.naive = predict(mnaive,test[,-dim(test)[2]],type="prob")
pred = prediction(prob.naive[,2], test$renta)
plot(performance(pred, "tpr", "fpr"),col='chartreuse3',main="ROC sin SMOTE")
auc.naive = auc(test$renta,prob.naive[,2])
cost.perf = performance(pred, "tpr", "fpr")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


legend('bottomright',legend=c("CART (AUC=0.8736)","Gradient Boosting (AUC=0.8608)","Naive Bayes (AUC=0.9442)"),col=c("blue","magenta","chartreuse3"),lty=1,cex=0.8)

opt.cut=function(perf,pred){
    cut.ind = mapply(FUN=function(x,y,p){
        d=(x-0)^2 + (y-1)^2
        ind = which(d==min(d))
        c(sensitivity = y[[ind]],specificity = 1-x[[ind]],cutoff=p[[ind]])
    },perf@x.values,perf@y.values,pred@cutoffs)
}

roc.perf = performance(pred,measure=&quot;tpr&quot;, x.measure = &quot;fpr&quot;)
