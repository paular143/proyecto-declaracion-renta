load('base2015_con_na')

#imbalanceo de clases
library(DMwR)
proporcion=0.4
over=((proporcion*(length(data$renta)-sum(as.numeric(as.character(data$renta)))))/sum(as.numeric(as.character(data$renta))))
under=((1-proporcion)*(length(data$renta)-sum(as.numeric(as.character(data$renta)))))/(sum(as.numeric(as.character(data$renta)))*over)
over=round(over,1)*100
under=round(under,1)*100

smoteData = SMOTE(renta ~ ., data = data, perc.over = over,perc.under=under)

datosfaltantes = colSums(is.na(smoteData))/50468
smoteData = smoteData[,-which(datosfaltantes==1)]

save(smoteData, file='smoteData')
