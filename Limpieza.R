base2015 = read.csv("base2015_sin_ingreso.csv")
base2016 = read.csv("base2016_sin_ingreso.csv")


#Juntar bases de datos
base2015 = base2015[,names(base2015)%in%names(base2016)]
data = rbind(base2015,base2016)
data[data=="."]='NA'

data = data[,5:324] #quitar variables no predictivas 
data$cont = NULL
data$percapita = NULL

#Quitar menores de edad
data = data[which(data$p6040>=18),] #Quitar menores de edad

#Quitar variables que TODAS las observaciones sean NA
datosfaltantes = colSums(is.na(data))/52042
data = data[,-which(datosfaltantes==1)]



#Arreglar tipo datos
maximos = lapply(data, function(x) summary(x)[6])
maximos = data.frame(matrix(unlist(maximos)))
var_cont = names(data)[which(maximos>50)]
data[,!names(data)%in%var_cont] = lapply(data[,!names(data)%in%var_cont], as.factor)
data$p1_departamento = as.factor(data$p1_departamento)

save(data, file = "base2015_con_na")

#================================================================================================
#Analisis de datos faltantes
data = data[,-which(datosfaltantes>=0.5)]

#Imputacion variables continuas
#data$p5130[is.na(data$p5130)] = mean(data$p5130[!is.na(data$p5130)])

#Funcion Moda
getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Imputar NAs categoricos con Moda y continuos con 0 
for (var in names(data)) {
    if(is.factor(data[,var])){
        data[is.na(data[,var]),var] = getmode(data[!is.na(data[,var]),var])
    }
    else{
        data[is.na(data[,var]),var] = 0
    }
}



save(data, file = "base_sin_na")
