
path15='./ECV2015'
path16='./ECV2016'
path=path15
txt_files = list.files(pattern = '*.txt',path =path)
txt_files=txt_files[-c(3,11)]
data_list = lapply(paste(path,'/',txt_files,sep = ''), read.table, sep = "\t", header = TRUE)

base2015=data.frame(data_list[2])
base2015$SECUENCIA_ENCUESTA=NULL
base2015$SECUENCIA_P=NULL
base2015$NRO_ENCUESTA=NULL
base2015$FEX_C=NULL
base2015$LLAVE=paste(base2015$LLAVEHOG,base2015$ORDEN,sep='-')
base2015$ORDEN=NULL


for(i in 1:length(data_list)){
  if(i!=5 && i!=2){
    print(i)
    temp=data.frame(data_list[i])
    temp$SECUENCIA_ENCUESTA=NULL
    temp$SECUENCIA_P=NULL
    temp$NRO_ENCUESTA=NULL
    temp$FEX_C=NULL
    temp$LLAVE=paste(temp$LLAVEHOG,temp$ORDEN,sep='-')
    temp$ORDEN=NULL
    base2015=merge(base2015,temp,by=c('LLAVEHOG','DIRECTORIO'))
  }
}
BASE=base2015
base2016=merge(base2016,data.frame(data_list[5]), by='DIRECTORIO')


# CREAR LOS IDS UNICOS
base_uniq=data.frame(data_list[2])
base_uniq$LLAVE=paste(paste(base_uniq$DIRECTORIO,base_uniq$SECUENCIA_P,sep=''),base_uniq$ORDEN,sep='-')
base_uniq=base_uniq[,-(1:29),drop=FALSE]

for(i in 1:length(data_list)){
    if(i!=5){
        print(i)
        temp=data.frame(data_list[i])
        temp$SECUENCIA_ENCUESTA=NULL
        temp$NRO_ENCUESTA=NULL
        temp$FEX_C=NULL
        temp$LLAVE=paste(paste(temp$DIRECTORIO,temp$SECUENCIA_P,sep=''),temp$ORDEN,sep='-')
        temp$ORDEN=NULL
        temp$DIRECTORIO=NULL
        temp$SECUENCIA_P=NULL


        base_uniq=merge(base_uniq,temp,by='LLAVE')
        print(dim(base_uniq))
    }
}

prueba=data.frame(data_list[5])
print(txt_files)
