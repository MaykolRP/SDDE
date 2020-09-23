#Censo Nacional Agropecuario
#Suelos Bogotá
CNA2014_S6CUL_2013_11 <- read.csv("~/Documents/SecDes/Economía Rural/11Bogota/CNA2014_S6CUL_2013_11.csv", header=FALSE)
CNA_cultivos_Bogota<-CNA2014_S6CUL_2013_11
CNA_cultivos_Bogota<-CNA_cultivos_Bogota[,c(3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
colnames(CNA_cultivos_Bogota)<-c("Departamento","Municipio","Encuesta","Cod Vereda","P_S6P45B",
                                 "P_S6P45A","P_S6P46","P_S6P47A","P_S6P47B","P_S6P48",
                                 "P_S6P50","P_S6P51_SP1","P_S6P51_SP2","P_S6P51_SP3",
                                 "P_S6P53","P_S6P57A","P_S6P59_UNIF","P_S6P60",
                                 "AREA_SEMBRADA","AREA_COSECHADA")
CNA_cultivos_Bogota<-CNA_cultivos_Bogota[2:9841,]

#Cual cultivo o plantacion forestal tiene en el lote
cultivo<-as.data.frame(table(CNA_cultivos_Bogota["P_S6P46"]))
#Veredas en la data
codigoverda<-as.data.frame(table(CNA_cultivos_Bogota["Cod Vereda"]))

k<-codigoverda$Var1==11001001
which(Veredas$COD_VEREDA==11001001)
Veredas[which(Veredas$COD_VEREDA==11001040),]
which(11001001)

#Convertir factor en numerico
cv<-as.numeric(as.character(codigoverda$Var1))
cv[1]


for(i in 1:49){
  print(Veredas[which(Veredas$COD_VEREDA==cv[i]),])[1]
}



nombre_vereda<-c()
for(i in 1:49){
  nombre_vereda[i]=Veredas[which(Veredas$COD_VEREDA==cv[i]),][1]
}
nombre_vereda[[3]][1]
nv<-c()
for(j in 1:49){
  nv=c(nv,nombre_vereda[[j]][1])
}


Veredas[which(Veredas$COD_VEREDA==cv[2]),][2]

data<-data.frame(nombre_vereda)


vereda_por_codigo<-data.frame(Codigo=codigoverda$Var1,Vereda=nv,Frecuencia=codigoverda$Freq)
write_xlsx(vereda_por_codigo,"/home/tata/vereda_por_codigo.xlsx")

which(codigo_cultivo$cod_cultivo==00112201001)
codigo_cultivo[which(codigo_cultivo$cod_cultivo==00112201001),]

for(i in 1:92){
  print(codigo_cultivo[which(codigo_cultivo$cod_cultivo==cc[i]),])[2]
}

cultivo_nombre<-list()
for(i in 1:92){
  cultivo_nombre[i]=codigo_cultivo[which(codigo_cultivo$cod_cultivo==cc[i]),][2]
}

cc<-as.numeric(as.character(cultivo$Var1))

cultivo_nombre[[1]][1]
cn<-c()
for(i in 1:92){
  cn=c(cn,cultivo_nombre[[i]][1])
}
cn

cultivo_por_codigo<-data.frame(codigo_cultivo=cultivo$Var1,nombre_cultivo =cn,frecuencia=cultivo$Freq)
write_xlsx(cultivo_por_codigo,"/home/tata/cultivo_por_codigo.xlsx")

CNA_cultivos_Bogota<-CNA_cultivos_Bogota[,c("Departamento","Encuesta","Cod Vereda","P_S6P46",
                                            "AREA_SEMBRADA","AREA_COSECHADA")]
sum(as.numeric(CNA_cultivos_Bogota[CNA_cultivos_Bogota$P_S6P46=='00121901001',]["AREA_SEMBRADA"]))

L=length(CNA_cultivos_Bogota[CNA_cultivos_Bogota$P_S6P46=='00117201001',]["AREA_SEMBRADA"][,1])
sum(as.numeric(CNA_cultivos_Bogota[CNA_cultivos_Bogota$P_S6P46=='00117201001',]["AREA_SEMBRADA"][1:L,]))

codigoc<-c()
areac<-c()
Area_cultivo<-data.frame()
for(i in cultivo_por_codigo$codigo_cultivo){
  codigoc<-c(codigoc,i)
  L=length(CNA_cultivos_Bogota[CNA_cultivos_Bogota$P_S6P46==i,]["AREA_SEMBRADA"][,1])
  ar=sum(as.numeric(CNA_cultivos_Bogota[CNA_cultivos_Bogota$P_S6P46==i,]["AREA_SEMBRADA"][1:L,]))
  areac<-c(areac,ar)
}

Area_cultivo<-data.frame(codigo=codigoc,Area=areac)
Area_cultivo[duplicated(Area_cultivo$codigo),]

cultivo_por_codigo_area<-data.frame(cultivo_por_codigo,Area_cultivo$Area)
colnames(cultivo_por_codigo_area)<-c("Codigo Cultivo","Cultivo","Frecuencia","Area")
write_xlsx(cultivo_por_codigo_area,"/home/tata/Cultivo_por_codigo_area.xlsx")


CNA_cultivos_Bogota$'VeredaN'=0
for(i in 1:length(vereda_por_codigo$Codigo)){
  for(j in 1:length(CNA_cultivos_Bogota$`Cod Vereda`)){
    if(vereda_por_codigo$Codigo[i]==CNA_cultivos_Bogota$`Cod Vereda`[j]){
      CNA_cultivos_Bogota$VeredaN[j]=vereda_por_codigo$Vereda[i]
    }
  }
}

Vereda_cultivo<-data.frame(Vereda=CNA_cultivos_Bogota$VeredaN,
                           Código_Cultivo=CNA_cultivos_Bogota$P_S6P46,
                           Area_Sembrada=CNA_cultivos_Bogota$AREA_SEMBRADA)

Vereda_cultivo<-Vereda_cultivo[order(Vereda_cultivo$Vereda),]
Vereda_cultivo$'CultivoN'<-0
for(i in 1:length(cultivo_por_codigo$codigo_cultivo)){
  for(j in 1:length(Vereda_cultivo$Código_Cultivo)){
    if(cultivo_por_codigo$codigo_cultivo[i]==Vereda_cultivo$Código_Cultivo[j]){
      Vereda_cultivo$CultivoN[j]=cultivo_por_codigo$nombre_cultivo[i]
    }
  }
}

Vereda_Cultivo<-data.frame(Vereda=Vereda_cultivo$Vereda,Cultivo=Vereda_cultivo$CultivoN,
                           Area_Sembrada=Vereda_cultivo$Area_Sembrada)
write_xlsx(Vereda_Cultivo,"/home/tata/Vereda_Cultivo.xlsx")

cla=c()
for(i in Cultivo_por_codigo_area$Codigo.Cultivo){
  k=Cultivo_clasificacion[which(Cultivo_clasificacion$Codigo==i),][2]
  cla=c(cla,k)
}
cla

Cultivo_Area_Clasificacion<-data.frame(Cultivo_por_codigo_area$Codigo.Cultivo,
                                       Cultivo_por_codigo_area$Cultivo,clas,
                                       Cultivo_por_codigo_area$Frecuencia,
                                       Cultivo_por_codigo_area$Area)

for(i in 1:92){
  print(cla[i]$Clasificacion)
}

cla[92]$Clasificacion

clas<-c()
for(i in 1:92){
  clas<-c(clas,cla[i]$Clasificacion)
}

