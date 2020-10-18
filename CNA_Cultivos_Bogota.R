#Censo Nacional Agropecuario
#Suelos Bogotá
install.packages('writexl')
library("writexl")



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

colnames(Cultivo_Area_Clasificacion)<-c('Codigo','Cultivo','Clasificacion','Frecuencia','Area')

for(i in 1:92){
  print(cla[i]$Clasificacion)
}

cla[92]$Clasificacion

clas<-c()
for(i in 1:92){
  clas<-c(clas,cla[i]$Clasificacion)
}

write_xlsx(Cultivo_Area_Clasificacion,"/home/tata/Cultivo_Area_Clasificacion.xlsx")




#Base para hallar datos pecuarios y no agropecuarios
CNA2014_pecuario_noagropecuario <- read.csv("~/Documents/SDDE/Economía Rural/11Bogota/CNA2014_ENCABEZADO_11.csv",
                                            header=FALSE)

pecuario<-CNA2014_pecuario_noagropecuario[2:4913,]
colnames(pecuario)<-CNA2014_pecuario_noagropecuario[1,]
nombre_vereda

pecuario$'VeredaN'=0
for(i in 1:4912){
  for(j in 1:length(Veredas$COD_VEREDA)){
    if(pecuario$COD_VEREDA[i]==Veredas$COD_VEREDA[j]){
      pecuario$VeredaN[i]=Veredas$Nombre.de.la.vereda[j]
    }
  }
}



#Agregar la localidad a la vereda
pecuario$Localidad<-0
for(i in 1:4912){
  for(j in 1:length(VCultivo$Localidad)){
    if(pecuario$VeredaN[i]==VCultivo$Vereda[j]){
      pecuario$Localidad[i]=VCultivo$Localidad[j]
    }
  }
}

localidad_a_mano<-function(a,b){
  for(i in 1:length(pecuario$VeredaN)){
    if(pecuario$VeredaN[i]==a){
      pecuario$Localidad[i]=b
    }
  }
}

localidad_a_mano('CHORRERAS','Sumapaz')

for(i in length(pecuario$VeredaN)){
  if(pecuario$VeredaN[i]=="CHORRERAS"){
    pecuario$Localidad[i]="Sumapaz"
  }
}
pecuario[which(pecuario$VeredaN=='CHORRERAS'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='CONCEPCION'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='NUEVA GRANADA'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='HOYA SAN CRISTOBAL'),][166]='San Cristobal'
pecuario[which(pecuario$VeredaN=='INGEMAR ORIENTAL'),][166]='Chapinero'
pecuario[which(pecuario$VeredaN=='LASANIMAS'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='CURUBITAL'),][166]='Usme'
pecuario[which(pecuario$VeredaN=='LA UNION'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='LAS PALMAS'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='NAZARETH'),][166]='Sumapaz'
pecuario[which(pecuario$VeredaN=='AURORA ALTA'),][166]='Usaquen'
factor(pecuario[which(pecuario$Localidad==0),][165])

write_xlsx(pecuario,"/home/tata/pecuario_localidad.xlsx")

#pecuario<-data.frame(pecuario$ENCUESTA,pecuario$COD_VEREDA,pecuario$P_S7P78,pecuario$P_S7P79_SP1,
#                pecuario$P_S7P79_SP2,pecuario$P_S7P79_SP3,
#                pecuario$P_S7P79_SP4,pecuario$P_S7P79_SP5,pecuario$P_S7P79_SP6,
#                pecuario$P_S7P82,pecuario$P_S7P83A,pecuario$P_S7P83B,pecuario$P_S7P83C,
#                pecuario$P_S7P83D,pecuario$P_S7P83E,pecuario$P_S7P83F,pecuario$P_S7P84A,
#                pecuario$P_S7P84A,pecuario$P_S7P84B,pecuario$P_S7P84C,pecuario$P_S7P84D,
#                pecuario$P_S7P84E,pecuario$P_S7P84F,pecuario$P_S7P85B,pecuario$P_S7P86,
#                pecuario$P_S7P87_SP1,pecuario$P_S7P87_SP2,pecuario$P_S7P87_SP3,pecuario$P_S7P87_SP4,
#                pecuario$P_S7P87_SP5,pecuario$P_S7P89A,pecuario$P_S7P89B,pecuario$P_S7P89C,pecuario$P_S7P89D,
#                pecuario$P_S7P89E,pecuario$P_S7P89F,pecuario$P_S7P90,pecuario$P_S7P91_SP1,pecuario$P_S7P91_SP2,
#                pecuario$P_S7P91_SP3,pecuario$P_S7P91_SP4,pecuario$P_S7P91_SP5,pecuario$P_S7P92A,pecuario$P_S7P92B,
#                pecuario$P_S7P93A,pecuario$P_S7P93B,pecuario$P_S7P101,pecuario$P_S7P102C,pecuario$P_S7P102D,
#                pecuario$P_S7P102E,pecuario$P_S7P102F,pecuario$P_S7P102G,pecuario$P_S7P102H,pecuario$P_S7P102I,
#                pecuario$P_S7P102J,pecuario$P_S7P102K,pecuario$P_S7P102L,pecuario$P_S7P105)


#Que localidades hay

pecuario_localidad<-pecuario_localidad[2:4913,]
colnames(pecuario_localidad)<-pecuario_localidad[1,]
factor(pecuario_localidad$Localidad)

for(i in 1:length(pecuario_localidad$Localidad)){
  if(pecuario_localidad$Localidad[i]==0 | pecuario_localidad$Localidad[i]==""){
    pecuario_localidad$Localidad[i]='Otras localidades'
  }
}
pecuario_localidad$Localidad[1]
write_xlsx(pecuario_localidad,"/home/tata/Pecuario_Localidad.xlsx")

sumapaz_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Sumapaz'),])
chapinero_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Chapinero'),])
ciudadBolivar_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Ciudad Bolivar'),])
SanCristobal_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='San Cristobal'),])
SantaFe_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Santa Fe'),])
Usaquen_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Usaquen'),])
Usme_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Usme'),])
Suba_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Suba'),])
Otras_pecuario<-data.frame(pecuario_localidad[which(pecuario_localidad$Localidad=='Otras localidades'),])

#P_S7P78 Ha tenido ganado bovino durante los últimos 12 meses:
table(sumapaz_pecuario$P_S7P78)
table(chapinero_pecuario$P_S7P78)
table(ciudadBolivar_pecuario$P_S7P78)
table(SanCristobal_pecuario$P_S7P78)
table(SantaFe_pecuario$P_S7P78)
table(Usaquen_pecuario$P_S7P78)
table(Usme_pecuario$P_S7P78)
table(Suba_pecuario$P_S7P78)
table(Otras_pecuario$P_S7P78)

#La orientación de la actividad ganadera ha sido:Doble propósito
table(sumapaz_pecuario$P_S7P79_SP1)
table(chapinero_pecuario$P_S7P79_SP1)
table(ciudadBolivar_pecuario$P_S7P79_SP1)
table(SanCristobal_pecuario$P_S7P79_SP1)
table(SantaFe_pecuario$P_S7P79_SP1)
table(Usaquen_pecuario$P_S7P79_SP1)
table(Usme_pecuario$P_S7P79_SP1)
table(Suba_pecuario$P_S7P79_SP1)
table(Otras_pecuario$P_S7P79_SP1)

#La orientación de la actividad ganadera ha sido:Leche
table(sumapaz_pecuario$P_S7P79_SP2)
table(chapinero_pecuario$P_S7P79_SP2)
table(ciudadBolivar_pecuario$P_S7P79_SP2)
table(SanCristobal_pecuario$P_S7P79_SP2)
table(SantaFe_pecuario$P_S7P79_SP2)
table(Usaquen_pecuario$P_S7P79_SP2)
table(Usme_pecuario$P_S7P79_SP2)
table(Suba_pecuario$P_S7P79_SP2)
table(Otras_pecuario$P_S7P79_SP2)

#	La orientación de la actividad ganadera ha sido:Carne: ciclo completo
table(sumapaz_pecuario$P_S7P79_SP3)
table(chapinero_pecuario$P_S7P79_SP3)
table(ciudadBolivar_pecuario$P_S7P79_SP3)
table(SanCristobal_pecuario$P_S7P79_SP3)
table(SantaFe_pecuario$P_S7P79_SP3)
table(Usaquen_pecuario$P_S7P79_SP3)
table(Usme_pecuario$P_S7P79_SP3)
table(Suba_pecuario$P_S7P79_SP3)
table(Otras_pecuario$P_S7P79_SP3)

#La orientación de la actividad ganadera ha sido:Carne: Carne: cría; levante
table(sumapaz_pecuario$P_S7P79_SP4)
table(chapinero_pecuario$P_S7P79_SP4)
table(ciudadBolivar_pecuario$P_S7P79_SP4)
table(SanCristobal_pecuario$P_S7P79_SP4)
table(SantaFe_pecuario$P_S7P79_SP4)
table(Usaquen_pecuario$P_S7P79_SP4)
table(Usme_pecuario$P_S7P79_SP4)
table(Suba_pecuario$P_S7P79_SP4)
table(Otras_pecuario$P_S7P79_SP4)

#	La orientación de la actividad ganadera ha sido:Carne: Carne: Carne: ceba
table(sumapaz_pecuario$P_S7P79_SP5)
table(chapinero_pecuario$P_S7P79_SP5)
table(ciudadBolivar_pecuario$P_S7P79_SP5)
table(SanCristobal_pecuario$P_S7P79_SP5)
table(SantaFe_pecuario$P_S7P79_SP5)
table(Usaquen_pecuario$P_S7P79_SP5)
table(Usme_pecuario$P_S7P79_SP5)
table(Suba_pecuario$P_S7P79_SP5)
table(Otras_pecuario$P_S7P79_SP5)

#La orientación de la actividad ganadera ha sido:Carne: Carne: Genética
table(sumapaz_pecuario$P_S7P79_SP6)
table(chapinero_pecuario$P_S7P79_SP6)
table(ciudadBolivar_pecuario$P_S7P79_SP6)
table(SanCristobal_pecuario$P_S7P79_SP6)
table(SantaFe_pecuario$P_S7P79_SP6)
table(Usaquen_pecuario$P_S7P79_SP6)
table(Usme_pecuario$P_S7P79_SP6)
table(Suba_pecuario$P_S7P79_SP6)
table(Otras_pecuario$P_S7P79_SP6)

#Tiene ganado Bovino hoy
table(sumapaz_pecuario$P_S7P82)
table(chapinero_pecuario$P_S7P82)
table(ciudadBolivar_pecuario$P_S7P82)
table(SanCristobal_pecuario$P_S7P82)
table(SantaFe_pecuario$P_S7P82)
table(Usaquen_pecuario$P_S7P82)
table(Usme_pecuario$P_S7P82)
table(Suba_pecuario$P_S7P82)
table(Otras_pecuario$P_S7P82)

#Cuántos machos de ganado bovino tiene en total?

count<-function(x){
  y<-as.numeric(x)
  for(i in 1:length(y)){
    if(is.na(y[i])){
      y[i]=0
    }
  }
  return(sum(y))
}

count(sumapaz_pecuario$P_S7P83A)
count(chapinero_pecuario$P_S7P83A)
count(ciudadBolivar_pecuario$P_S7P83A)
count(SanCristobal_pecuario$P_S7P83A)
count(SantaFe_pecuario$P_S7P83A)
count(Usaquen_pecuario$P_S7P83A)
count(Usme_pecuario$P_S7P83A)
count(Suba_pecuario$P_S7P83A)
count(Otras_pecuario$P_S7P83A)



#¿Cuántos machos de ganado bovino tiene menores de 1 año?
count(pecuario$pecuario.P_S7P83B)
#2353 menores de un año

#¿Cuántos machos de ganado bovino tiene de 1 año y menores de 2 años?
count(pecuario$pecuario.P_S7P83C)
#2002

#	¿Cuántos machos de ganado bovino tiene de 2 años y menores de 3 años?
count(pecuario$pecuario.P_S7P83D)
#1621

#	¿Cuántos machos de ganado bovino tiene de 3 años y más?
count(pecuario$pecuario.P_S7P83E)
#454

#	¿Cuántos machos de ganado bovino tiene reproductores?
count(sumapaz_pecuario$P_S7P83F)
count(chapinero_pecuario$P_S7P83F)
count(ciudadBolivar_pecuario$P_S7P83F)
count(SanCristobal_pecuario$P_S7P83F)
count(SantaFe_pecuario$P_S7P83F)
count(Usaquen_pecuario$P_S7P83F)
count(Usme_pecuario$P_S7P83F)
count(Suba_pecuario$P_S7P83F)
count(Otras_pecuario$P_S7P83F)

#	Cuántas hembras de ganado bovino tiene en total?
count(sumapaz_pecuario$P_S7P84A)
count(chapinero_pecuario$P_S7P84A)
count(ciudadBolivar_pecuario$P_S7P84A)
count(SanCristobal_pecuario$P_S7P84A)
count(SantaFe_pecuario$P_S7P84A)
count(Usaquen_pecuario$P_S7P84A)
count(Usme_pecuario$P_S7P84A)
count(Suba_pecuario$P_S7P84A)
count(Otras_pecuario$P_S7P84A)

#	Cuántas hembras de ganado bovino tiene en ordeño?
count(sumapaz_pecuario$P_S7P84F)
count(chapinero_pecuario$P_S7P84F)
count(ciudadBolivar_pecuario$P_S7P84F)
count(SanCristobal_pecuario$P_S7P84F)
count(SantaFe_pecuario$P_S7P84F)
count(Usaquen_pecuario$P_S7P84F)
count(Usme_pecuario$P_S7P84F)
count(Suba_pecuario$P_S7P84F)
count(Otras_pecuario$P_S7P84F)

#Cantidad total de leche recolectada:
count(sumapaz_pecuario$P_S7P85B)
count(chapinero_pecuario$P_S7P85B)
count(ciudadBolivar_pecuario$P_S7P85B)
count(SanCristobal_pecuario$P_S7P85B)
count(SantaFe_pecuario$P_S7P85B)
count(Usaquen_pecuario$P_S7P85B)
count(Usme_pecuario$P_S7P85B)
count(Suba_pecuario$P_S7P85B)
count(Otras_pecuario$P_S7P85B)

#¿Durante el 2013 ha tenido cerdos(as) o marranos(as)?
table(sumapaz_pecuario$P_S7P86)
table(chapinero_pecuario$P_S7P86)
table(ciudadBolivar_pecuario$P_S7P86)
table(SanCristobal_pecuario$P_S7P86)
table(SantaFe_pecuario$P_S7P86)
table(Usaquen_pecuario$P_S7P86)
table(Usme_pecuario$P_S7P86)
table(Suba_pecuario$P_S7P86)
table(Otras_pecuario$P_S7P86)


#La orientación de la actividad porcícola ha sido para:Cría?
count(sumapaz_pecuario$P_S7P87_SP1)
count(chapinero_pecuario$P_S7P87_SP1)
count(ciudadBolivar_pecuario$P_S7P87_SP1)
count(SanCristobal_pecuario$P_S7P87_SP1)
count(SantaFe_pecuario$P_S7P87_SP1)
count(Usaquen_pecuario$P_S7P87_SP1)
count(Usme_pecuario$P_S7P87_SP1)
count(Suba_pecuario$P_S7P87_SP1)
count(Otras_pecuario$P_S7P87_SP1)

#	La orientación de la actividad porcícola ha sido para:Levante y ceba
count(sumapaz_pecuario$P_S7P87_SP2)
count(chapinero_pecuario$P_S7P87_SP2)
count(ciudadBolivar_pecuario$P_S7P87_SP2)
count(SanCristobal_pecuario$P_S7P87_SP2)
count(SantaFe_pecuario$P_S7P87_SP2)
count(Usaquen_pecuario$P_S7P87_SP2)
count(Usme_pecuario$P_S7P87_SP2)
count(Suba_pecuario$P_S7P87_SP2)
count(Otras_pecuario$P_S7P87_SP2)

#	La orientación de la actividad porcícola ha sido para:Ciclo completo
count(sumapaz_pecuario$P_S7P87_SP3)
count(chapinero_pecuario$P_S7P87_SP3)
count(ciudadBolivar_pecuario$P_S7P87_SP3)
count(SanCristobal_pecuario$P_S7P87_SP3)
count(SantaFe_pecuario$P_S7P87_SP3)
count(Usaquen_pecuario$P_S7P87_SP3)
count(Usme_pecuario$P_S7P87_SP3)
count(Suba_pecuario$P_S7P87_SP3)
count(Otras_pecuario$P_S7P87_SP3)

#La orientación de la actividad porcícola ha sido para:Genética
count(sumapaz_pecuario$P_S7P87_SP4)
count(chapinero_pecuario$P_S7P87_SP4)
count(ciudadBolivar_pecuario$P_S7P87_SP4)
count(SanCristobal_pecuario$P_S7P87_SP4)
count(SantaFe_pecuario$P_S7P87_SP4)
count(Usaquen_pecuario$P_S7P87_SP4)
count(Usme_pecuario$P_S7P87_SP4)
count(Suba_pecuario$P_S7P87_SP4)
count(Otras_pecuario$P_S7P87_SP4)

#	Respecto al número de cerdos: ¿Cuántos machos reproductores tiene hoy.?
count(sumapaz_pecuario$P_S7P89A)
count(chapinero_pecuario$P_S7P89A)
count(ciudadBolivar_pecuario$P_S7P89A)
count(SanCristobal_pecuario$P_S7P89A)
count(SantaFe_pecuario$P_S7P89A)
count(Usaquen_pecuario$P_S7P89A)
count(Usme_pecuario$P_S7P89A)
count(Suba_pecuario$P_S7P89A)
count(Otras_pecuario$P_S7P89A)

#Respecto al número de cerdas): ¿Cuántas hembras gestantes, lactantes y vacías tiene hoy?
count(sumapaz_pecuario$P_S7P89B)
count(chapinero_pecuario$P_S7P89B)
count(ciudadBolivar_pecuario$P_S7P89B)
count(SanCristobal_pecuario$P_S7P89B)
count(SantaFe_pecuario$P_S7P89B)
count(Usaquen_pecuario$P_S7P89B)
count(Usme_pecuario$P_S7P89B)
count(Suba_pecuario$P_S7P89B)
count(Otras_pecuario$P_S7P89B)

#Respecto al número de cerdos (as): ¿Cuántos (as) cerdos (as) destetos (as) ha tenido durante 2013?
count(sumapaz_pecuario$P_S7P89E)
count(chapinero_pecuario$P_S7P89E)
count(ciudadBolivar_pecuario$P_S7P89E)
count(SanCristobal_pecuario$P_S7P89E)
count(SantaFe_pecuario$P_S7P89E)
count(Usaquen_pecuario$P_S7P89E)
count(Usme_pecuario$P_S7P89E)
count(Suba_pecuario$P_S7P89E)
count(Otras_pecuario$P_S7P89E)

#Respecto al número de cerdos (as): ¿Cuántos (as) cerdos (as) cebadas (as) ha tenido durante 2013?
count(sumapaz_pecuario$P_S7P89F)
count(chapinero_pecuario$P_S7P89F)
count(ciudadBolivar_pecuario$P_S7P89F)
count(SanCristobal_pecuario$P_S7P89F)
count(SantaFe_pecuario$P_S7P89F)
count(Usaquen_pecuario$P_S7P89F)
count(Usme_pecuario$P_S7P89F)
count(Suba_pecuario$P_S7P89F)
count(Otras_pecuario$P_S7P89F)
count(pecuario$pecuario.P_S7P89F)
#1557

#Durante los últimos 12 meses; ¿se han criado gallinas o engordado pollos en galpones?
table(sumapaz_pecuario$P_S7P90)
table(chapinero_pecuario$P_S7P90)
table(ciudadBolivar_pecuario$P_S7P90)
table(SanCristobal_pecuario$P_S7P90)
table(SantaFe_pecuario$P_S7P90)
table(Usaquen_pecuario$P_S7P90)
table(Usme_pecuario$P_S7P90)
table(Suba_pecuario$P_S7P90)
table(Otras_pecuario$P_S7P90)
table(pecuario$pecuario.P_S7P90)
#13 registraron

#¿Tiene búfalos; equinos; ovinos o caprinos?
table(sumapaz_pecuario$P_S7P101)
table(chapinero_pecuario$P_S7P101)
table(ciudadBolivar_pecuario$P_S7P101)
table(SanCristobal_pecuario$P_S7P101)
table(SantaFe_pecuario$P_S7P101)
table(Usaquen_pecuario$P_S7P101)
table(Usme_pecuario$P_S7P101)
table(Suba_pecuario$P_S7P101)
table(Otras_pecuario$P_S7P101)
table(pecuario$pecuario.P_S7P101)


#ACLARACIÓN: Si responde 1 es porque tiene de 1 a 100 ejemplares

#Dígame la cantidad total que tiene de: Caballos
count(sumapaz_pecuario$P_S7P102C)
count(chapinero_pecuario$P_S7P102C)
count(ciudadBolivar_pecuario$P_S7P102C)
count(SanCristobal_pecuario$P_S7P102C)
count(SantaFe_pecuario$P_S7P102C)
count(Usaquen_pecuario$P_S7P102C)
count(Usme_pecuario$P_S7P102C)
count(Suba_pecuario$P_S7P102C)
count(Otras_pecuario$P_S7P102C)
count(pecuario$pecuario.P_S7P102C)


table(count(pecuario$pecuario.P_S7P102C))
#435

#	Dígame la cantidad total que tiene de: Yeguas
count(sumapaz_pecuario$P_S7P102D)
count(chapinero_pecuario$P_S7P102D)
count(ciudadBolivar_pecuario$P_S7P102D)
count(SanCristobal_pecuario$P_S7P102D)
count(SantaFe_pecuario$P_S7P102D)
count(Usaquen_pecuario$P_S7P102D)
count(Usme_pecuario$P_S7P102D)
count(Suba_pecuario$P_S7P102D)
count(Otras_pecuario$P_S7P102D)
count(pecuario$pecuario.P_S7P102D)
#200

#Dígame la cantidad total que tiene de: Mulos
count(sumapaz_pecuario$P_S7P102E)
count(chapinero_pecuario$P_S7P102E)
count(ciudadBolivar_pecuario$P_S7P102E)
count(SanCristobal_pecuario$P_S7P102E)
count(SantaFe_pecuario$P_S7P102E)
count(Usaquen_pecuario$P_S7P102E)
count(Usme_pecuario$P_S7P102E)
count(Suba_pecuario$P_S7P102E)
count(Otras_pecuario$P_S7P102E)
count(pecuario$pecuario.P_S7P102E)
#10

#	Dígame la cantidad total que tiene de: Mulas
count(sumapaz_pecuario$P_S7P102F)
count(chapinero_pecuario$P_S7P102F)
count(ciudadBolivar_pecuario$P_S7P102F)
count(SanCristobal_pecuario$P_S7P102F)
count(SantaFe_pecuario$P_S7P102F)
count(Usaquen_pecuario$P_S7P102F)
count(Usme_pecuario$P_S7P102F)
count(Suba_pecuario$P_S7P102F)
count(Otras_pecuario$P_S7P102F)
count(pecuario$pecuario.P_S7P102F)

#Dígame la cantidad total que tiene de: Burros
count(sumapaz_pecuario$P_S7P102G)
count(chapinero_pecuario$P_S7P102G)
count(ciudadBolivar_pecuario$P_S7P102G)
count(SanCristobal_pecuario$P_S7P102G)
count(SantaFe_pecuario$P_S7P102G)
count(Usaquen_pecuario$P_S7P102G)
count(Usme_pecuario$P_S7P102G)
count(Suba_pecuario$P_S7P102G)
count(Otras_pecuario$P_S7P102G)
count(pecuario$pecuario.P_S7P102G)
#16

#Dígame la cantidad total que tiene de: Burras
count(sumapaz_pecuario$P_S7P102H)
count(chapinero_pecuario$P_S7P102H)
count(ciudadBolivar_pecuario$P_S7P102H)
count(SanCristobal_pecuario$P_S7P102H)
count(SantaFe_pecuario$P_S7P102H)
count(Usaquen_pecuario$P_S7P102H)
count(Usme_pecuario$P_S7P102H)
count(Suba_pecuario$P_S7P102H)
count(Otas_pecuario$P_S7P102H)
count(pecuario$pecuario.P_S7P102H)
#6

#Dígame la cantidad total que tiene de: Cabros
count(sumapaz_pecuario$P_S7P102I)
count(chapinero_pecuario$P_S7P102I)
count(ciudadBolivar_pecuario$P_S7P102I)
count(SanCristobal_pecuario$P_S7P102I)
count(SantaFe_pecuario$P_S7P102I)
count(Usaquen_pecuario$P_S7P102I)
count(Usme_pecuario$P_S7P102I)
count(Suba_pecuario$P_S7P102I)
count(Otras_pecuario$P_S7P102I)
count(pecuario$pecuario.P_S7P102I)
#30

#Dígame la cantidad total que tiene de: Cabras
count(sumapaz_pecuario$P_S7P102J)
count(chapinero_pecuario$P_S7P102J)
count(ciudadBolivar_pecuario$P_S7P102J)
count(SanCristobal_pecuario$P_S7P102J)
count(SantaFe_pecuario$P_S7P102J)
count(Usaquen_pecuario$P_S7P102J)
count(Usme_pecuario$P_S7P102J)
count(Suba_pecuario$P_S7P102J)
count(Otras_pecuario$P_S7P102J)
count(pecuario$pecuario.P_S7P102J)
#25

#Dígame la cantidad total que tiene de: Ovejos
count(sumapaz_pecuario$P_S7P102K)
count(chapinero_pecuario$P_S7P102K)
count(ciudadBolivar_pecuario$P_S7P102K)
count(SanCristobal_pecuario$P_S7P102K)
count(SantaFe_pecuario$P_S7P102K)
count(Usaquen_pecuario$P_S7P102K)
count(Usme_pecuario$P_S7P102K)
count(Suba_pecuario$P_S7P102K)
count(Otras_pecuario$P_S7P102K)
count(pecuario$pecuario.P_S7P102K)
#339

#Dígame la cantidad total que tiene de: Ovejas
count(sumapaz_pecuario$P_S7P102L)
count(chapinero_pecuario$P_S7P102L)
count(ciudadBolivar_pecuario$P_S7P102L)
count(SanCristobal_pecuario$P_S7P102L)
count(SantaFe_pecuario$P_S7P102L)
count(Usaquen_pecuario$P_S7P102L)
count(Usme_pecuario$P_S7P102L)
count(Suba_pecuario$P_S7P102L)
count(Otras_pecuario$P_S7P102L)
count(pecuario$pecuario.P_S7P102L)
#689

#¿Tiene otras especies de animales? 
#(pollos, patos, piscos, avestruces, codornices, cuyes, conejos, colmenas, etc.)
table(sumapaz_pecuario$P_S7P105)
table(chapinero_pecuario$P_S7P105)
table(ciudadBolivar_pecuario$P_S7P105)
table(SanCristobal_pecuario$P_S7P105)
table(SantaFe_pecuario$P_S7P105)
table(Usaquen_pecuario$P_S7P105)
table(Usme_pecuario$P_S7P105)
table(Suba_pecuario$P_S7P105)
table(Otras_pecuario$P_S7P105)
table(pecuario$pecuario.P_S7P105)
#474 

#¿Cuántas aves entraron a galpones en piso durante 2013?
count(sumapaz_pecuario$P_S7P92A)
table(sumapaz_pecuario$P_S7P92A)
table(chapinero_pecuario$P_S7P92A)
table(ciudadBolivar_pecuario$P_S7P92A)
table(SanCristobal_pecuario$P_S7P92A)
table(SantaFe_pecuario$P_S7P92A)
table(Usaquen_pecuario$P_S7P92A)
table(Usme_pecuario$P_S7P92A)
table(Suba_pecuario$P_S7P92A)
table(Otras_pecuario$P_S7P92A)
table(pecuario$P_S7P92A)

#¿Cuántas aves salieron de galpones en piso durante 2013?
table(sumapaz_pecuario$P_S7P92B)
table(chapinero_pecuario$P_S7P92B)
table(ciudadBolivar_pecuario$P_S7P92B)
table(SanCristobal_pecuario$P_S7P92B)
table(SantaFe_pecuario$P_S7P92B)
table(Usaquen_pecuario$P_S7P92B)
table(Usme_pecuario$P_S7P92B)
table(Suba_pecuario$P_S7P92B)
table(Otras_pecuario$P_S7P92B)
table(pecuario$P_S7P92B)

#¿Cuántas aves entraron a galpones en jaula durante 2013?
table(sumapaz_pecuario$P_S7P93A)
table(chapinero_pecuario$P_S7P93A)
table(ciudadBolivar_pecuario$P_S7P93A)
table(SanCristobal_pecuario$P_S7P93A)
table(SantaFe_pecuario$P_S7P93A)
table(Usaquen_pecuario$P_S7P93A)
table(Usme_pecuario$P_S7P93A)
table(Suba_pecuario$P_S7P93A)
table(Otras_pecuario$P_S7P93A)
table(pecuario$P_S7P93A)

#¿Cuántas aves salieron de galpones en jaula durante 2013?
table(sumapaz_pecuario$P_S7P93B)
table(chapinero_pecuario$P_S7P93B)
table(ciudadBolivar_pecuario$P_S7P93B)
table(SanCristobal_pecuario$P_S7P93B)
table(SantaFe_pecuario$P_S7P93B)
table(Usaquen_pecuario$P_S7P93B)
table(Usme_pecuario$P_S7P93B)
table(Suba_pecuario$P_S7P93B)
table(Otras_pecuario$P_S7P93B)
table(pecuario$P_S7P93B)

#Caballos machos
count(sumapaz_pecuario$P_S7P102C)
count(chapinero_pecuario$P_S7P102C)
count(ciudadBolivar_pecuario$P_S7P102C)
count(SanCristobal_pecuario$P_S7P102C)
count(SantaFe_pecuario$P_S7P102C)
count(Usaquen_pecuario$P_S7P102C)
count(Usme_pecuario$P_S7P102C)
count(Suba_pecuario$P_S7P102C)
count(Otras_pecuario$P_S7P102C)
count(pecuario$P_S7P102C)

#Yeguas
count(sumapaz_pecuario$P_S7P102D)
count(chapinero_pecuario$P_S7P102D)
count(ciudadBolivar_pecuario$P_S7P102D)
count(SanCristobal_pecuario$P_S7P102D)
count(SantaFe_pecuario$P_S7P102D)
count(Usaquen_pecuario$P_S7P102D)
count(Usme_pecuario$P_S7P102D)
count(Suba_pecuario$P_S7P102D)
count(Otras_pecuario$P_S7P102D)
count(pecuario$P_S7P102D)

#Mulos
count(sumapaz_pecuario$P_S7P102E)
count(chapinero_pecuario$P_S7P102E)
count(ciudadBolivar_pecuario$P_S7P102E)
count(SanCristobal_pecuario$P_S7P102E)
count(SantaFe_pecuario$P_S7P102E)
count(Usaquen_pecuario$P_S7P102E)
count(Usme_pecuario$P_S7P102E)
count(Suba_pecuario$P_S7P102E)
count(Otras_pecuario$P_S7P102E)
count(pecuario$P_S7P102E)

#Mulas
count(sumapaz_pecuario$P_S7P102F)
count(chapinero_pecuario$P_S7P102F)
count(ciudadBolivar_pecuario$P_S7P102F)
count(SanCristobal_pecuario$P_S7P102F)
count(SantaFe_pecuario$P_S7P102F)
count(Usaquen_pecuario$P_S7P102F)
count(Usme_pecuario$P_S7P102F)
count(Suba_pecuario$P_S7P102F)
count(Otras_pecuario$P_S7P102F)
count(pecuario$P_S7P102F)

#Burros
count(sumapaz_pecuario$P_S7P102G)
count(chapinero_pecuario$P_S7P102G)
count(ciudadBolivar_pecuario$P_S7P102G)
count(SanCristobal_pecuario$P_S7P102G)
count(SantaFe_pecuario$P_S7P102G)
count(Usaquen_pecuario$P_S7P102G)
count(Usme_pecuario$P_S7P102G)
count(Suba_pecuario$P_S7P102G)
count(Otras_pecuario$P_S7P102G)
count(pecuario$P_S7P102G)

#Burras
count(sumapaz_pecuario$P_S7P102H)
count(chapinero_pecuario$P_S7P102H)
count(ciudadBolivar_pecuario$P_S7P102H)
count(SanCristobal_pecuario$P_S7P102H)
count(SantaFe_pecuario$P_S7P102H)
count(Usaquen_pecuario$P_S7P102H)
count(Usme_pecuario$P_S7P102H)
count(Suba_pecuario$P_S7P102H)
count(Otras_pecuario$P_S7P102H)
count(pecuario$P_S7P102H)

#Cabros
count(sumapaz_pecuario$P_S7P102I)
count(chapinero_pecuario$P_S7P102I)
count(ciudadBolivar_pecuario$P_S7P102I)
count(SanCristobal_pecuario$P_S7P102I)
count(SantaFe_pecuario$P_S7P102I)
count(Usaquen_pecuario$P_S7P102I)
count(Usme_pecuario$P_S7P102I)
count(Suba_pecuario$P_S7P102I)
count(Otras_pecuario$P_S7P102I)
count(pecuario$P_S7P102I)

#Cabras
count(sumapaz_pecuario$P_S7P102J)
count(chapinero_pecuario$P_S7P102J)
count(ciudadBolivar_pecuario$P_S7P102J)
count(SanCristobal_pecuario$P_S7P102J)
count(SantaFe_pecuario$P_S7P102J)
count(Usaquen_pecuario$P_S7P102J)
count(Usme_pecuario$P_S7P102J)
count(Suba_pecuario$P_S7P102J)
count(Otras_pecuario$P_S7P102J)
count(pecuario$P_S7P102J)

#Ovejos
count(sumapaz_pecuario$P_S7P102K)
count(chapinero_pecuario$P_S7P102K)
count(ciudadBolivar_pecuario$P_S7P102K)
count(SanCristobal_pecuario$P_S7P102K)
count(SantaFe_pecuario$P_S7P102K)
count(Usaquen_pecuario$P_S7P102K)
count(Usme_pecuario$P_S7P102K)
count(Suba_pecuario$P_S7P102K)
count(Otras_pecuario$P_S7P102K)
count(pecuario$P_S7P102K)

#Ovejas
count(sumapaz_pecuario$P_S7P102L)
count(chapinero_pecuario$P_S7P102L)
count(ciudadBolivar_pecuario$P_S7P102L)
count(SanCristobal_pecuario$P_S7P102L)
count(SantaFe_pecuario$P_S7P102L)
count(Usaquen_pecuario$P_S7P102L)
count(Usme_pecuario$P_S7P102L)
count(Suba_pecuario$P_S7P102L)
count(Otras_pecuario$P_S7P102L)
count(pecuario$P_S7P102L)



#Actividad ovina caprino

#La orientación de la actividad ovino caprina ha sido para:Carne
count(sumapaz_pecuario$P_S7P103_SP1)
count(chapinero_pecuario$P_S7P103_SP1)
count(ciudadBolivar_pecuario$P_S7P103_SP1)
count(SanCristobal_pecuario$P_S7P103_SP1)
count(SantaFe_pecuario$P_S7P103_SP1)
count(Usaquen_pecuario$P_S7P103_SP1)
count(Usme_pecuario$P_S7P103_SP1)
count(Suba_pecuario$P_S7P103_SP1)
count(Otras_pecuario$P_S7P103_SP1)
count(pecuario$P_S7P103_SP1)
#87

#Leche
count(sumapaz_pecuario$P_S7P103_SP2)
count(chapinero_pecuario$P_S7P103_SP2)
count(ciudadBolivar_pecuario$P_S7P103_SP2)
count(SanCristobal_pecuario$P_S7P103_SP2)
count(SantaFe_pecuario$P_S7P103_SP2)
count(Usaquen_pecuario$P_S7P103_SP2)
count(Usme_pecuario$P_S7P103_SP2)
count(Suba_pecuario$P_S7P103_SP2)
count(Otras_pecuario$P_S7P103_SP2)
count(pecuario$P_S7P103_SP2)
#27

#Lana
count(sumapaz_pecuario$P_S7P103_SP3)
count(chapinero_pecuario$P_S7P103_SP3)
count(ciudadBolivar_pecuario$P_S7P103_SP3)
count(SanCristobal_pecuario$P_S7P103_SP3)
count(SantaFe_pecuario$P_S7P103_SP3)
count(Usaquen_pecuario$P_S7P103_SP3)
count(Usme_pecuario$P_S7P103_SP3)
count(Suba_pecuario$P_S7P103_SP3)
count(Otras_pecuario$P_S7P103_SP3)
count(pecuario$P_S7P103_SP3)

#Pie de cría
count(sumapaz_pecuario$P_S7P103_SP4)
count(chapinero_pecuario$P_S7P103_SP4)
count(ciudadBolivar_pecuario$P_S7P103_SP4)
count(SanCristobal_pecuario$P_S7P103_SP4)
count(SantaFe_pecuario$P_S7P103_SP4)
count(Usaquen_pecuario$P_S7P103_SP4)
count(Usme_pecuario$P_S7P103_SP4)
count(Suba_pecuario$P_S7P103_SP4)
count(Otras_pecuario$P_S7P103_SP4)
count(pecuario$P_S7P103_SP4)

#Otro
count(sumapaz_pecuario$P_S7P103_SP5)
count(chapinero_pecuario$P_S7P103_SP5)
count(ciudadBolivar_pecuario$P_S7P103_SP5)
count(SanCristobal_pecuario$P_S7P103_SP5)
count(SantaFe_pecuario$P_S7P103_SP5)
count(Usaquen_pecuario$P_S7P103_SP5)
count(Usme_pecuario$P_S7P103_SP5)
count(Suba_pecuario$P_S7P103_SP5)
count(Otras_pecuario$P_S7P103_SP5)
count(pecuario$P_S7P103_SP5)


#Digame la cantidad total que tiene de: Cerdos o marranos de traspatio
count(sumapaz_pecuario$P_S7P106A)
count(chapinero_pecuario$P_S7P106A)
count(ciudadBolivar_pecuario$P_S7P106A)
count(SanCristobal_pecuario$P_S7P106A)
count(SantaFe_pecuario$P_S7P106A)
count(Usaquen_pecuario$P_S7P106A)
count(Usme_pecuario$P_S7P106A)
count(Suba_pecuario$P_S7P106A)
count(Otras_pecuario$P_S7P106A)
count(pecuario$P_S7P106A)
#211

#Digame la cantidad total que tiene de: Gallos, pollos y gallinas de traspatio

count(sumapaz_pecuario$P_S7P106B)
count(chapinero_pecuario$P_S7P106B)
count(ciudadBolivar_pecuario$P_S7P106B)
count(SanCristobal_pecuario$P_S7P106B)
count(SantaFe_pecuario$P_S7P106B)
count(Usaquen_pecuario$P_S7P106B)
count(Usme_pecuario$P_S7P106B)
count(Suba_pecuario$P_S7P106B)
count(Otras_pecuario$P_S7P106B)
count(pecuario$P_S7P106B)
#5361

#Digame la cantidad total que tiene de: Gallos finos o de pelea
count(sumapaz_pecuario$P_S7P106C)
count(chapinero_pecuario$P_S7P106C)
count(ciudadBolivar_pecuario$P_S7P106C)
count(SanCristobal_pecuario$P_S7P106C)
count(SantaFe_pecuario$P_S7P106C)
count(Usaquen_pecuario$P_S7P106C)
count(Usme_pecuario$P_S7P106C)
count(Suba_pecuario$P_S7P106C)
count(Otras_pecuario$P_S7P106C)
count(pecuario$P_S7P106C)
#99

#Digame la cantidad total que tiene de: Piscos, pavos o bimbos
count(sumapaz_pecuario$P_S7P106D)
count(chapinero_pecuario$P_S7P106D)
count(ciudadBolivar_pecuario$P_S7P106D)
count(SanCristobal_pecuario$P_S7P106D)
count(SantaFe_pecuario$P_S7P106D)
count(Usaquen_pecuario$P_S7P106D)
count(Usme_pecuario$P_S7P106D)
count(Suba_pecuario$P_S7P106D)
count(Otras_pecuario$P_S7P106D)
count(pecuario$P_S7P106D)
#74

#	Digame la cantidad total que tiene de: Patos y gansos
count(sumapaz_pecuario$P_S7P106E)
count(chapinero_pecuario$P_S7P106E)
count(ciudadBolivar_pecuario$P_S7P106E)
count(SanCristobal_pecuario$P_S7P106E)
count(SantaFe_pecuario$P_S7P106E)
count(Usaquen_pecuario$P_S7P106E)
count(Usme_pecuario$P_S7P106E)
count(Suba_pecuario$P_S7P106E)
count(Otras_pecuario$P_S7P106E)
count(pecuario$P_S7P106E)
#386

#Digame la cantidad total que tiene de: Codornices
count(sumapaz_pecuario$P_S7P106F)
count(chapinero_pecuario$P_S7P106F)
count(ciudadBolivar_pecuario$P_S7P106F)
count(SanCristobal_pecuario$P_S7P106F)
count(SantaFe_pecuario$P_S7P106F)
count(Usaquen_pecuario$P_S7P106F)
count(Usme_pecuario$P_S7P106F)
count(Suba_pecuario$P_S7P106F)
count(Otras_pecuario$P_S7P106F)
count(pecuario$P_S7P106F)
#10

#Avestruces

count(pecuario$P_S7P106G)
#0

#Cuyes
count(sumapaz_pecuario$P_S7P106H)
count(chapinero_pecuario$P_S7P106H)
count(ciudadBolivar_pecuario$P_S7P106H)
count(SanCristobal_pecuario$P_S7P106H)
count(SantaFe_pecuario$P_S7P106H)
count(Usaquen_pecuario$P_S7P106H)
count(Usme_pecuario$P_S7P106H)
count(Suba_pecuario$P_S7P106H)
count(Otras_pecuario$P_S7P106H)
count(pecuario$P_S7P106H)
#64

#Conejos
count(sumapaz_pecuario$P_S7P106I)
count(chapinero_pecuario$P_S7P106I)
count(ciudadBolivar_pecuario$P_S7P106I)
count(SanCristobal_pecuario$P_S7P106I)
count(SantaFe_pecuario$P_S7P106I)
count(Usaquen_pecuario$P_S7P106I)
count(Usme_pecuario$P_S7P106I)
count(Suba_pecuario$P_S7P106I)
count(Otras_pecuario$P_S7P106I)
count(pecuario$P_S7P106I)
#643

#Digame la cantidad total que tiene de: Colmenas productoras de miel
count(sumapaz_pecuario$P_S7P106J)
count(chapinero_pecuario$P_S7P106J)
count(ciudadBolivar_pecuario$P_S7P106J)
count(SanCristobal_pecuario$P_S7P106J)
count(SantaFe_pecuario$P_S7P106J)
count(Usaquen_pecuario$P_S7P106J)
count(Usme_pecuario$P_S7P106J)
count(Suba_pecuario$P_S7P106J)
count(Otras_pecuario$P_S7P106J)
count(pecuario$P_S7P106J)
#75

#Colmenas productoras de polen
count(sumapaz_pecuario$P_S7P106K)
count(chapinero_pecuario$P_S7P106K)
count(ciudadBolivar_pecuario$P_S7P106K)
count(SanCristobal_pecuario$P_S7P106K)
count(SantaFe_pecuario$P_S7P106K)
count(Usaquen_pecuario$P_S7P106K)
count(Usme_pecuario$P_S7P106K)
count(Suba_pecuario$P_S7P106K)
count(Otras_pecuario$P_S7P106K)
count(pecuario$P_S7P106K)
#15

#Colmenas productoras de otros productos
count(sumapaz_pecuario$P_S7P106L)
count(chapinero_pecuario$P_S7P106L)
count(ciudadBolivar_pecuario$P_S7P106L)
count(SanCristobal_pecuario$P_S7P106L)
count(SantaFe_pecuario$P_S7P106L)
count(Usaquen_pecuario$P_S7P106L)
count(Usme_pecuario$P_S7P106L)
count(Suba_pecuario$P_S7P106L)
count(Otras_pecuario$P_S7P106L)
count(pecuario$P_S7P106L)
#3

#Colmenas de abejas sin aguijon
count(sumapaz_pecuario$P_S7P106M)
count(chapinero_pecuario$P_S7P106M)
count(ciudadBolivar_pecuario$P_S7P106M)
count(SanCristobal_pecuario$P_S7P106M)
count(SantaFe_pecuario$P_S7P106M)
count(Usaquen_pecuario$P_S7P106M)
count(Usme_pecuario$P_S7P106M)
count(Suba_pecuario$P_S7P106M)
count(Otras_pecuario$P_S7P106M)
count(pecuario$P_S7P106M)


#área sembrada en pastos
count(sumapaz_pecuario$P_S6P68)
count(chapinero_pecuario$P_S6P68)
count(ciudadBolivar_pecuario$P_S6P68)
count(SanCristobal_pecuario$P_S6P68)
count(SantaFe_pecuario$P_S6P68)
count(Usaquen_pecuario$P_S6P68)
count(Usme_pecuario$P_S6P68)
count(Suba_pecuario$P_S6P68)
count(Otras_pecuario$P_S6P68)
count(pecuario$P_S6P68)
#183.768.761

#Los productos que obtiene de pastos sembrados son:

#semilla

count(sumapaz_pecuario$P_S6P69_SP1)
count(chapinero_pecuario$P_S6P69_SP1)
count(ciudadBolivar_pecuario$P_S6P69_SP1)
count(SanCristobal_pecuario$P_S6P69_SP1)
count(SantaFe_pecuario$P_S6P69_SP1)
count(Usaquen_pecuario$P_S6P69_SP1)
count(Usme_pecuario$P_S6P69_SP1)
count(Suba_pecuario$P_S6P69_SP1)
count(Otras_pecuario$P_S6P69_SP1)
count(pecuario$P_S6P69_SP1)



#Hoy existen viveros
table(sumapaz_pecuario$P_S6P72)
table(chapinero_pecuario$P_S6P72)
table(ciudadBolivar_pecuario$P_S6P72)
table(SanCristobal_pecuario$P_S6P72)
table(SantaFe_pecuario$P_S6P72)
table(Usaquen_pecuario$P_S6P72)
table(Usme_pecuario$P_S6P72)
table(Suba_pecuario$P_S6P72)
table(Otras_pecuario$P_S6P72)
table(pecuario$P_S6P72)
#si 15 no 3892 1005 no responden

pl<-data.frame(table(pecuario_localidad$Localidad))
sum(pl$Freq)
