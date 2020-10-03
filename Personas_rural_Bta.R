install.packages('writexl')
library("writexl")


#Personas
Personas <- read.csv("~/Documents/SDDE/Economía Rural/11Bogota/CNA2014_S15P_11.csv", header=FALSE)
colnames(Personas)<-Personas[1,]
Personas<-Personas[2:6013,]
table(Personas$ENCUESTA)[1]
Personas[which(Personas$ENCUESTA=='1180719'),]
Cultivo_clasificacion[which(Cultivo_clasificacion$Codigo==i),][2]

#Sexo
table(Personas$P_S15P168)
#Se registraron 3007 hombres y 3005 hombres


#Edades
Edades<-as.data.frame(table(Personas$P_S15P169))
Edades<-Edades[order(Edades$Var1,decreasing = FALSE), ]


#Multiproposito
#Clasificacion de directorios que sean rurales
Identificacion_mp <- read.csv2("~/Downloads/Identificacion ( Capitulo A)/Identificacion ( Capitulo A).csv", header=FALSE)
name<-Identificacion_mp[1,]
name<-as.vector(name)
Identificacion_mp=Identificacion_mp[2:107219,]
colnames(Identificacion_mp)<-c(name)
I=Identificacion_mp[which(Identificacion_mp$CLASE=="3"),]

#Personas
Personas_mp <- read.csv2("~/Downloads/Composicion del hogar y demografia ( Capitulo E)/Composicion del hogar y demografia ( Capitulo E).csv", header=FALSE)
namepersonas<-Personas_mp[1,]
namepersonas<-as.vector(namepersonas)
Personas_mp=Personas_mp[2:319953,]
colnames(Personas_mp)<-c(namepersonas)


Personas_mp<-data.frame(Personas_mp$DIRECTORIO_PER,Personas_mp$DIRECTORIO_HOG,Personas_mp$DIRECTORIO,
                        Personas_mp$NPCEP4,Personas_mp$NPCEP5,Personas_mp$NPCEP6,Personas_mp$NPCEP11A,
                        Personas_mp$NPCEP15,Personas_mp$NPCEP17,Personas_mp$NPCEP22)


clas_rural=data.frame()

Personas_mp[which(Personas_mp$Personas_mp.DIRECTORIO=='101000'),][c(1,2)][1]


for(i in I$DIRECTORIO){
  print(i)
  k=Personas_mp[which(Personas_mp$Personas_mp.DIRECTORIO==i),]
  clas_rural=rbind(clas_rural,k)
}

#Localidad
Localidad<-c()
for(i in 1:7239){
  for(j in 1:length(Identificacion_mp$DIRECTORIO)){
    if(clas_rural$DIRECTORIO[i]==Identificacion_mp$DIRECTORIO[j]){
      Localidad<-c(Localidad,I$LOCALIDAD_TEX[i])
    }
  }
}
I$LOCALIDAD_TEX[3]

#Clas con localidad
clas_rural_localidad<-cbind(clas_rural,Localidad)

personas_localidad<-data.frame(table(clas_rural_localidad$Localidad))
write_xlsx(personas_localidad,"/home/tata/personas_localidad_mp.xlsx")


####################################CARACTERIZACION#############################
#NPCEP4:4.¿Cuántos años cumplidos tiene ... ?
#NPCEP5:5. Sexo:
#NPCEP6: 6.¿Cuál es el parentesco de ... con el o la jefe(a) de este hogar?
#NPCEP11A: 11.¿Donde nació ...?
#NPCEP15: 15. ¿Cuál fue la principal razón para venir a este municipio?
#NPCEP17:  19. De acuerdo con su cultura, pueblo o rasgos físicos, ... es o se reconoce como:
#1 » Indígena
#2 » Gitano(a) (ROM)
#3 » Raizal del archipiélago de San Andrés, Providencia y Santa Catalina
#4 » Palenquero(a) de San Basilio
#5 » Negro(a), mulato(a) (afrodescendiente)
#6 » De ninguno de los anteriores

#NPCEP22:23. ¿Cuál es o fue el nivel de educación más alto alcanzado por el padre de ... ?
#1 » Algunos años de primaria
#2 » Toda la primaria
#3 » Algunos años de secundaria
#4 » Toda la secundaria
#5 » Uno o más años de técnica o tecnológica
#6 » Técnica o tecnológica completa (con o sin título) 
#7 » Algunos años de universidad
#8 » Universitaria completa (con título)
#9 » Posgrado
#10 » Ninguno
#99 » No sabe



colnames(clas_rural)<-c('DIRECTORIO_PER','DIRECTORIO_HOG','DIRECTORIO',
                        'NPCEP4','NPCEP5','NPCEP6', 'NPCEP11A','NPCEP15',
                        'NPCEP17', 'NPCEP22')

#Cuantos años tiene cumplidos?
Edad_mp<-data.frame(table(clas_rural$NPCEP4))
write_xlsx(Edad_mp,"home/Documents/SDDE/Economia Rural/Caracterizacion/
           Multiproposito/Edad_mp.xlsx")
write_xlsx(Edad_mp,"/home/tata/Edad_mp.xlsx")

#Sexo
Sexo_mp<-data.frame(table(clas_rural$NPCEP5))
write_xlsx(Sexo_mp,"/home/tata/Sexo_mp.xlsx")

#Parentezco
Parentezco_mp<-data.frame(table(clas_rural$NPCEP6))

#Mujer Cabeza de hohar
Mujer<-clas_rural[which(clas_rural$NPCEP5=='2'),]
Mujer_Cabeza<-Mujer[which(Mujer$NPCEP6=='1'),]

#Hombre Cabeza de hogar
Hombre<-clas_rural[which(clas_rural$NPCEP5=='1'),]
Hombre_Cabeza<-Mujer[which(Hombre$NPCEP6=='1'),]


#Donde nació?
nacimiento_mp<-data.frame(table(clas_rural$NPCEP11A))
write_xlsx(Sexo_mp,"/home/tata/nacimiento_mp.xlsx")

#Cual fue la razón para venir a este municipio
migrar_municipio<-data.frame(table(clas_rural$NPCEP15))
write_xlsx(migrar_municipio,"/home/tata/migrar_municipio_mp.xlsx")

#Se reconoce como:
grupo_etnico<-data.frame(table(clas_rural$NPCEP17))
grupo_etnico
write_xlsx(grupo_etnico,"/home/tata/grupo_etnico.xlsx")

#¿Cuál es o fue el nivel de educación más alto alcanzado por el padre de
edu_padre<-data.frame(table(clas_rural$NPCEP22))
edu_padre
write_xlsx(edu_padre,"/home/tata/edu_padre.xlsx")


#############################################Salud############################################

Salud_rural_mp<- read.csv2("~/Downloads/Salud ( Capitulo F)/Salud ( Capitulo F).csv", header=FALSE)
namesalud<-Salud_rural_mp[1,]
namesalud<-as.vector(namesalud)
Salud_rural_mp=Salud_rural_mp[2:319953,]
colnames(Salud_rural_mp)<-c(namesalud)

Salud_rural_mp<-Salud_rural_mp[,c(1,2,3,5,6,7)]

Salud_rural<-data.frame()
for(i in I$DIRECTORIO){
  print(i)
  k=Salud_rural_mp[which(Salud_rural_mp$DIRECTORIO==i),]
  Salud_rural=rbind(Salud_rural,k)
}

#NPCFP1:1. ¿ está afiliado(a) ( cotizante o beneficiario(a)) a alguna entidad 
#de seguridad social en salud? 
#1 » Si 2 » No 9 » No sabe, no informa

#NPCFP2:2. ¿A cuál de los siguientes regímenes de seguridad social en salud está afiliado(a)?
#1 » Contributivo  2 » Especial o de Excepción (Fuerzas militares y de policía, Ecopetrol, universidades públicas, magisterio
#3 » Subsidiado (EPS-S) 9 » No sabe, no informa
                             


#Está afiliado:
afiliacion_salud<-data.frame(table(Salud_rural$NPCFP1))
afiliacion_salud
write_xlsx(afiliacion_salud,"/home/tata/afiliacion_salud.xlsx")

#Regimen afiliado
regimen_salud<-data.frame(table(Salud_rural$NPCFP2))
regimen_salud
write_xlsx(regimen_salud,"/home/tata/regimen_salud.xlsx")


############################################Educación##########################################

#NPCHP1: 1. ¿ ... sabe leer y escribir?
#1 » Si
#2 » No

#NPCHP4:4. ¿Cuál es el nivel educativo más alto alcanzado por 
#... y el último año o grado aprobado en este nivel?
#Pregunta abierta

Educacion_rural <- read.csv2("~/Downloads/Educacion (capitulo H)/Educacion (capitulo H).csv", header=FALSE)
nameeducacion<-Educacion_rural[1,]
nameeducacion<-as.vector(nameeducacion)
Educacion_rural=Educacion_rural[2:301824,]
colnames(Educacion_rural)<-c(nameeducacion)

Educacion_rural_mp<-Educacion_rural[,c(1,2,3,6,9)]

Educacion_rural<-data.frame()
for(i in I$DIRECTORIO){
  print(i)
  k=Educacion_rural_mp[which(Educacion_rural_mp$DIRECTORIO==i),]
  Educacion_rural=rbind(Educacion_rural,k)
}

#Sabe leer Y escribir?

Leer<-data.frame(table(Educacion_rural$NPCHP1))
Leer
write_xlsx(Leer,"/home/tata/leer.xlsx")

Estudios_Alcanzados<-data.frame(table(Educacion_rural$NPCHP4))
Estudios_Alcanzados
#NO se tiene un rango para saber a que se refiere cada uno








######################################### Trabajo ############################################

#NPCKP1: 1. ¿En qué actividad ocupó ... la mayor parte del tiempo la SEMANA PASADA?
#1 » Trabajando
#2 » Buscando trabajo
#3 » Estudiando
#4 » Oficios del hogar
#5 » Incapacitado(a) permanente para trabajar
#6 » Otra actividad

#NPCKP17:17. En este trabajo ... es:

#1 » Obrero o empleado de empresa particular
#2 » Obrero o empleado del gobierno
#3 » Empleado doméstico
#4 » Profesional independiente
#5 » Trabajador independiente o por cuenta propia
#6 » Patrón o empleador
#7 » Trabajador de su propia finca o de finca en arriendo o aparcería
#8 » Trabajador familiar sin remuneración
#9 » Ayudante sin remuneración (hijo o familiar de empleados domésticos, mayordomos, jornaleros, etc.)
#10 » Trabajador sin remuneración en empresas o negocios de otros hogares
#11 » Jornalero o peón


#NPCKP24A: 24. ¿Cuánto recibió?
#Pregunta abierta

trabajo_rural <- read.csv2("~/Downloads/Fuerza de trabajo  (capitulo K)/Fuerza de trabajo  (capítulo K).csv", header=FALSE)

nametrabajo<-trabajo_rural[1,]
nametrabajo<-as.vector(nametrabajo)
trabajo_rural=trabajo_rural[2:281183,]
colnames(trabajo_rural)<-c(nametrabajo)

Trabajo_rural_mp<-trabajo_rural[,c(1,2,3,6,19,28)]

Trabajo_rural<-data.frame()
for(i in I$DIRECTORIO){
  print(i)
  k=Trabajo_rural_mp[which(Trabajo_rural_mp$DIRECTORIO==i),]
  Trabajo_rural=rbind(Trabajo_rural,k)
}


#En que actividad se ocupo la semana pasada

ocupacion<-data.frame(table(Trabajo_rural$NPCKP1))
ocupacion
write_xlsx(ocupacion,"/home/tata/ocupacion.xlsx")

#En ese trabajo es

empleo<-data.frame(table(Trabajo_rural$NPCKP17))
empleo
write_xlsx(empleo,"/home/tata/empleo.xlsx")

#Cuanto recibio

remuneracion<-data.frame(table(Trabajo_rural$NPCKP24A))
remuneracion 

