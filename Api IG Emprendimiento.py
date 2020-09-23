# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import instaloader
from instaloader import Instaloader, Profile
import json

from datetime import datetime
from itertools import dropwhile, takewhile

loader = Instaloader()
SINCE = datetime(2020,9,1)
UNTIL = datetime(2020,9,10)
NUM_POSTS = 10000
def get_hashtags_posts(query):
    posts = loader.get_hashtag_posts(query)
    prof_bio=[]
    count = 0
    for post in posts:
    #for post in takewhile(lambda p: p.date > SINCE, dropwhile(lambda p: p.date > UNTIL, posts)):
        #print(post)
        caption=post.caption
        postprofile=post.profile
        bio=Profile.from_username(loader.context, postprofile)
        biopf=bio.biography
        numfollowers=bio.followers
        busscount=bio.is_business_account
        busscateg=bio.business_category_name
        prof_bio.append([postprofile,biopf,numfollowers,caption,
                         busscount,busscateg])
        count += 1
        if count == NUM_POSTS:
                break
    return prof_bio

tt=get_hashtags_posts('EmprendedoresBogota')
tt

usuario=[]
bio=[]
numf=[]
cap=[]
for i in range(0,9232):
    usuario.append(tt[i][0])
    bio.append(tt[i][1])
    numf.append(tt[i][2])
    cap.append(tt[i][3])


import pandas as pd

data1=pd.DataFrame(list(zip(usuario, bio, numf,cap)),columns =['Perfil', 'Bio', 'Followers','Caption']) 
data1=data.drop_duplicates()
data.to_excel ('EmprendimientoBta50000.xlsx', index = False, header=True)

tb=get_hashtags_posts('Bogota')



n= Profile.from_username(loader.context, "soyemprendator")
n.is_business_account
n.business_category_name


for i in n.get_posts():
    print(i.date)

usuario=[]
bio=[]
numf=[]
cap=[]
sector=[]
for i in range(0,9):
    usuario.append(tb[i][0])
    bio.append(tb[i][1])
    numf.append(tb[i][2])
    cap.append(tb[i][3])
    #sector.append(tb[i][4])
    
data=pd.DataFrame(list(zip(usuario, bio, numf,cap)), columns =['Perfil', 'Bio', 'Followers','Caption']) 

            
#Identificar tiempo de post
#Tiempo del post para un solo perfil
timepost=[]
for i in n.get_posts():
    timepost.append(i.date)
timepost[0]    
timepost[-1]

        
#Tiempo para todos los perfiles
for t in data['Perfil']:
    n=Profile.from_username(loader.context,t)
    for h in range(0, 9):
        data['Time post'][h]=[i.date for i in n.get_posts()]
       

for i in n.get_posts():
    print(i.date)
     

import pandas as pd            
muestra=pd.read_csv('EmprendimientoBta50000.csv')
result_df = muestra_filtrar.drop_duplicates(subset=['Perfil', 'Bio'])
EmprendimientoBta
empbta=pd.read_csv('empbta.csv')
        
empbta['Time post']=0
tiempopost=[]
for t in empbta['Time post']:
    n=Profile.from_username(loader.context,t)
    tiempopost.append([i.date for i in n.get_posts()])
            
for h in range(0,len(empbta['Perfil'])):
    empbta['Time post'][h]=tiempop1[h]
    

         
tiempop1=tiempop1+tiempopost2

#Cambiar formato de fecha posts
empbta['Time post'][1][0].strftime('%b %d,%Y')


x=muestra['Time post'][1][0]
x.strftime('%b %d,%Y')
        

#Obtención del primer y ultimo post
empbta['Primer post']=0
empbta['Ultimo post']=0    


for i in range(0, 1230):
    empbta['Primer post'][i]= empbta['Time post'][i][-1].strftime('%b %d,%Y')
    empbta['Ultimo post'][i]= empbta['Time post'][i][0].strftime('%b %d,%Y')
    
for i in range(0, 1231):
    if len(empbta['Time post'][i])!=0:
        empbta['Primer post'][i]= empbta['Time post'][i][-1].strftime('%b %d,%Y')
        empbta['Ultimo post'][i]= empbta['Time post'][i][0].strftime('%b %d,%Y')

 empbta.to_excel ('empbta.xlsx', index = False, header=True)   


#Identificar sectores
    
muestra['Sector']=0
    
ropa=['ropa','pijama','camisas','Pijama','camiseta','tshirts','tshirt']
for i in ropa:
    for j in range(0,9):
        if i in muestra['Caption'][j]:
             muestra['sector'][j]='Textil'
            
comida=['Comida','Restaurante','Desayuno','restauranre','comida',
        'chocolate','fresas','reseta']
for i in comida:
    for j in range(0,9):
        if i in muestra['Caption'][j]:
            muestra['sector'][j]='Alimentos'
            
belleza=['Maquillaje','maquillaje','cuidado','cabello',
         'uñas','hair','belleza','beauty','brillo']

for i in belleza:
    for j in range(0,9):
        if i in muestra['Caption'][j]:
            muestra['sector'][j]='Belleza'


muestra.to_excel ('Muestra.xlsx', index = False, header=True)
    

    
 print(muestra[muestra['Perfil']=='juanamartina.arteenaccesorios'].index.values)

muestra_filtrar=muestra.iloc[0:8436,]

#Filtrar repetidos
result_df = muestra_filtrar.drop_duplicates(subset=['Perfil', 'Bio'])
rdf=empbta.iloc[139:1230,]

    

muestra=empbta.iloc[0:6,]

timepoMuestra=[]
for t in muestra['Perfil']:
    n=Profile.from_username(loader.context,t)
    timepoMuestra.append([i.date for i in n.get_posts()])
