

"""
Transforma Psat para o arquivo obsvervado.csv localizado em Trabalho/<nome subbacia>/observado.csv

Como rodar:

python psat2csv <date> <nprev> <lag> <ndays>

date: data do dia  (yyyymmdd)
nprev: numero de dias de previsao (o numero de columnas do aqruivo observado.csv, usado para SMAP ONS 12)
lag= lag de dias ate a data do dia 
ndays: numero de dias a serem usados desde a data  do dia

"""

#################################################################
#  IMPORTING PACKAGES 
################################################################

import numpy as np
from shapely.geometry import MultiPoint, Point, Polygon,shape
from shapely.geometry.polygon import Polygon
import os
import pandas as pd
import datetime 
import sys
import glob


date=datetime.datetime.strptime(str(sys.argv[1]),"%Y%m%d").date()
nprev=int(sys.argv[2])
lag=int(sys.argv[3])
Ndays=int(sys.argv[4])

######################################################################
# READING CONFIGURATION FILES 
#########################################################

file_xlsx=os.getcwd()+'/Parametros/Configuracao.xlsx'
lookup_xlsx=pd.read_excel(file_xlsx,sheet_name = "Plan1")
lookup_xlsx.index=[i.strip() for i in lookup_xlsx['Codigo ANA'].values]


# caminho dos contornos
caminhocontornos=os.getcwd()+'/Parametros/Contornos/ETA40'
input_bacias=lookup_xlsx[['Macro-Bacia','Nome']]

caminhopsat=os.getcwd()+'/Arq_Entrada/Observado/PSAT_ONS'




# data final e inciial  dos arquivos de observacao

date_end=date-datetime.timedelta(lag)
date_ini=date-datetime.timedelta(Ndays)
print(date_ini)

theDate=date_ini
ALL_LIST=[] # for creating Dataframe




while theDate <= date_end:
	#count.date<-count.date+1

    file='%s/psat_%s.txt' %(caminhopsat,datetime.datetime.strftime(theDate,"%d%m%Y"))

    PP_psat=pd.read_csv(file,header=None,delim_whitespace=True) 
    PP_psat.index=PP_psat[0]


    Coinci=set(PP_psat.index).intersection(set(lookup_xlsx.index))
    DATA_COINCI=pd.DataFrame([])
    DATA_COINCI['PP']=PP_psat.loc[Coinci,3]
    DATA_COINCI['subb']=lookup_xlsx.loc[Coinci,'Nome'] 
    DATA_COINCI['bacia']=lookup_xlsx.loc[Coinci,'Macro-Bacia'] 
    DATA_COINCI['date']=[theDate]*len(DATA_COINCI)

    ALL_LIST.append(DATA_COINCI)

    theDate+=datetime.timedelta(1)

ALL_LIST=pd.concat(ALL_LIST,axis=0)

#ALL_LIST=pd.DataFrame(ALL_LIST,columns=['lon','lat','PP','bacia','subb','date']) 

ALL_LIST.index=pd.to_datetime(ALL_LIST['date'])
for var,Data in ALL_LIST.groupby(['subb','bacia']):
    sub=var[0]
    bacia=var[1]
    if nprev==12:
    	FILE_TRABALHO='Trabalho/%s/%s/observado.csv' %(bacia,sub.strip())
    else:	
    	FILE_TRABALHO='Trabalho/%s/%s/observado_%d.csv' %(bacia,sub.strip(),nprev)
    DATA=pd.DataFrame([],columns=['D%d'%i for i in range(1,nprev+1)])
    
    print('creating file %s' %FILE_TRABALHO)
    for dti in range(len(Data.index)):
        DIAS=(Data.index-Data.index[dti]).astype('timedelta64[D]')
        SERIE={}
        for dias in range(nprev):

            val=Data.loc[DIAS==dias,'PP'].values
            if len(val)==0:
                val=np.nan
                SERIE['D%d'%(dias+1)]='%5.2f'%val
            else:
                val=val[0]
                SERIE['D%d'%(dias+1)]='%5.2f'%val    

            
        row = pd.Series(SERIE,name='/'.join([str(Data.index[dti].day),str(Data.index[dti].month),str(Data.index[dti].year)]))
        DATA = DATA.append(row)

    DATA.index=pd.to_datetime(DATA.index,dayfirst=True)
    DATA=DATA.sort_index() # ordering index, 

    DATA.index=DATA.index.strftime('%d/%m/%Y') # converting to the format 
        
    DATA[DATA.columns]=DATA[DATA.columns].astype(float) # every columns in float format 


    DATA.to_csv(FILE_TRABALHO,sep=';',decimal=',') #writing a file

