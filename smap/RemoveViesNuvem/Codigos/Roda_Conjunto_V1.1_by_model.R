  cat("\014") 
  rm(list=ls())
  library(parallel)
  library(readxl)
  
  #args = commandArgs(trailingOnly=TRUE)
  
  model_nam<-"ECMWF"
  dias_previstos<-12
  fecha<-"20201208" 
  model_name<-unlist(strsplit(model_nam, '-'))
  observado<-"observado"
  agrupamento<-3
  tempo_regressao<-120
  observado_i<-unlist(strsplit(observado, '_'))


  #setwd("/home/leo/Documents/RemoveViesNuvem/")
  #source('./Codigos_R/Conjunto_V1.4.R')
  source('./Codigos/Conjunto_V1.4.R')
  #source('./Codigos/Conjunto_V1.4_test.R')
  

  # arquivo log
  arq_log<- paste0(getwd(),"/log.txt")
  write("-----Calculando Previsao do conjunto com remocao de vies-----",file=arq_log,append=TRUE)
  
  #=================================data (caso queira rodar outra data alterar aqui)================================================================================
  dia_previsao<-as.Date(fecha,"%Y%m%d")
  #dia_previsao<-as.Date('31/10/2019', "%d/%m/%Y")
  texto = paste0("data da rodada:",dia_previsao,"\n")
  cat(texto)
  write(texto,file=arq_log,append=TRUE)
  
  #=================================Parametros da rodada============================================================================================================
  #tempo_regressao<-120
  #dias_previstos<-9
  #dias_previstos<-9
  #modelos<-c('ETA40','GEFS')


  
  modelos <- model_name
  alpha<-2
  #print(modelos)
  beta<-1
  lambdas<-seq(0,0.5,by=0.01)
  
  #=================================Leitura do arquivo de configuracao===============================================================================================
  planilha <- read_xlsx("./Parametros/Configuracao.xlsx",sheet = "Plan1")
  bacias<-cbind(planilha$'Macro-Bacia',planilha$Nome)
  diretorios<-NULL
  for ( i in 1:nrow(bacias)){diretorios<-rbind(diretorios,paste0(getwd(),"/Trabalho/",bacias[i,1],"/",bacias[i,2]))}
  texto = "Arquivo de configuracao lido com sucesso \n"
  cat(texto)
  write(texto,file=arq_log,append=TRUE)
  
  #================================ cria o cluster ==================================================================================================================
 # numCores <- detectCores()
  ensemble<-matrix(NA_real_,nrow=dias_previstos,ncol=length(diretorios))
  clust <- makeCluster(8, type = 'PSOCK') 
  clusterExport(clust, varlist = c('roda_bacia', 'roda_lp','dia_previsao','tempo_regressao','dias_previstos','agrupamento','modelos','alpha','beta','lambdas','diretorios','observado'), envir = .GlobalEnv)
  
  #=========================== roda o conjunto para as bacias ========================================================================================================
  texto = "Gerando Conjunto \n"
  cat(texto)
  write(texto,file=arq_log,append=TRUE)
  
  res<-parLapply(clust,1:length(diretorios), function(x) roda_bacia(diretorios[x],dia_previsao,tempo_regressao,dias_previstos,agrupamento,modelos,alpha,beta,lambdas,observado))
  stopCluster(clust)
  
  texto = "Conjunto gerado \n"
  cat(texto)
  write(texto,file=arq_log,append=TRUE)
  


  if ("observado" %in% observado_i){
  
  #===========================aplica os limites======================================================================================================================
  for( i in 1:length(diretorios)){res[[i]][[1]][res[[i]][[1]]>planilha$`P lim d`[i] ]<-planilha$`P lim d`[i]}
  
  #============================ gera os arquivos de saida ============================================================================================================
  
  } else{

   print("limites nao aplicados")

  }

  texto = "Gerando arquivos de saida\n"
  cat(texto)
  write(texto,file=arq_log,append=TRUE)
  
  for( i in 1:dias_previstos){
    arq<-paste0(getwd(),paste("/Arq_Saida_com_remocao_vies/PMEDIA_",model_nam, "_p", sep=""),format(dia_previsao, format="%d%m%y"),"a",format((dia_previsao+i), format="%d%m%y"),".dat")
    file.create(arq)
    for( j in 1:length(diretorios)){
      texto<-paste0(format(planilha$Longitude[j], nsmall=2)," ",format(planilha$Latitude[j], nsmall=2)," ", format(round(res[[j]][[1]][i],1),nsmall=2))
      write.table(texto,arq, dec=".",row.names = FALSE,col.names = FALSE,append = TRUE,quote = FALSE)
    }
  }
  
  texto = "Arquivos de saida gerados com sucesso!"
  cat(texto)
  write(texto,file=arq_log,append=TRUE)

