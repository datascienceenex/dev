rm(list=ls())

#library(openair)
library(parallel)

setwd("/home/middle/SMAP_DESK_COURSE/EsquemaSMAP_nuvem/")

source("functions_SMAP_novoVies_parallel.R")

# Load all parameters
load("Parametros.RData")

#datename <- "xtodayx"
date_file <- "20200810"
datename <- date_file

subname <- vertudo$subbacia
bigname <- vertudo$bacia

namesObs <- c("QQueixo","ESPORA", "QM", "Monjolinho", "StaClara", "EDACUNHA", "ESouza", "JordSeg", "FozChapeco", "Machadinho", "CAPESCURO",
              "FOA", "Camargos", "PBUENOS", "CORUMBAIV", "FUNIL MG", "PARAGUACU", "Chavantes", "SDO","SJoao", "SDOFACAO", "RB-SMAP", 
              "Ibitinga","SALTOVERDI","BG", "Ita", "CanoasI", "Flor+Estra", "CN", "Maua", "NOVAPONTE", "Rosana", "PASSAGEM", "NAvanhanda", 
              "Jurumirim", "SCaxias", "EMBORCACAO", "PCOLOMBIA", "MARIMBONDO", "FURNAS", "CORUMBA1", "AVERMELHA", "Balsa", "ITUMBIARA",
              "UVitoria", "Capivara", "BBonita","JUPIA", "SFR2", "RVerde", "Ivinhema", "PTaquara", "TM-SMAP","ILHAEQUIV", "SSIMAO2", "Itaipu", "SMesa","PPRI","SRM2")


folder <- paste("Modelos_Chuva_Vazao_", date_file, sep="")
saidas_bkup <- list()
lag<-1
tstart <- Sys.time()
mainONS <- function(datename, folder, subname, bigname, namesObs, vertudo,lag,i){

  setwd("/home/middle/SMAP_DESK_COURSE/EsquemaSMAP_nuvem/")
  print(subname)
  dir <- paste(folder, "/SMAP/", bigname, "/ARQ_ENTRADA/", sep="")
  modname <- "ECMWF"
  if(length(Sys.glob(paste(dir, "*", sep=""))) > 0){
    
    valoresInicializacao <- readInic(dir, subname)
    datei <- valoresInicializacao$date
    diasWarm <- as.integer(valoresInicializacao$infos[1])
#   diasPrev <- as.integer(valoresInicializacao$infos[2])
    diasPrev <- 9
    ebin <- valoresInicializacao$infos[3]
    supin <- valoresInicializacao$infos[4]
    tuin <- valoresInicializacao$infos[5]
    
    mod <- geraSerieChuvaModel(dir, subname, modname)  
    obs <- geraSerieObservada(dir, subname)   # Observed rainfall serie
     
    # Set the parameters to the especific sub-basin
    param <- vertudo[vertudo$subbacia == subname,-1]
    y <- param[, substr(names(param),1,3) == "kt_"]
    y <- y[!is.na(y)]
    y <- length(y)-3
    
    # Applies the time coefficients and pcoef to the rain series (for optimization)
    df <- geraChuvaSMAP(datei, diasWarm, diasPrev, mod, obs, vertudo, param$pcof, subname, y, lag)
    df_previsao <- df[[2]]
    df <- df[[1]]
    
    # Opens the streamflow observed data
    vaz <- leVazaoObs(dir, subname)
    vaz <- merge(vaz, df, all.y=T)[,c(1,2)]  # set the same time period to optimization
    vaz <- vaz[!is.na(vaz$vazaoObs),]
    
    # Opens the evapotranspiration serie
    evapo <- read.table(paste(dir,subname,"_EVAPOTRANSPIRACAO.txt", sep=""))
    evapo_prev <- evapo[as.numeric(format(df_previsao$date, "%m")),2]
    evapo <- evapo[as.numeric(format(df$date, "%m")),2]
    
    bat_pars <- readBat(dir)
    Lower = c(rep(param$inf_chuva,31), param$inf_ebin*ebin, 0)
    Upper = c(rep(param$sup_chuva,31), param$sup_ebin*ebin, 1000000)
    cal <- bat_optim_leo(D=33, NP=as.numeric(bat_pars[4,2]), N_Gen=5000, A=as.numeric(bat_pars[1,2]), gamma=as.numeric(bat_pars[5,2]), Lower, Upper, tuin,
                         param, df, evapo, vaz$vazaoObs,
                         alvo=as.numeric(bat_pars[2,2]), semente=as.numeric(bat_pars[11,2]), constA=as.numeric(bat_pars[6,2]), 
                         constB=as.numeric(bat_pars[7,2]), ipeso=bat_pars[9,2], tipo=bat_pars[8,2])
    
    
    setwd(dir)
    
    chuvafinal <- rbind(df, df_previsao)
    evapofinal <- c(evapo, evapo_prev)
    pesosfinal <- c(cal[1:31], rep(1,length(df_previsao$date)))
    
    
    qprev <- data.frame(date=chuvafinal$date, vaz=smap(tuin, cal[32], cal[33],
                                                       param$Area,
                                                       param$kkt, param$k1t, param$k2t, param$k2t2, param$k3t,
                                                       param$str, param$crec, param$ai, param$capc,
                                                       param$H, param$H1,
                                                       param$pcof, param$ecof, param$ecof2,
                                                       evapofinal, chuvafinal,
                                                       pesosfinal,
                                                       0))
    
    #qprev <- qprev[,1:2]
    names(qprev)[2] <- subname
    
    return(qprev[,1:2])
    
  }
  
  
}



### paraleliza
numCores <- detectCores()
clust <- makeCluster(numCores, type = 'PSOCK')
clusterExport(clust, varlist = c('mainONS','readInic','aplicaCoefsTempo', 'consertaNomes','smap','geraSerieChuvaModel','geraSerieObservada',
                                 'geraChuvaSMAP','leVazaoObs','funObjetivo','readBat','bat_optim_leo',
                                 'datename', 'folder', 'subname', 'bigname','namesObs','vertudo','lag'), envir = .GlobalEnv)
res<-parLapply(clust,1:length(subname), function(x) mainONS(datename, folder, subname[x], bigname[x], namesObs[x], vertudo,lag,x))


stopCluster(clust)

junta <- Reduce(merge, res)

save(junta, file=paste("/home/middle/SMAP_DESK_COURSE/EsquemaSMAP_nuvem/",datename, "_ECMWF_Simulacoes.RData", sep=""))

