roda_bacia <-function(diretorio,dia_previsao,tempo_regressao,dias_previstos,agrupamento,modelos,alpha,beta,lambdas,observado){

##############################################
#   update April 21, 2020 
#  included by jorge a variable 'observado'
#  a string defining the variable taken as reference 
# in order to perform the Bias removing.    
##############################################    
#=======================================pega  os arquivos de precipita??o prevista==============================================
precs<-lapply(modelos,function(x) read.table(paste0(diretorio,"/",x,".csv"),header=T, dec=",",sep =";"))

#===================================== ordena a lista por ordem de tamanho de horizonte ========================================
num_de_previsoes<-sapply(precs, function(x) ncol(x))
ord<-order(num_de_previsoes,decreasing = TRUE)
precs<-precs[ord]
modelos<-modelos[ord]
num_de_previsoes<-num_de_previsoes[ord]-1
print(num_de_previsoes)                         
                         
num_mod_hor<-rep(0,dias_previstos)
for (i in 1:length(modelos)){for (j in 1:num_de_previsoes[i]){num_mod_hor[j]<-num_mod_hor[j]+1}}
#===================================== checa se ? possivel dividir naquele n?mero de grupamentos================================
tes<- (num_de_previsoes %/% tempo_regressao)
if (sum(tes)>0){stop("nao eh possivel dividir os modelos previstos no agrupamento solicitado")}

#===================================== pega o verificado e formata a data ======================================================
obs<-read.table(paste0(diretorio,"/",observado,".csv"),header=T, dec=",",sep =";")
print(paste0(diretorio,"/",observado,".csv") )                     
precs<-c(precs,list(obs))
if (ncol(precs[[length(precs)]])< max(num_de_previsoes)){stop("formato do arquivo de precipita??o observada inconsistente. Ele necessita ter o mesmo n?mero de colunas do maior modelo diretorio:",diretorio )}
precs<- lapply(precs, function(x) {x[, 1] <- as.Date(x[, 1], '%d/%m/%Y'); x})
#for (i in 1:(length(modelos)-1)){if (length(which(precs[[i]][,1]==dia_previsao))==0){stop (paste0("o modelo " , modelos[i], " n?o possui a data de de previs?o diretorio:",diretorio))}}

#=======================================acha o modelo/observado com o menor historio ===========================================
tt<-lapply(precs,function (x) min(x[,1]))
for(j in 1:length(modelos)){if (is.na(tt[j][1])){tt[[j]]<-NULL}}
maior_data <-max(do.call(c,tt))

#=======================================cria  a lista com as matrizes por horizonte=============================================
precs2<-list()
for (i in 1:dias_previstos){precs2[[i]]<-matrix(NA_real_, nrow =tempo_regressao , ncol = num_mod_hor[i]+1)}
#=======================================preenche a lista com as previs?es=======================================================
for (i in 1:dias_previstos){
  soma<-1
  k=1
  while(soma<=tempo_regressao){
    dia<-dia_previsao -k # dia sendo busco para ver se a regress?o existe
    res<-rep(NA_integer_,num_mod_hor[i]+1)
    for(j in 1:num_mod_hor[i]){if(length(which(precs[[j]][,1]==dia))){res[j]<-which(precs[[j]][,1]==dia)}} # acha a posi??o do dia 
    if(length(which(precs[[num_mod_hor[1]+1]][,1]==dia))){res[num_mod_hor[i]+1]<-which(precs[[num_mod_hor[1]+1]][,1]==dia)}
    if (sum((sapply(res, function(y) y[1]!=-1)),na.rm = TRUE) == length(res)){  # if para ver se a data existe em todos modelos e no observado

#======================================= checa se no horizonte "i" todo mundo tem valor======================================================    
      valor<-rep(NA_real_,num_mod_hor[i]+1)
      for(j in 1:num_mod_hor[i]){valor[j]<-precs[[j]][res[j],i+1]}
      valor[num_mod_hor[i]+1]<-precs[[num_mod_hor[1]+1]][res[num_mod_hor[i]+1],i+1]
      if (sum((sapply(valor, function(y) !is.na(y))),na.rm = TRUE) == length(res)){
#=======================================escreve os valores na matriz da lista precs2=========================================================
        for(j in 1:num_mod_hor[i]){precs2[[i]][soma,j]<-precs[[j]][res[j],i+1]}
        precs2[[i]][soma,num_mod_hor[i]+1]<-precs[[num_mod_hor[1]+1]][res[num_mod_hor[i]+1],i+1]
        soma<-soma +1
      }
    }  
  if (maior_data> dia){stop("N?mero insuficiente de elementos para o tamanho da regress?o solicitada")}
  k=k+1
  }
}
                      
print(precs2[[7]][c(TRUE,FALSE),])                      
#for (j in 1:15){write.table(precs2[[j]],paste0(diretorio,"/matriz_Leg_",j,".csv"), dec=",",sep=";")} imprime as matrizes 

#===================================== Loop para rodar a regress?o========================================================================
ens<-rep(0,dias_previstos)
lambdass<-rep(0,(dias_previstos/agrupamento))
Coefs<-matrix(NA_real_,nrow =dias_previstos,ncol=num_mod_hor[1])
colnames(Coefs)<-modelos[]
for (j in 1:(dias_previstos/agrupamento)){
  l <- precs2[seq(1+((j-1)*agrupamento),(agrupamento+((j-1)*agrupamento)))]
  #l2 <- lapply(l, function(x) x[!x[, ncol(l[[1]])] == 0, ])
   if(ncol(l[[1]])>2){
     l3<-list(lapply(l, function(x) x[c(TRUE,FALSE), ]),lapply(l, function(x) x[c(FALSE,TRUE), ]))
     numero_de_lambdas<-length(lambdas)
     erro_lambda<-matrix(0,numero_de_lambdas,agrupamento)
     for (h in 1:numero_de_lambdas){
       lambda<-lambdas[h]
       for( w in 1:2){
         b<-roda_lp(l3[[w]],alpha,beta,lambda)
         l4<-l3[[1 + (w %% 2)]]
         l5<-lapply(seq_along(l4), function(x) {
           m <- l4[[x]]
           v <- b[x, ]
           m[, -ncol(l4[[1]])] <- m[, -ncol(l4[[1]])] %*% diag(v)
           abs(m[, ncol(l4[[1]])] - apply(m[, -ncol(l4[[1]])], 1, sum))
         })
         erro_lambda[h, ]<- sapply(l5, mean) + erro_lambda[h, ]
       }
     }
     erro<-apply(erro_lambda, 1, mean)
     lambda<-lambdas[which.min(erro)]
   } else {
     lambda<-0 # caso com apenas um modelo
   }
  b<-roda_lp(l,alpha,beta,lambda)
  lambdass[j]<-lambda
  for(k in 1:agrupamento){
    for( w in 1:num_mod_hor[k +(j-1)*agrupamento]){
      ens[k +(j-1)*agrupamento]<-ens[k +(j-1)*agrupamento]+precs[[w]][which(precs[[w]][,1]==dia_previsao),k +1 +(j-1)*agrupamento]*b[k,w]
      Coefs[k +(j-1)*agrupamento,w]<-b[k,w]
    }
  }
}
resultado<-list(ens,Coefs,lambdass)
return(resultado)
}


roda_lp <- function(l,alpha,beta,lambda){
#==========================================================bloco que depois vai virar a função======================================================
library(lpSolve)
num_modelos<-ncol(l[[1]])-1
t_regre_t<-sum(unlist(lapply(l,nrow)))
num_dias<-length(l)
# formatos => x=[w1+,....,w1-,fi1,..fin,] c=[1,...,1,....,0,...,0], u=[y1,0,..,yn,0,y1,0,....,yn,0,...,0,a,-b] mib Ct.x s.a: Ax>=u
# formatos 2 => x
#========================================= Preenche a Matriz A==========================================================================================
A<-matrix(0, nrow = 4*t_regre_t +4 + num_dias*num_modelos + num_dias*2, ncol = 2*t_regre_t +2 + num_modelos*num_dias)
for (i in 1:(2*t_regre_t +2 )){ # preenche com 1 a parte da matriz para  W+ e W-
  A[1+(i-1)*2,1+(i-1)]<-1
  A[2+(i-1)*2,1+(i-1)]<-1
} 
for(i in 1:(num_modelos*num_dias)){A[4*t_regre_t +4 +i,2*t_regre_t +2 + i]<-1} # preenche com 1 a parte dos fis para garantir que eles sejam maiores que 0
t_regre<-0
for(i in 1:num_dias){ # preenche com os valores di?rios de prec prevista
  for(j in 1:num_modelos){
    for(k in 1:(nrow(l[[i]]))){
      A[1 +(k-1)*4+4*t_regre,2*t_regre_t + 2 + j + num_modelos*(i-1)]<-l[[i]][k,j] 
      A[3 +(k-1)*4+4*t_regre,2*t_regre_t + 2 + j + num_modelos*(i-1)]<--l[[i]][k,j]
    }
  }    
  t_regre<-t_regre +nrow(l[[i]])
}

for(i in 1:num_dias){ # preenche com os valores m?dios de prec prevista
  for(j in 1:num_modelos){
    A[1 + 4*t_regre_t,2*t_regre_t + 2 + j +num_modelos*(i-1)]<- mean( l[[i]][ ,j])/num_dias
    A[3 + 4*t_regre_t,2*t_regre_t + 2 + j +num_modelos*(i-1)]<- -mean( l[[i]][ ,j])/num_dias
  }
} 

for(i in 1:num_dias){ # preenche com os fis para o li e ls
  for(j in 1:num_modelos){
    A[4*t_regre_t +4 + num_dias*num_modelos + 1 +(i-1)*2,2*t_regre_t +2 + j+ (i-1)*num_modelos]<- 1 # preenche li
    A[4*t_regre_t +4 + num_dias*num_modelos + 2 +(i-1)*2,2*t_regre_t +2 + j+ (i-1)*num_modelos]<- -1 # preenche ls
  }
}  


#======================================== Preenche o U ===========================================================================================
U<-rep(0,4*t_regre_t +4 + num_dias*num_modelos + num_dias*2)
t_regre<-0
for(i in 1:num_dias){ # preenche com os valores di?rios de prec observada
  for(k in 1:(nrow(l[[i]]))){
    U[1 +(k-1)*4+4*t_regre]<-l[[i]][k,num_modelos + 1] 
    U[3 +(k-1)*4+4*t_regre]<--l[[i]][k,num_modelos +1]
  }
  t_regre<-t_regre +nrow(l[[i]])
}
U[1 + 4*t_regre_t]<- mean(unlist(lapply(l, function(x) mean(x[, num_modelos +1]))))
U[3 + 4*t_regre_t]<- -mean(unlist(lapply(l, function(x) mean(x[, num_modelos +1]))))

for(i in 1:num_dias){ # preenche com os fis para o li e ls
    U[4*t_regre_t +4 + num_dias*num_modelos + 1 +(i-1)*2]<- 0.5 # preenche li
    U[4*t_regre_t +4 + num_dias*num_modelos + 2 +(i-1)*2]<- -2 # preenche ls
}



#=======================================Preenche o OBJ===============================================================================================
OBJ<-rep(lambda,2*t_regre_t +2 + num_modelos*num_dias) # j? preenche com o valor do lambda para o peso dos modelos o resto ser? substituido      
t_regre<-0
for(i in 1:num_dias){ # preenche com os valores di?rios de prec prevista
  for(k in 1:(nrow(l[[i]]))){
    OBJ[1 +(k-1)*2+2*t_regre]<-alpha/(nrow(l[[i]])*num_dias)
    OBJ[2 +(k-1)*2+2*t_regre]<-alpha/(nrow(l[[i]])*num_dias)
  }
  t_regre<-t_regre +nrow(l[[i]])
}
OBJ[2*t_regre_t +1]<-beta
OBJ[2*t_regre_t +2]<-beta

VI<-rep(">=", 4*t_regre_t +4 + num_dias*num_modelos + num_dias*2)
#======== rodar o pl==============
a<-lp("min",OBJ,A,VI,U)$solution
b<-matrix(tail(a, num_dias*num_modelos), num_dias, num_modelos, byrow = T)
return(b)
}

	
