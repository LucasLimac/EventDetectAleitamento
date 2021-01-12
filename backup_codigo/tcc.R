library(dplyr)
library(sqldf)
library(tidyverse)
library(tibbletime)
library(anomalize)
library(tibble)
install.packages("tibble")
install.packages("anomalize")
install.packages("tidyverse")
install.packages("devtools")
install.packages("Rcpp")
library(devtools)
#install_github("twitter/AnomalyDetection")
library(Rcpp)
library(AnomalyDetection)
library(hrbrthemes)
library(ggplot2)
#devtools::install_github("hrbrmstr/AnomalyDetection")

if (!require("devtools")){install.packages("devtools")}

devtools::install_github("config-i1/greybox")

install_version("greybox", version = "0.5.9", repos = "http://cran.us.r-project.org")

remove.packages("greybox")

library(greybox)

packageDescription("greybox")

install.packages("tidyverse")
install.packages("ggplot2")


#install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")

tblCampanhas <- read.csv(file="Credenciamento_IHAC_BLH.csv", header=TRUE, sep=",")
MyData <- read.csv(file="sinasc_sim_taxas.csv", header=TRUE, sep=",")
DataComObitosENasc <- read.csv(file="sinasc_sim_cnes_taxas_joined.csv", header=TRUE, sep=",")


colnames(tblCampanhas)
names(tblCampanhas)[names(tblCampanhas) == "NOME.EMPRESARIAL"] <- "nome_empresarial"
names(tblCampanhas)[names(tblCampanhas) == "Credenciamento.IHAC"] <- "credenciamento_ihac"
names(tblCampanhas)[names(tblCampanhas) == "Nome.do.BLH"] <- "nome_blh"
names(tblCampanhas)[names(tblCampanhas) == "Credenciamento.BLH"] <- "credenciamento_blh"

df <- data.frame(DataComObitosENasc)

camp <- data.frame(tblCampanhas)

#AGORA TRABALHAREMOS COM O BRASIL TODO
#camp <- camp[camp$MUNICIPIO == "RIO DE JANEIRO",]
#df <- df[df$mun_MUNNOME == "Rio de Janeiro",]

mergetabelas <- sqldf("SELECT df.Estab, camp.CNES, camp.UF, df.uf_REGIAO, camp.MUNICIPIO, camp.FANTASIA, max(df.ano_Nascimentos_sum) as max_nasc_anual, df.ano, df.mes, df.tm_neo_mes_cnes,df.Nascimentos, df.neo_Obitos, camp.credenciamento_blh, camp.credenciamento_ihac, cred_canguru.cred_canguru
                   FROM camp
                   JOIN df
                   ON camp.CNES == df.Estab 
                   left JOIN cred_canguru
                   ON camp.CNES == cred_canguru.CNES
                    group by df.Estab")


#mergetabelas <- sqldf("SELECT df.Estab, camp.CNES, camp.UF, df.uf_REGIAO, camp.MUNICIPIO, camp.FANTASIA, max(df.ano_Nascimentos_sum) as max_nasc_anual, df.ano, df.mes, df.tm_neo_mes_cnes,df.Nascimentos, df.neo_Obitos, camp.credenciamento_blh, camp.credenciamento_ihac
#                   FROM camp
#                   JOIN df
#                   WHERE camp.CNES == df.Estab 
#                    group by df.Estab")

hospitaisblabla <- sqldf("SELECT Estab
                                FROM mergetabelas
                                ")

################# HOSPITAIS APTOS PUBLICOS SEM CAMPANHA ######################################
hospitaisAptosPublicosSemCampanha <- sqldf("SELECT Estab, sum(Nascimentos) as soma
                                FROM tb
                                WHERE Estab not in hospitaisblabla
                                           group by Estab
                                           order by soma desc
                                           limit 3
                                           ")


#mergetabelas$anocredenciamento_blh <- substr(mergetabelas$credenciamento_blh, 7, 10 )
#mergetabelas$anocredenciamento_blh <- as.numeric(mergetabelas$credenciamento_blh)

#mergetabelas$credenciamento_blh <- substr(mergetabelas$credenciamento_blh, 7, 10 )
#mergetabelas$credenciamento_blh <- as.numeric(mergetabelas$credenciamento_blh)

mergetabelas$credenciamento_blh <-  as.Date(mergetabelas$credenciamento_blh, "%d/%m/%Y")
mergetabelas$credenciamento_ihac <- as.Date(mergetabelas$credenciamento_ihac, "%d/%m/%Y")
mergetabelas$cred_canguru <- as.Date(mergetabelas$cred_canguru, "%d/%m/%Y")

mergetabelas$credenciamento_blh <-  as.character(mergetabelas$credenciamento_blh)
mergetabelas$credenciamento_ihac <- as.character(mergetabelas$credenciamento_ihac)
mergetabelas$cred_canguru <- as.character(mergetabelas$cred_canguru)

hospitaisAptosBLH <- sqldf("SELECT Estab, CNES, UF, uf_REGIAO, MUNICIPIO, FANTASIA, max_nasc_anual, ano, mes, tm_neo_mes_cnes, credenciamento_blh, credenciamento_ihac
                   FROM mergetabelas
                   WHERE 
                  credenciamento_blh > '2006-01-01' and credenciamento_blh < '2017-12-31'")

hospitaisAptosIHAC <- sqldf("SELECT Estab, CNES, UF, uf_REGIAO, MUNICIPIO, FANTASIA, max_nasc_anual, ano, mes, tm_neo_mes_cnes, credenciamento_blh, credenciamento_ihac
                   FROM mergetabelas
                   WHERE 
                  credenciamento_ihac > '2006-01-01' and credenciamento_ihac < '2017-12-31'")


##TOTAL DE HOSPITAIS APTOS, TIRANDO A INTERSEÇÃO DE HOSPITAIS QUE SAO APTOS TANTO NO IHAC QUANTO NO BLH
hospitaisAptos <- sqldf("SELECT Estab, CNES, UF,uf_REGIAO, MUNICIPIO, FANTASIA, max_nasc_anual, ano, mes, tm_neo_mes_cnes, Nascimentos, neo_Obitos, credenciamento_blh, credenciamento_ihac, cred_canguru
                   FROM mergetabelas
                   WHERE 
                  (credenciamento_blh >= '2006-01-01' and credenciamento_blh <= '2017-12-31') or
                  (credenciamento_ihac >= '2006-01-01' and credenciamento_ihac <= '2017-12-31') or 
                  (cred_canguru >= '2006-01-01' and cred_canguru <= '2017-12-31')")

tb_Estab <- sqldf("SELECT DISTINCT Estab
                  FROM tb")

hospitaisAptosPublicos <- sqldf("SELECT *
                                FROM hospitaisAptos
                                WHERE Estab in tb_Estab")

hospitaisAptosPublicos2 <- sqldf("SELECT Estab
                                FROM hospitaisAptos
                                WHERE Estab in tb_Estab")

##HOSPITAIS POR UF
hospitaisAptosCount <- sqldf("SELECT count(UF) as qtd_estado, Estab, CNES, UF, uf_REGIAO, MUNICIPIO, FANTASIA, max_nasc_anual, ano, mes, tm_neo_mes_cnes, credenciamento_blh, credenciamento_ihac
                   FROM hospitaisAptos
                   group by UF")

colsMUN <- c("blue4", "blue4", "blue4", "blue4", "blue4", "blue4",
             "blue4", "blue4", "blue4", "blue4", "blue4", "blue4")

barplot(hospitaisAptosCount$qtd_estado, main="Quantidade de Estabs Aptos por UF", xlab="UF", ylab="Quantidade de Estabs", names.arg = hospitaisAptosCount$UF, ylim=c(0, 30), cex.names = 0.8,  xaxs = "i", col =colsMUN)
grid(nx=NA, ny=NULL)

# HOSPITAIS POR REGIAO
hospitaisAptosCountREGIAO <- sqldf("SELECT count(uf_REGIAO) as qtd_estado, Estab, CNES, UF, uf_REGIAO, MUNICIPIO, FANTASIA, max_nasc_anual, ano, mes, tm_neo_mes_cnes, credenciamento_blh, credenciamento_ihac
                   FROM hospitaisAptos
                   group by uf_REGIAO")


barplot(hospitaisAptosCountREGIAO$qtd_estado, main="Quantidade de Estabs Aptos por REGIAO", xlab="REGIAO", ylab="Quantidade de Estabs", names.arg = hospitaisAptosCountREGIAO$uf_REGIAO, ylim=c(0, 60), cex.names = 0.8,  xaxs = "i", col =colsMUN)
grid(nx=NA, ny=NULL)


##LER CSV COM HOSPITAIS FILTRADOS PELA FIOCRUZ (TIROU ALGUNS HOSPITAIS)

#estabs_filtrados <- read.csv(file="maternidades_tm_hist.csv", header=TRUE, sep=",")
#estabs <- data.frame(estabs_filtrados)
selectCodigo <- sqldf("SELECT distinct Estab
                   FROM estabs")


##PEGAR OS CNES APTOS E APLICAR OS PACOTES
cnesAptos <- data.frame(hospitaisAptos$Estab)
names(cnesAptos)[names(cnesAptos) == "hospitaisAptos.Estab"] <- "Estab"

##Select para verificar dos 95 cnes quais estão na lista filtrada
selectVerificacao <- sqldf("SELECT *
                   FROM selectCodigo
                   WHERE Estab in cnesAptos")

##Select para verificar quais foram os 5 hospitais eliminados 
##Agora são 90 hospitais para trabalhar
selectHospitaisEliminados <- sqldf("SELECT *
                   FROM cnesAptos
                   WHERE Estab not in selectVerificacao")


#HospitaisEliminados
hospitaisEliminados <- sqldf("SELECT *
                   FROM hospitaisAptos
                   WHERE Estab in selectHospitaisEliminados")

EstabsFinais <- sqldf("SELECT Estab
                   FROM hospitaisAptos
                   WHERE Estab in selectVerificacao")


## ESTABS PUBLICOS

EstabsFinaisPublicos <- sqldf("SELECT *
                   FROM hospitaisAptosPublicos
                   WHERE Estab in selectVerificacao
                   order by max_nasc_anual desc
                   LIMIT 3")

EstabsPublicos <- sqldf("SELECT *
                   FROM tbEstab
                   order by max_nasc_anual desc
                   ")



## Pegar estabs sem campanha

filtrarSemCampanha <- sqldf("SELECT distinct Estab
                            FROM mergetabelas
                            where Estab in tb_Estab")

filtrarSemCampanha2 <- sqldf("SELECT  max(ano_Nascimentos_sum) as max_nasc_anual, Estab, MUNICIPIO, UF, uf_REGIAO
                            FROM estabs
                            WHERE Estab not in (SELECT *
                            FROM filtrarSemCampanha) 
                            GROUP BY Estab
                            ORDER BY max_nasc_anual desc
                            LIMIT 3 ")

Estabs1 <- sqldf("SELECT Estab, UF, FANTASIA, credenciamento_blh, credenciamento_ihac, cred_canguru
                   FROM hospitaisAptosPublicos
                   WHERE Estab in selectVerificacao
                   order by max_nasc_anual desc
                   LIMIT 3")

Estabs2 <- sqldf("SELECT   Estab, max(ano_Nascimentos_sum) as max_nasc_anual, uf_SIGLA_UF, null, null, null
                            FROM estabs
                            WHERE Estab in (SELECT *
                            FROM filtrarSemCampanha) 
                            GROUP BY Estab
                            ORDER BY max_nasc_anual desc
                            LIMIT 3 ")

mergeEstabs <- rbind(Estabs1,Estabs2)               


##########################################################################################################################
####################### TRABALHANDO COM AS CIDs ##########################################################################
##########################################################################################################################

cidP36  <- read.csv(file="maternidades_tm_hist_P36.csv" , header=TRUE, sep=",")
cidA09  <- read.csv(file="maternidades_tm_hist_A09.csv" , header=TRUE, sep=",")
cidP77  <- read.csv(file="maternidades_tm_hist_P77.csv" , header=TRUE, sep=",")
cidP399 <- read.csv(file="maternidades_tm_hist_P399.csv", header=TRUE, sep=",")


cidP36$CID <- "P36"
cidA09$CID <- "A09"
cidP77$CID <- "P77"
cidP399$CID <- "P399"

CIDS <- rbind(cidP36,cidA09,cidP77,cidP399)

save(CIDS,file="data.Rda")
load("data.Rda")


CIDSREGIAO <- sqldf("SELECT  *
                  FROM CIDS
                  GROUP BY uf_SIGLA_UF")

#PREPARA PARA USAR A MEDIA MOVEL DOS ULTIMOS 6 MESES
prep_data_CID <- function(cod){
  
  dado <- CIDS
  taxacnes <- subset(dado,select = c("ano","mes","uf_REGIAO","tm_neo_ma_regiao"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod) #Selecionando o estabalecimento de interesse
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  trend <- a$trend
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}

dataInstavelCIDS <- prep_data_CID("Sudeste") #colocar código do estabelecimento

estab(dataInstavelCIDS[[1]],dataInstavelCIDS[[2]],NULL, NULL, NULL, paste(Estabs1[i,]$FANTASIA, Estabs1[i,]$UF, NULL, sep = " - ")) #colocar data de credenciamento


for (i in 1:nrow(Estabs1)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==1){
    
    break
    
  }
}


prep_data_CID_Nasc <- function(cod){
  
  dado <- CIDS
  taxacnes <- subset(dado,select = c("ano","mes","Estab","Nascimentos"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod) #Selecionando o estabalecimento de interesse
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  trend <- a$trend
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}


for (i in 1:nrow(Estabs1)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==1){
    dataInstavelCIDS <- prep_data_CID_Nasc(Estabs1[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavelCIDS[[1]],dataInstavelCIDS[[2]],Estabs1[i,]$credenciamento_ihac,Estabs1[i,]$credenciamento_blh, Estabs1[i,]$cred_canguru, paste(Estabs1[i,]$FANTASIA, Estabs1[i,]$UF, Estabs1[i,]$uf_REGIAO, sep = " - ")) #colocar data de credenciamento
    
    break
    
  }
}

######################################################################################################
######################################### APLICANDO A FUNCAO #########################################
######################################################################################################

#PREPARA PARA USAR A MEDIA MOVEL DOS ULTIMOS 6 MESES
prep_data4 <- function(cod){
  
  dado <- sinasc_sim_cnes_taxas_joined
  taxacnes <- subset(dado,select = c("ano","mes","Estab","tm_neo_ma_cnes"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod) #Selecionando o estabalecimento de interesse
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  trend <- a$trend
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}

#PREPARA PARA USAR O NUMERO DE NASCIMENOS MENSAL
prep_data2 <- function(cod){
  
  dado <- sinasc_sim_cnes_taxas_joined
  taxacnes <- subset(dado,select = c("ano","mes","Estab","Nascimentos"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod) #Selecionando o estabalecimento de interesse
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  trend <- a$trend
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}

#PREPARA PARA USAR NUMERO DE OBITOS MENSAL
prep_data3 <- function(cod){
  
  dado <- sinasc_sim_cnes_taxas_joined
  taxacnes <- subset(dado,select = c("ano","mes","Estab","neo_Obitos"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod) #Selecionando o estabalecimento de interesse
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  trend <- a$trend
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}


#######################################FINAL#######################################
BLABLA <- subset(maternidades_corrigido, Estab=='434') #Selecionando o estabalecimento de interesse
BLABLA2 <- subset(sinasc_sim_cnes_taxas_joined, Estab=='434') #Selecionando o estabalecimento de interesse

IHAC_BLH_MC_periodo <- subset(IHAC_BLH_MC_periodo, Estab=cod) #S

estabsNA <- subset(IHAC_BLH_MC_periodo, is.na(UF))
estabsNA <- estabsNA$CNES

hosp <-  5714397 
IHAC_BLH_MC_periodo[IHAC_BLH_MC_periodo$CNES==hosp, "UF"] <- 'SE'
IHAC_BLH_MC_periodo[IHAC_BLH_MC_periodo$CNES==hosp, "MUNICIPIO"] <- 'ARACAJU'
IHAC_BLH_MC_periodo[IHAC_BLH_MC_periodo$CNES==hosp, "FANTASIA"] <- 'MATERNIDADE NOSSA SENHORA DE LOURDES'

#TAXA DE MORTALIDADE
prep_data <- function(cod){
  
  dado <- maternidades_corrigido
  taxacnes <- subset(dado,select = c("ano","mes","Estab","tm_neo_mes_cnes"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  taxacnes <- taxacnes[order(taxacnes$data),]
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod) #Selecionando o estabalecimento de interesse
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  trend <- a$trend
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}

an_outliers <- function(x, k) {
  ts.sw <- function(x,k)
  {
    ts.lagPad <- function(x, k) 
    {
      c(rep(NA, k), x)[1 : length(x)] 
    }
    
    n <- length(x)-k+1
    sw <- NULL
    for(c in (k-1):0){
      t  <- ts.lagPad(x,c)
      sw <- cbind(sw,t,deparse.level = 0)
    }
    col <- paste("t",c((k-1):0), sep="")
    rownames(sw) <- NULL
    colnames(sw) <- col
    return (sw)
  }
  
  ts.outliers.boxplot <- function(data, alpha = 1.5)
  {
    org = nrow(data)
    cond <- rep(FALSE, org)
    if (org >= 30) {
      i = ncol(data)
      q = quantile(data[,i], na.rm=TRUE)
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      cond = data[,i] < lq1 | data[,i] > hq3
    }
    return (cond)
  }
  
  sx <- ts.sw(x,12)
  
  ma <- apply(sx, 1, mean)
  
  sxd <- sx - ma
  
  i <- ts.outliers.boxplot(sxd)
  
  return(i)
}




estab <- function(x,hu,dcihac,dcblh,dccanguru, title){
  
  source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
  source("timeseries_Ogasawara.R")
  library("TSPred")
  library("boot")
  library("stringr")
  library("chron")
  
  #Create sliding windows of size
  sw_size <- 13
  data <- data.frame(ts.sw(x, sw_size))
  data <- data[complete.cases(data), ]
  
  #===== Function to analyze each data window ======
  analyze_window <- function(data) {
    #browser()
    n <- length(data)
    y <- as.data.frame(data)
    colnames(y) <- "y"
    y$t <- 1:n
    
    mdl <- lm(y~t, y)
    err <- mean(mdl$residuals^2)
    
    y_a <- y[1:floor(n/2),]
    mdl_a <- lm(y~t, y_a)
    y_d <- y[ceiling(n/2+1):n,]
    mdl_d <- lm(y~t, y_d)
    
    err_ad <- mean(c(mdl_a$residuals,mdl_d$residuals)^2)
    
    #return 1-error on whole window; 2-error on window halves; 3-error difference
    return(data.frame(mdl=err, mdl_ad=err_ad, mdl_dif=err-err_ad))
  }
  
  #===== Analyzing all data windows ====== 
  errors <- do.call(rbind,apply(data,1,analyze_window))
  
  #===== Boxplot analysis of results ======
  outliers.index <- function(data, alpha = 1.5){
    org = length(data)
    
    if (org >= 30) {
      q = quantile(data)
      
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      cond = data < lq1 | data > hq3
      index.cp = which(cond)#data[cond,]
    }
    return (index.cp)
  }
  
  #Returns index of windows with outlier error differences
  index.cp <- outliers.index(errors$mdl_dif)
  
  #===== Plot detected events/outliers ======
  #Adjusting window index to time series data
  index.cp.x <- index.cp+floor(sw_size/2)
  
  
  numbers_to_cluster <- index.cp.x
  distances_between_numbers <- dist(x = numbers_to_cluster, method = "euclidean")
  hierarchical_clustering <- hclust(d = distances_between_numbers, method = "single")
  clusters <- cutree(tree = hierarchical_clustering, h = 1)
  
  intt <- vector()
  int <- vector()
  
  for (i in 1:length(unique(clusters))){
    intt[i] <- head(which(clusters == i),1)
    int[i] <- index.cp.x[as.numeric(intt[i])]
  }
  
  
  print(title)
  df <- data.frame()
  changePoint<- data.frame(CHANGEPOINT=hu$data[int])
  
  
  for(i in 1:nrow(changePoint)){
    if(!is.na(dcihac)){
      linha <- data.frame(CNES=title, IHAC=as.character(dcihac), BLH=NA,CANGURU=NA,CHANGEPOINT=as.character(changePoint[i,]))
      df <- rbind(df, linha)
    }
    if(!is.na(dcblh)){
      linha <- data.frame(CNES=title, IHAC=NA, BLH=as.character(dcblh),CANGURU=NA,CHANGEPOINT=as.character(changePoint[i,]))
      df<-rbind(df,linha)
    }
    if(!is.na(dccanguru)){
      linha <- data.frame(CNES=title, IHAC=NA, BLH=NA,CANGURU=as.character(dccanguru),CHANGEPOINT=as.character(changePoint[i,]))
      df <- rbind(df, linha)
    }
  }
  
  return(df)
  
}
estabx <- function(x,hu,dcihac,dcblh,dccanguru, title){
  
  source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
  source("timeseries_Ogasawara.R")
  library("TSPred")
  library("boot")
  library("stringr")
  library("chron")
  
  
  #Create sliding windows of 
  
  #COM A JANELA tamanho 10, todos conseguem plotar
  #sw_size <-10
  sw_size <- 13
  data <- data.frame(ts.sw(x, sw_size))
  data <- data[complete.cases(data), ]
  
  #===== Function to analyze each data window ======
  analyze_window <- function(data) {
    #browser()
    n <- length(data)
    y <- as.data.frame(data)
    colnames(y) <- "y"
    y$t <- 1:n
    
    mdl <- lm(y~t, y)
    err <- mean(mdl$residuals^2)
    
    y_a <- y[1:floor(n/2),]
    mdl_a <- lm(y~t, y_a)
    y_d <- y[ceiling(n/2+1):n,]
    mdl_d <- lm(y~t, y_d)
    
    err_ad <- mean(c(mdl_a$residuals,mdl_d$residuals)^2)
    
    #return 1-error on whole window; 2-error on window halves; 3-error difference
    return(data.frame(mdl=err, mdl_ad=err_ad, mdl_dif=err-err_ad))
  }
  
  #===== Analyzing all data windows ====== 
  errors <- do.call(rbind,apply(data,1,analyze_window))
  #print(paste("ERRORS",errors))
  
  #===== Boxplot analysis of results ======
  outliers.index <- function(data, alpha = 1.5){
    org = length(data)
    
    if (org >= 30) {
      q = quantile(data)
      
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      cond = data < lq1 | data > hq3
      index.cp = which(cond)#data[cond,]
    }
    return (index.cp)
  }
  
  #Returns index of windows with outlier error differences
  index.cp <- outliers.index(errors$mdl_dif)
  print(paste("INDEX.CP", index.cp))
  
  #print(paste("ERRORS",errors$mdl_dif))
  
  #===== Plot detected events/outliers ======
  #Adjusting window index to time series data
  index.cp.x <- index.cp+floor(sw_size/2)
  
  numbers_to_cluster <- index.cp.x
  #print(paste("NUMBERS TO CLUSTER",numbers_to_cluster))
  distances_between_numbers <- dist(x = numbers_to_cluster, method = "euclidean")
  #print(paste("DISTANCIA BETWEEN NUMBERS",distances_between_numbers))
  hierarchical_clustering <- hclust(d = distances_between_numbers, method = "single")
  #print(paste("HIERARQUICAL CLUSTERING",hierarchical_clustering))
  clusters <- cutree(tree = hierarchical_clustering, h = 1)
  #print(clusters)
  intt <- vector()
  int <- vector()
  
  for (i in 1:length(unique(clusters))){
    intt[i] <- head(which(clusters == i),1)
    int[i] <- index.cp.x[as.numeric(intt[i])]
  }
  
  
  #plot(hu$data, hu$tx, type="l", main = title, xlab="", ylab="TMN", ylim=c(0,50))
  plot(hu$data, hu$tx, type="l", xlab="", ylab="TMN", ylim=c(0,50),axes = TRUE, box = FALSE)
  
  
  #ggplot(hu, aes(x=data, y=tx)) + geom_line() + labs(title=title, x="", y="TMN")
  
  
  valores<-which(hu$tx>50)
  print(valores)
  if(max(hu$tx)>50){
    for (i in 0:length(valores)+1){
      indice <- which(hu$tx==hu$tx[valores[i]])
      #geom_text(aes(x=hu$data[indice], y=50, label=round(hu$tx[valores[i]])), check_overlap = TRUE)
      text(x=hu$data[indice], y=50, cex = 0.80, label=round(hu$tx[valores[i]]), col="green4")
    }
  }
  print(hu$data[int])
  abline(v=hu$data[int], col="red")
  print(title)
  
  
  if(!is.null(dcihac)){
    print(paste("IHAC",as.Date(dcihac)))
    abline(v=as.Date(dcihac), col="blue")
  }
  if(!is.null(dcblh)){
    print(paste("BLH",as.Date(dcblh)))
    abline(v=as.Date(dcblh), col="green")
  }
  if(!is.null(dccanguru)){
    print(paste("CANGURU",as.Date(dccanguru)))
    abline(v=as.Date(dccanguru), col="#FF9933")
  }
  
  
  df <- data.frame(intervalmin="", intervalmax="", var="", coefang="", coeflin="", estabilidade="", stringsAsFactors=FALSE)
  
  
  for (i in 0:length(int)+1){
    if (i==1){
      df[1,1:2]  <- seq(0, int[1], length.out = 2)
    }else if (i==length(int)+1){
      df[length(int)+1,1:2] <- seq(int[i-1]+1, 240, length.out = 2)
    }else{
      df[i,1:2] <- seq(int[i-1]+1, int[i], length.out = 2)}
  }
  
  #calcular variancia de cada segmento                 
  for (i in 1:nrow(df)){
    df[i,3]  <- var(hu$tx[as.numeric(df[i,1]):as.numeric(df[i,2])])
  }
  
  #função de regressão
  mylm <- function(y) {
    xy <- data.frame(x=1:length(y), y=y)
    a <- lm(y~x, xy)
    return(a)
  }
  
  #calcular coef angular de cada segmento                 
  for (i in 1:nrow(df)){
    a <- mylm(hu$tx[as.numeric(df[i,1]):as.numeric(df[i,2])])
    df[i,4] <- as.data.frame(a["coefficients"])[2,1]
    df[i,5] <- as.data.frame(a["coefficients"])[1,1]
  }
  
  foo <- function(data, indices){
    indices <- sort(unique(indices))
    dt<-data[indices,]
    
    a <- mylm(dt)
    c(
      as.data.frame(a["coefficients"])[2,1],
      0
    )
  }
  
  for (i in 1:nrow(df)){
    interval <- as.numeric(df[i,1]):as.numeric(df[i,2])
    set.seed(12)
    myBootstrap <- boot(as.data.frame(hu$tx[interval]), foo, R=5)
    a <- t.test(myBootstrap$t[,1], mu=0)
    if (a$p.value >= 0.05) {
      df[i,6] <- (str_c("estável:", a$p.value))
      sub <- hu[as.numeric(df[i,1]):as.numeric(df[i,2]),]
      #lines(sub$data, sub$tx, col="green",lwd = 2)
    } else {
      df[i,6] <- (str_c("instável:", a$p.value))
    }
  }
  return(df)
}

IHAC_BLH_MC_periodofinal <- IHAC_BLH_MC_periodo[-c(23,52,61,62,63,64,107,108,109,110,126,127,128,129,146,147,148,154,155,156),]


tabelaComDatas <- data.frame()
##indice 75, 80, 83 ta zoado
for (i in 176:nrow(IHAC_BLH_MC_periodo)){
  #controla o estabelecimento sendo o 1 o numero 1..
  #if(i==1){
  dataInstavel <- prep_data(IHAC_BLH_MC_periodo[i,]$CNES) #colocar código do estabelecimento
  tabelaComDatas <- rbind(tabelaComDatas,estab(dataInstavel[[1]],dataInstavel[[2]],IHAC_BLH_MC_periodo[i,]$cred_IHAC,IHAC_BLH_MC_periodo[i,]$cred_BLH, IHAC_BLH_MC_periodo[i,]$cred_MC, IHAC_BLH_MC_periodo[i,]$CNES)) #colocar data de credenciamento
  #}
}

#########################################
##### CODIGO DA API ######################
##########################################
prep_data_api <- function(cod){
  library("dplyr")
  library("anomalize")
  
  dado <- maternidades_corrigido
  #geral
  taxacnes <- subset(dado,select = c("ano","mes","Estab","tm_neo_mes_cnes"))
  #precoce
  #taxacnes <- subset(dado,select = c("ano","mes","Estab","tm_neo_pre_mes_cnes"))
  #tardio
  #taxacnes <- subset(dado,select = c("ano","mes","Estab","tm_neo_tar_mes_cnes"))
  
  #criando nova coluna ano/mes
  taxacnes$data <- paste(taxacnes$ano,formatC(taxacnes$mes, width = 2, format = "d", flag = "0"),"01",sep="-")
  taxacnes$data <- as.Date(taxacnes$data)
  taxacnes$ano <- NULL
  taxacnes$mes <- NULL
  
  taxacnes <- taxacnes[order(taxacnes$data),]
  
  names(taxacnes) <- c("Estab","tx","data")
  
  taxacnes <- subset(taxacnes, Estab==cod)
  
  test <- unique(taxacnes)
  test <- as.tbl(test)
  
  a <-test %>% 
    time_decompose(tx, method = "stl", frequency = "12 months", trend = "60 months") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  
  trend <- a[c(1,4)]
  names(trend) <- c("data","tx")
  taxa <- test[c(3,2)]
  
  list <- list(trend,taxa)
  return(list)
}

##CODIGO FINAL
estab_api <- function(x,hu,dcihac,dcblh,dccanguru, title){
  dev.off()
  
  events_cp_v1 <- evtdet.changepoints_v1(x, w=13,na.action=na.omit)
  
  a <- append(diff(events_cp_v1$time), 100, after = 0)
  evt <- events_cp_v1[a>35,][,1]
  
  plot(hu$data, hu$tx, type="l", xlab="", ylab="TMN", ylim=c(0,50), axes = FALSE)
  
  #escrever a taxa do lado do plot
  axis(side=2, at=c(0,5,10,15,20,25,30,35,40,45,50), cex.axis=1)
  mtext("TMN", side = 50, line=2.5, at=0)
  
  #escrever as datas embaixo do plot
  axis.Date(1, at=seq(min(hu$data), max(hu$data), length.out=10), 
            format='%b %Y', las=2, cex.axis=1)
  
  
  #adicionar uma margem  superior para botar o texto dos outliers
  par(mar = c (0,0,2,0))
  
  #PEGAR OS OUTLIERS
  
  t <- ts.an.outliers.boxplot(hu$tx, 12, alpha=3)
  
  
  print(hu$tx[t])
  
  outliers <- hu$tx[t]
  print(outliers)
  
  
  valores<-which(hu$tx>50)
  if(max(hu$tx)>50){
    for (i in 0:length(valores)+1){
      indice <- which(hu$tx==hu$tx[valores[i]])
      #browser()
      if(!is.na(hu$tx[indice]) && !is.null(hu$tx[indice]) && length(hu$tx[indice]) > 0 && hu$tx[indice] %in% outliers){
        text(x=hu$data[indice], y=50, pos=3, offset=1.6, cex = 1, label=round(hu$tx[valores[i]]), col="red")
        next
      }else{
        text(x=hu$data[indice], y=50, pos=3, offset=1.6, cex = 1, label=round(hu$tx[valores[i]]), col="black")
        next
      }
    }
  }
  
  print(evt)
  abline(v=evt, col="red")
  print(title)
  
  
  if(!is.null(dcihac)){
    print(paste("IHAC",as.Date(dcihac)))
    abline(v=as.Date(dcihac), col="blue")
  }
  if(!is.null(dcblh)){
    print(paste("BLH",as.Date(dcblh)))
    abline(v=as.Date(dcblh), col="green")
  }
  if(!is.null(dccanguru)){
    print(paste("CANGURU",as.Date(dccanguru)))
    abline(v=as.Date(dccanguru), col="#FF9933")
  }
  
  #taxa geral
  dev.copy(png,filename=paste("plotsmenores/",title,".png",sep=""),width=600,height=315)
  
  #taxa precoce
  #dev.copy(png,filename=paste("plots_precoce/",title,".png",sep=""),width=600,height=315)
  
  #taxa tardio
  #dev.copy(png,filename=paste("plots_tardio/",title,".png",sep=""),width=600,height=315)
  
  dev.off ()
}


estab_api_regressao_linear <- function(x,hu,dcihac,dcblh,dccanguru, title){
  dev.off()
  
  events_cp_v1 <- evtdet.changepoints_v1(x, w=13,na.action=na.omit)
  
  a <- append(diff(events_cp_v1$time), 100, after = 0)
  evt <- events_cp_v1[a>35,][,1]
  print(evt)
  ##REGRESSAO LINEAR PARTICIONADA
  ##FAZER SUBSET DE ACORDO COM A QTD DE CHANGEPOINTS
  #subset1 <- filter(hu, data >= "2006-01-01", data <= "2014-11-01")
  #subset2 <- filter(hu, data >= "2014-11-01", data <= "2017-12-01")
  #subset3 <- filter(hu, data >= "2016-03-01", data <= "2017-12-01")
  # subset4 <- filter(hu, data >= "2014-01-01", data <= "2017-12-01")
  #subset5 <- filter(hu, data >= "2015-05-01", data <= "2017-12-01")
  
  #browser()
  
  
  
  
  #AUMENTAR A LINHA DE ACORDO COM QTD DE CHANGEPOINTS
  # line1 <-lm(subset1$tx ~ subset1$data)
  #line2 <- lm(subset2$tx ~ subset2$data)
  #line3 <- lm(subset3$tx ~ subset3$data)
  #line4 <- lm(subset4$tx ~ subset4$data)
  #line5 <- lm(subset5$tx ~ subset5$data)
  
  #AUMENTAR A LINHA DE ACORDO COM QTD DE CHANGEPOINTS
  # p1 <- predict(line1, hu)
  # p2 <- predict(line2, hu)
  # p3 <- predict(line3, hu)
  #p4 <- predict(line4, hu)
  # p5 <- predict(line5, hu)
  
  plot(hu$data, hu$tx, type="l", xlab="", ylab="TMN", ylim=c(0,50), axes = FALSE)
  
  #AUMENTAR A LINHA DE ACORDO COM QTD DE CHANGEPOINTS
  # lines(x=subset1$data, y=p1,  col="darkslateblue")
  # lines(x=subset2$data, y=p2,  col="darkslateblue")
  #lines(x=subset3$data, y=p3,  col="darkslateblue")
  #lines(x=subset4$data, y=p4,  col="darkslateblue")
  #lines(x=subset5$data, y=p5,  col="darkslateblue")
  
  #escrever a taxa do lado do plot
  axis(side=2, at=c(0,5,10,15,20,25,30,35,40,45,50), cex.axis=1)
  mtext("TMN", side = 50, line=2.5, at=0)
  
  #escrever as datas embaixo do plot
  axis.Date(1, at=seq(min(hu$data), max(hu$data), length.out=10), 
            format='%b %Y', las=2, cex.axis=1)
  
  
  #adicionar uma margem  superior para botar o texto dos outliers
  par(mar = c (0,0,2,0))
  
  #PEGAR OS OUTLIERS
  
  t <- ts.an.outliers.boxplot(hu$tx, 12, alpha=3)
  
  
  print(hu$tx[t])
  
  outliers <- hu$tx[t]
  print(outliers)
  
  
  valores<-which(hu$tx>50)
  if(max(hu$tx)>50){
    for (i in 0:length(valores)+1){
      indice <- which(hu$tx==hu$tx[valores[i]])
      #browser()
      if(!is.na(hu$tx[indice]) && !is.null(hu$tx[indice]) && length(hu$tx[indice]) > 0 && hu$tx[indice] %in% outliers){
        text(x=hu$data[indice], y=50, pos=3, offset=1.6, cex = 1, label=round(hu$tx[valores[i]]), col="red")
        next
      }else{
        text(x=hu$data[indice], y=50, pos=3, offset=1.6, cex = 1, label=round(hu$tx[valores[i]]), col="black")
        next
      }
    }
  }
  
  browser()
  print(evt)
  abline(v=evt, col="red")
  print(title)
  
  # 
  # if(!is.null(dcihac)){
  #   print(paste("IHAC",as.Date(dcihac)))
  #   abline(v=as.Date(dcihac), col="blue")
  # }
  # if(!is.null(dcblh)){
  #   print(paste("BLH",as.Date(dcblh)))
  #   abline(v=as.Date(dcblh), col="green")
  # }
  # if(!is.null(dccanguru)){
  #   print(paste("CANGURU",as.Date(dccanguru)))
  #   abline(v=as.Date(dccanguru), col="#FF9933")
  # }
  # 
  #taxa geral
  #dev.copy(png,filename=paste("plotsmenores/",title,".png",sep=""),width=600,height=315)
  
  #taxa precoce
  #dev.copy(png,filename=paste("plots_precoce/",title,".png",sep=""),width=600,height=315)
  
  #taxa tardio
  #dev.copy(png,filename=paste("plots_tardio/",title,".png",sep=""),width=600,height=315)
  
  #dev.off ()
}


dev.set(dev.next())
dev.set(dev.next())


#REGRESSAO LINEAR PLOT
for (i in 1:nrow(IHAC_BLH_MC_periodo)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==1){
    print(IHAC_BLH_MC_periodo[i,]$CNES)
    
    dataInstavel <- prep_data_api(IHAC_BLH_MC_periodo[i,]$CNES)
    
    estab_api_regressao_linear(dataInstavel[[1]],dataInstavel[[1]],IHAC_BLH_MC_periodo[i,]$cred_IHAC,IHAC_BLH_MC_periodo[i,]$cred_BLH, IHAC_BLH_MC_periodo[i,]$cred_MC, paste(IHAC_BLH_MC_periodo[i,]$CNES,IHAC_BLH_MC_periodo[i,]$FANTASIA, IHAC_BLH_MC_periodo[i,]$UF, IHAC_BLH_MC_periodo[i,]$MUNICIPIO, sep = "_"))
    
    #break
    
  }
  
}

#ERROS: 22 (ficou sem imagem, cnes-27049), 52, 114, 136, 138,160,161,173,175,176
#PLOT COM A API
for (i in 1:nrow(IHAC_BLH_MC_periodo)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==4){
    print(IHAC_BLH_MC_periodo[i,]$CNES)
    #print(length(maternidades_corrigido$Estab[maternidades_corrigido$Estab==IHAC_BLH_MC_periodo[i,]$CNES]))
    
    dataInstavel <- prep_data_api(IHAC_BLH_MC_periodo[i,]$CNES)
    
    #dataInstavel <- prep_data_api(2395037)
    estab_api(dataInstavel[[2]],dataInstavel[[2]],IHAC_BLH_MC_periodo[i,]$cred_IHAC,IHAC_BLH_MC_periodo[i,]$cred_BLH, IHAC_BLH_MC_periodo[i,]$cred_MC, paste(IHAC_BLH_MC_periodo[i,]$CNES,IHAC_BLH_MC_periodo[i,]$FANTASIA, IHAC_BLH_MC_periodo[i,]$UF, IHAC_BLH_MC_periodo[i,]$MUNICIPIO, sep = "_"))
    
    #break
    
  }
  
}



lm(2,5)


###GRAFICO PIZZA##########
##qtd de estabs p/regiao

#12-24 meses BLH
#slices <- c(0.4, 0.444444444, 0.333333333, 0.6 , 0.384615385)
slices <- c(8, 4, 2, 3, 5)

#IHAC (12-24 meses): 
#slices <- c(0.368421053, 0.4, 0, 0.5, 0.285714286)
#slices <- c(7, 2, 0, 4, 2)

#Canguru (12-24):
#slices <- c(0.285714286, 0.25, 0.5, 0.2 , 0.208333333)
#slices <- c(12, 3, 3, 1, 5)

#IHAC (1-6 meses): 
#slices <- c(0.157894737, 0.2, 0.333333333, 0.125 , 0.285714286)
#slices <- c(3, 1, 1, 1, 2)

#Canguru(1-6 meses)
#slices <- c(0.19047619, 0.166666667, 0, 0.4 , 0.125)
#slices <- c(8, 2, 0, 2, 3)

#BLH (1-6 meses): 
#slices <- c(0.15, 0.222222222, 0.666666667, 0.6 , 0.076923077)
#slices <- c(3, 2, 4, 3, 1)

colors = c("darkgreen","orange","brown","goldenrod4","dodgerblue4")
#regioes
lbls <- c("Sudeste", "Sul", "Centro-Oeste", "Norte", "Nordeste")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=colors,
    main="BLH - Quantidade de estabs 12-24 meses por região")

####################################

# barchart with added parameters
regions <-c("Sudeste", "Sul", "Centro-Oeste", "Norte", "Nordeste")


#slices <- c(8, 4, 2, 3, 5)
pct <- round(slices/sum(slices)*100)
barplot(pct,
        main = "BLH - Quantidade de estabs 1-6 meses por região",
        xlab = "Porcentagem",
        ylab = "Regiões",
        col = colors,
        horiz = TRUE,
        xlim=c(0,50))
legend("topright", regions, cex = 1.3, fill = colors)

# barchart with added parameters
#slices <- c(8, 4, 2, 3, 5)
pct <- round(slices/sum(slices)*100)
barplot(pct,
        main = "BLH - Quantidade de estabs 1-6 meses por região",
        names.arg = regions,
        xlab = "Porcentagem",
        ylab = "Regiões",
        col = colors,
        ylim=c(0,50))
legend("topright", regions, cex = 1.3, fill = colors)


##########################################
# erroMasPlota: 38, 47, 91, 109, 115, 135, 140, 152, 154
# erros: 23, 52, 64, 114, 136, 138, 160, 161, 173, 174, 175, 176, 177
# dataLimite: 33, 45, 122, 127, 141, 168, 169, 170
index<-1


#PLOT WITH CHANGEPOINT/TCAM
for (i in 1:nrow(IHAC_BLH_MC_periodo)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==2){
    print(IHAC_BLH_MC_periodo[i,]$CNES)
    #print(length(maternidades_corrigido$Estab[maternidades_corrigido$Estab==IHAC_BLH_MC_periodo[i,]$CNES]))
    dataInstavel <- prep_data(IHAC_BLH_MC_periodo[i,]$CNES) #colocar código do estabelecimento
    estabx(dataInstavel[[1]],dataInstavel[[2]],IHAC_BLH_MC_periodo[i,]$cred_IHAC,IHAC_BLH_MC_periodo[i,]$cred_BLH, IHAC_BLH_MC_periodo[i,]$cred_MC, paste(IHAC_BLH_MC_periodo[i,]$CNES,IHAC_BLH_MC_periodo[i,]$FANTASIA, IHAC_BLH_MC_periodo[i,]$UF, IHAC_BLH_MC_periodo[i,]$MUNICIPIO, sep = " - ")) #colocar data de credenciamento
    break
    
  }
  
}



#PLOT DOS OUTLIERS
for (i in 1:nrow(IHAC_BLH_MC_periodo)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==index){
    dataInstavel <- prep_data(IHAC_BLH_MC_periodo[i,]$CNES) #colocar código do estabelecimento
    have_outliers <- an_outliers(dataInstavel[[2]][["tx"]],12)
    dfWithOutliers <- data.frame(data=dataInstavel[[2]][["data"]], tx=dataInstavel[[2]][["tx"]], isoutlier=have_outliers)
    plot(dfWithOutliers$data, dfWithOutliers$tx, type="l", main = paste(IHAC_BLH_MC_periodo[i,]$CNES,IHAC_BLH_MC_periodo[i,]$FANTASIA, IHAC_BLH_MC_periodo[i,]$UF, IHAC_BLH_MC_periodo[i,]$MUNICIPIO, sep = " - "), xlab="", ylab="TMN")
    points(dfWithOutliers$data[which(dfWithOutliers$isoutlier==TRUE)], dfWithOutliers$tx[which(dfWithOutliers$isoutlier==TRUE)], pch = 1, col="red")
    break
    
  }
  
}

##################################################




estabSem <- function(x,hu,title){
  
  source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
  source("timeseries_Ogasawara.R")
  library("TSPred")
  library("boot")
  library("stringr")
  library("chron")
  
  #Create sliding windows of size
  sw_size <- 13
  data <- data.frame(ts.sw(x, sw_size))
  data <- data[complete.cases(data), ]
  
  #===== Function to analyze each data window ======
  analyze_window <- function(data) {
    #browser()
    n <- length(data)
    y <- as.data.frame(data)
    colnames(y) <- "y"
    y$t <- 1:n
    
    mdl <- lm(y~t, y)
    err <- mean(mdl$residuals^2)
    
    y_a <- y[1:floor(n/2),]
    mdl_a <- lm(y~t, y_a)
    y_d <- y[ceiling(n/2+1):n,]
    mdl_d <- lm(y~t, y_d)
    
    err_ad <- mean(c(mdl_a$residuals,mdl_d$residuals)^2)
    
    #return 1-error on whole window; 2-error on window halves; 3-error difference
    return(data.frame(mdl=err, mdl_ad=err_ad, mdl_dif=err-err_ad))
  }
  
  #===== Analyzing all data windows ====== 
  errors <- do.call(rbind,apply(data,1,analyze_window))
  
  #===== Boxplot analysis of results ======
  outliers.index <- function(data, alpha = 1.5){
    org = length(data)
    
    if (org >= 30) {
      q = quantile(data)
      
      IQR = q[4] - q[2]
      lq1 = q[2] - alpha*IQR
      hq3 = q[4] + alpha*IQR
      cond = data < lq1 | data > hq3
      index.cp = which(cond)#data[cond,]
    }
    return (index.cp)
  }
  
  #Returns index of windows with outlier error differences
  index.cp <- outliers.index(errors$mdl_dif)
  
  #===== Plot detected events/outliers ======
  #Adjusting window index to time series data
  index.cp.x <- index.cp+floor(sw_size/2)
  
  
  numbers_to_cluster <- index.cp.x
  distances_between_numbers <- dist(x = numbers_to_cluster, method = "euclidean")
  hierarchical_clustering <- hclust(d = distances_between_numbers, method = "single")
  clusters <- cutree(tree = hierarchical_clustering, h = 1)
  
  intt <- vector()
  int <- vector()
  
  for (i in 1:length(unique(clusters))){
    intt[i] <- head(which(clusters == i),1)
    int[i] <- index.cp.x[as.numeric(intt[i])]
  }
  
  
  print(title)
  df <- data.frame()
  changePoint<- data.frame(CHANGEPOINT=hu$data[int])
  
  print(nrow(changePoint))
  return(nrow(changePoint))
  
}
changePoints<-0
###SEM CAMPANHA
qtdError<-0
for (i in 1:nrow(IHAC_BLH_MC)){
  #controla o estabelecimento sendo o 1 o numero 1..
  skip_to_next <- FALSE
  print(IHAC_BLH_MC[i,]$CNES)
  tryCatch(
    dataInstavel <- prep_data(IHAC_BLH_MC[i,]$CNES) #colocar código do estabelecimento
    ,error = function(e) { skip_to_next <<- TRUE
    qtdError<<-qtdError+1}  )
  tryCatch(
    changePoints <- changePoints + estabSem(dataInstavel[[1]],dataInstavel[[2]],IHAC_BLH_MC[i,]$CNES) #colocar data de credenciamento
    , error =function(e) { skip_to_next <<- TRUE
    qtdError<<-qtdError+1})
  if(skip_to_next) { next }  
}

write.csv(tabelaComDatas,"tabelComDatas.csv", row.names = FALSE)

############ Utilizando estabs sem campanha ################

matdis <- distinct(maternidades_corrigido, Estab, .keep_all = TRUE)

semCampanha <- sqldf("SELECT matdis.Estab
                     FROM matdis
                     WHERE NOT EXISTS
                     (SELECT a.CNES
                     FROM IHAC_BLH_MC as a
                     WHERE matdis.Estab == a.CNES)
                     ORDER BY matdis.Estab ASC")


semCampanhaTx <- sqldf("SELECT m.ano, m.mes, m.Estab, m.tm_neo_mes_cnes
                     FROM maternidades_corrigido as m
                     WHERE NOT EXISTS
                     (SELECT a.CNES
                     FROM IHAC_BLH_MC_periodo as a
                     WHERE m.Estab == a.CNES)
                     ORDER BY m.Estab ASC")

######################### FINAL ##############################################

EstabTeste <-sqldf("SELECT *
                   FROM EstabsFinais
                   order by Estab
                   ")

EstabComCampanhaRJ <-sqldf("SELECT *
                           FROM EstabsFinais
                           WHERE UF = 'RJ'
                           order by max_nasc_anual desc
                           LIMIT 3")






##USAR PARA PLOTAR GRAFICO DE NASCIMENTOS MENSAL DE UM ESTAB
for (i in 1:nrow(EstabsFinaisPublicos)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==3){
    dataInstavel2 <- prep_data2(EstabsFinaisPublicos[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavel2[[1]],dataInstavel2[[2]],EstabsFinaisPublicos[i,]$credenciamento_ihac,EstabsFinaisPublicos[i,]$credenciamento_blh, EstabsFinaisPublicos[i,]$cred_canguru, paste(EstabsFinaisPublicos[i,]$FANTASIA, EstabsFinaisPublicos[i,]$UF, EstabsFinaisPublicos[i,]$uf_REGIAO, sep = " - ")) #colocar data de credenciamento
    
    break
    
  }
}

##USAR PARA PLOTAR GRAFICO DE NASCIMENTOS MENSAL DE UM ESTAB SEM CAMPANHA
for (i in 1:nrow(hospitaisAptosPublicosSemCampanha)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==3){
    dataInstavel2 <- prep_data2(hospitaisAptosPublicosSemCampanha[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavel2[[1]],dataInstavel2[[2]],NULL,NULL, NULL, paste(NULL, NULL, NULL, sep = " - ")) #colocar data de credenciamento
    
    break
    
  }
}

##USAR PARA PLOTAR GRAFICO DE OBITOS MENSAL DE UM ESTAB
for (i in 1:nrow(EstabsFinaisPublicos)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==3){
    dataInstavel3 <- prep_data3(EstabsFinaisPublicos[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavel3[[1]],dataInstavel3[[2]],EstabsFinaisPublicos[i,]$credenciamento_ihac,EstabsFinaisPublicos[i,]$credenciamento_blh, EstabsFinaisPublicos[i,]$cred_canguru,  paste(EstabsFinaisPublicos[i,]$FANTASIA, EstabsFinaisPublicos[i,]$UF, EstabsFinaisPublicos[i,]$uf_REGIAO, sep = " - ")) #colocar data de credenciamento
    
    break
    
  }
}

##USAR PARA PLOTAR GRAFICO DE OBITOS MENSAL DE UM ESTAB SEM CAMPANHA
for (i in 1:nrow(hospitaisAptosPublicosSemCampanha)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==3){
    dataInstavel3 <- prep_data3(hospitaisAptosPublicosSemCampanha[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavel3[[1]],dataInstavel3[[2]],NULL,NULL, NULL, NULL) #colocar data de credenciamento
    
    break
    
  }
}

##USAR PARA PLOTAR GRAFICO DE MEDIA MOVEL DOS ULTIMOS 6 MESES DE UM ESTAB
for (i in 1:nrow(EstabsFinaisPublicos)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==3){
    dataInstavel4 <- prep_data4(EstabsFinaisPublicos[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavel4[[1]],dataInstavel4[[2]],EstabsFinaisPublicos[i,]$credenciamento_ihac,EstabsFinaisPublicos[i,]$credenciamento_blh, EstabsFinaisPublicos[i,]$cred_canguru, paste(EstabsFinaisPublicos[i,]$FANTASIA, EstabsFinaisPublicos[i,]$UF, EstabsFinaisPublicos[i,]$uf_REGIAO, sep = " - ")) #colocar data de credenciamento
    
    break
    
  }
}

##USAR PARA PLOTAR GRAFICO DE MEDIA MOVEL DOS ULTIMOS 6 MESES DE UM ESTAB SEM CAMPANHA
for (i in 1:nrow(semCampanha)){
  #controla o estabelecimento sendo o 1 o numero 1..
  if(i==1){
    dataInstavel4 <- prep_data4(semCampanha[i,]$Estab) #colocar código do estabelecimento
    estab(dataInstavel4[[1]],dataInstavel4[[2]],NULL,NULL, NULL, NULL) #colocar data de credenciamento
    
    break
    
  }
}



##############################################################################################################################################

#1  - Escolher um municipio para trabalhar (Escolher o que tem mais mortes)
#2  - Ver se tem campanhas ou ver se tem mais hospitais
#3  - Ver a taxa de mortalidade pra todos hospitais que ADERIRAM a politica de pro aleitamento
#4  - Gerar a série temporais em cima da coluna taxa de mortalidade
#5  - marcar a data que a politica foi encontrada, adotada
#6  - Separar os hospitais que visivelmente tiveram mudanças com as politicas aplicadas (Antes e depois da Data que a politica foi aplicada)
#7  - Selecionar alguns hospitais que aconteceu essa diferença (tanto pra menos quanto pra mais)
#8  - Aplicar tecnicas para  identificacao de eventos, (Evento -> Modelo indica que ponta da curva houve uma mudança de padrão)
#9  - Aplicar pelo menos 5 técnicas para detecção de eventos
#10 - Validar se a data do algoritmo é igual a data da mudança
