#stdlib
rm(list=ls(all=TRUE))
library(plotly)
library(fPortfolio)
library(BatchGetSymbols)
# todas as fun??es que subscrevem tidyverse devem vir antes
library(tidyverse)
library(readxl)
library(lubridate)
library(mondate)
library(gdata)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)
library(openxlsx)
library(tseries)
require(lubridate)
library(forecast)
setwd("Z:/GERENCIA/INVESTIMENTOS/05. PROVISAO DE PERDA/03. ANÁLISE")

Ano_base = 2022
Mes_base = 6 
Meses_calculados = 12
Data_base = dmy("03/06/2022")
fund_register = "Fundos_cadastrados.xlsx"
cvm_link = "http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_"
cvm_link2 = "inf_diario_fi_"
Previsao=0
Valor_quota=0
perda =0
perda = list()
TESTE= list()
dias_uteis =147
serie= list()
teste = list()
decisao = 0
decisao2 = 0
decisao3 = 0
decisao_final = 0
normalidade = 0
normalidade2 = 0
s=0
#intervalo = length(my_dt) -1
dados_treino = list()
dados_teste = list()
acuracia = list()
previsao = list()




suffix_cvm = function(Meses_calculados,Data_base)
{
  Anos=0
  Meses=0
  #Starting dates
  Meses[1] = format(mondate(Data_base, "%m"))
  Anos[1] = format(mondate(Data_base, "%Y"))
  
  #Creating suffix to be used in cvm link
  suffix = rep(0, Meses_calculados)
  suffix[1] = paste0(Anos[1], Meses[1])
  
  for (i in 1:(Meses_calculados-1))
  {
    Meses[i+1] = format(mondate(Data_base- months(i), "%m"))
    Anos[i+1] = format(mondate(Data_base- months(i), "%Y"))
    suffix[i+1] = paste0(Anos[i+1], Meses[i+1])
  }
  
  return(suffix)
}


Download_CVM = function(n, suffix)
{
  
  #Temporary file to be created
  tmp = 'tmp.zip'
  
  lista = list()
  myurl = paste0(cvm_link, suffix,".zip")
  mydta = paste0(cvm_link2, suffix,".csv")
  
  for (i in 1:n)
  {
    download.file(url = myurl[i], destfile = tmp)
    unzip(tmp)
    
    # lista[[i]] = read_delim(mydta[i],
    #                         delim = ";",
    #                         escape_double = FALSE,
    #                         col_types = cols(DT_COMPTC = col_character()),
    #                         trim_ws = TRUE)
  }
  
  
  lista = do.call(rbind, lapply(list.files(path = ".", pattern = "csv"),
                                read.csv2))
  
  return(lista)
}


Read_CVM = function(n, suffix)
{
  lista = list()
  mydta = vector()
  
  lista = do.call(rbind.data.frame, lapply(list.files(path = ".", pattern = "csv"),
                                           read.csv, sep = ";"))
  
  return(lista)
}





read_funds = function(fund_register)
{
  Fundos <- read_excel(fund_register)
  #Blacklist
  Fundos = filter(Fundos, Fundos$CNPJ_FUNDO != "35.292.597/0001-70")
  
  return(Fundos)
}



Analise_CNPJ = function(my_dt,Fundos)
{
  my_dt = inner_join(my_dt, Fundos, by= ("CNPJ_FUNDO"="CNPJ_FUNDO"))
  CNPJS = unique(my_dt$CNPJ_FUNDO)
  my_dt = my_dt[(order(as.Date(my_dt$DT_COMPTC, format="%Y/%m/%d"))),]
  
  my_dt2 = list()
  for (i in 1:length(CNPJS))
    my_dt2[[i]] = filter(my_dt , my_dt$CNPJ_FUNDO == CNPJS[i])
  
  
  return(my_dt2)
}



















# Download dos arquivos do site da cvm 
suffix = suffix_cvm(Meses_calculados, Data_base)
#my_dt = Download_CVM(Meses_calculados, suffix)
my_dt = Read_CVM(Meses_calculados, suffix)


# Step ?
Fundos = read_funds(fund_register)

my_dt = Analise_CNPJ(my_dt,Fundos)








#test_data_idx <- function(date)
#  {
#  y <- year(as.Date.character(date)) == 2022
#  m <- month(as.Date.character(date)) == 2

#  return(m & y)
#  }



#training_data_idx <- function(date)
#{
#  y <- year(as.Date.character(date)) == 2022
#  m <- month(as.Date.character(date)) == 2

#  return(!(m & y))
#}  

for (i in 1:length(my_dt)) {
  my_dt[[i]]= filter(my_dt[[i]], my_dt[[i]]$DT_COMPTC <= "2022-06-03")
}


ajuste= list()
vetor_coef= list()
for (i in 1:length(my_dt)) {
  serie[[i]] = auto.arima(ts(my_dt[[i]]$VL_QUOTA),allowdrift = TRUE,stepwise=TRUE,method="ML")
  teste[[i]] = Box.test(residuals(arima(ts(my_dt[[i]]$VL_QUOTA), order =arimaorder(serie[[i]]),method="ML" )),
                        type="Ljung-Box")
  s[i] = arimaorder(serie[[i]])[2]
  vetor_coef[[i]]=arimaorder(serie[[i]])
  ajuste[[i]]= arima(ts(my_dt[[i]]$VL_QUOTA),order=vetor_coef[[i]])
  if(s[i] !=0){
    
    teste[[i]]$dickey_fuller = adf.test(diff(ts(my_dt[[i]]$VL_QUOTA), differences = s[i]))
    teste[[i]]$pptest = pp.test(diff(ts(my_dt[[i]]$VL_QUOTA),differences = s[i])) 
  } else{
    teste[[i]]$dickey_fuller = adf.test(ts(my_dt[[i]]$VL_QUOTA))
    teste[[i]]$pptest = pp.test(ts(my_dt[[i]]$VL_QUOTA))
  }
  
  teste[[i]]$normalidade = jarque.bera.test(residuals(serie[[i]]))
  teste[[i]]$normalidade2 = shapiro.test(residuals(serie[[i]]))
  decisao[i] = if_else(teste[[i]]$p.value < 0.05 , "Rejeita independencia", "Não Rejeita independencia")
  decisao2[i] = if_else(teste[[i]]$dickey_fuller$p.value < 0.05 , "Rejeita não estacionariedade", "Rejeita estacionariedade")
  decisao3[i] = if_else(teste[[i]]$pptest$p.value < 0.05 , " Rejeita não  estacionariedade", "Rejeita  estacionariedade")
  normalidade[i] = if_else(teste[[i]]$normalidade$p.value < 0.05 , "Rejeita normalidade dos erros", "Não Rejeita normalidade dos erros")
  normalidade2[i] = if_else(teste[[i]]$normalidade2$p.value < 0.05 ,"Rejeita normalidade dos erros", "Não Rejeita normalidade dos erros")
  
  previsao[[i]] = forecast(ajuste[[i]],h=dias_uteis, level=95)
  #acuracia[[i]]=  accuracy(forecast(serie[[i]],h=30, level=95),dados_teste[[i]])
  
  
  
}



ajuste[[11]]
previsao[[11]]




table(decisao)
table(decisao2)
table(decisao3)
# 
11/57
2/57

#19,2% das series foi rejeitada a independencia
#92,3% das series foi rejeitada a não estacionariedade, então 1,92% são não estacionarios.


table(normalidade)
table(normalidade2)
11/57

#47/52 
#19,2% das series foi rejeitada a hipotese de normalidade dos erros



fstat= list()
homoc= 0
i=1
for (i in 1:length(serie)) {
  
  m = length(serie[[i]]$residuals)/2
  a1 = serie[[i]]$residuals[1:m]
  a2 = serie[[i]]$residuals[(m+1):length(serie[[i]]$residuals)]
  
  
  
  f= sum(a1^2/m)/(sum(a2^2/(m)))
  
  
  fstat[[i]] =qf(0.95,m,m)
  
  homoc[i] = if_else(fstat[[i]] < f , " Rejeita homocedasticidade", "Não rejeita homocedasticidade")
  
}

table(homoc)
10/57
#15,3% das series não sao homocedasticas



idx = which(Fundos$Numero_de_Quotas!=0) 
#Fundos_alocados = filter(Fundos , Fundos$Numero_de_Quotas !=0)

# Os indices de fundos que possuem alguma alocação




i=1

for (i in 1:length(my_dt)) {
  my_dt[[i]]$DT_COMPTC = as.Date.character(my_dt[[i]]$DT_COMPTC)
  TESTE[[i]]= filter(my_dt[[i]], my_dt[[i]]$DT_COMPTC == "2022-06-03")
  
}





perda=0
Previsao=0
Valor_quota=0
i=1
Nome_do_fundo= 0
Numero_de_quotas= 0

for (i in 1:length(my_dt)) {
  
  
  #Previsao[i] = tail(previsao[[i]]$lower,1)
  Previsao[i] = tail(previsao[[i]]$mean,1)
  Valor_quota[i]= TESTE[[i]]$VL_QUOTA
  perda[i] = (Previsao[i] - Valor_quota[i])*(TESTE[[i]]$Numero_de_Quotas) 
  Nome_do_fundo[i] = TESTE[[i]]$`Nome do fundo`
  Numero_de_quotas[i]= TESTE[[i]]$Numero_de_Quotas
}


Provisao_de_perda = data.frame(Nome_do_fundo= Nome_do_fundo,Previsao= Previsao,Valor_quota= Valor_quota,Numero_de_quotas=Numero_de_quotas ,Perda = perda )

Provisao_de_perda2 = filter(Provisao_de_perda, Provisao_de_perda$Perda <0)




#write.xlsx(Provisao_de_perda, "Excluir_depois.xlsx")
#write.xlsx(Provisao_de_perda2, "Excluir_depois2.xlsx")



# Analise do fundo fia institucional bdr 
serie[[35]]
ts(my_dt[[35]]$VL_QUOTA) %>% autoplot()
ts(diff(my_dt[[35]]$VL_QUOTA,differences=1)) %>% autoplot()
ts(diff(my_dt[[35]]$VL_QUOTA,differences=2)) %>% autoplot()
tseries::adf.test (ts(diff(my_dt[[35]]$VL_QUOTA,differences=1)),k=0) 

tseries::adf.test (ts(diff(my_dt[[35]]$VL_QUOTA,differences=2)),k=0)  

acf(diff(ts(my_dt[[35]]$VL_QUOTA), differences=1))

pacf(diff(ts(my_dt[[35]]$VL_QUOTA), differences=1))

# Para duas diferenças 

acf(diff(ts(my_dt[[35]]$VL_QUOTA), differences=2))

pacf(diff(ts(my_dt[[35]]$VL_QUOTA), differences=2))
# A partir da analise das auto correlações vemos que temos excesso de diferenças

# Escolhe-se entao por 1 diferença apenas 

fit1=  arima(ts(my_dt[[35]]$VL_QUOTA),order=c(1,1,1))
summary(fit1)

pred = forecast(fit1,h=dias_uteis, level=95)
pred






# Escolhemos o arima(0,1,1)




 



# Analizando o fundo açoes aloc investim exterior 
# O modelo pelo auto arima foi o (1,2,2)
serie[[43]]


ts(my_dt[[43]]$VL_QUOTA) %>% autoplot()
ts(diff(my_dt[[43]]$VL_QUOTA,differences=1)) %>% autoplot()



tseries::adf.test (ts(diff(my_dt[[43]]$VL_QUOTA,differences=1)),k=0) 

tseries::adf.test (ts(diff(my_dt[[43]]$VL_QUOTA,differences=2)),k=0)  

acf(diff(ts(my_dt[[43]]$VL_QUOTA), differences=1))

pacf(diff(ts(my_dt[[43]]$VL_QUOTA), differences=1))



# Para duas diferenças 

acf(diff(ts(my_dt[[43]]$VL_QUOTA), differences=2))

pacf(diff(ts(my_dt[[43]]$VL_QUOTA), differences=2))

# A partir da analise das auto correlações vemos que temos excesso de diferenças quando usamos 2 
# entao optaremos por 1 diferença so 
# o arima escolhido primariamente sera(0,1,1)
# que sera comparado ao (0,1,2)

fit2=  arima(ts(my_dt[[43]]$VL_QUOTA),order=c(0,1,1))
summary(fit2)

pred2 = forecast(fit2,h=dias_uteis, level=95)
pred2




# Escolhemos o arima(0,1,1) 


  
  #FIA INSTITUCIONAL BDR 
  Previsao[35] = tail(pred$mean,1)
  
 


  # AÇÕES ALOC INVESTIM EXTERIOR
  Previsao[43] = tail(pred2$mean,1)



  i=1
  for (i in 1:length(my_dt)) {
    
    
    #Previsao[i] = tail(previsao[[i]]$lower,1)
    #Previsao[i] = tail(previsao[[i]]$mean,1)
    Valor_quota[i]= TESTE[[i]]$VL_QUOTA
    perda[i] = (Previsao[i] - Valor_quota[i])*(TESTE[[i]]$Numero_de_Quotas) 
    Nome_do_fundo[i] = TESTE[[i]]$`Nome do fundo`
    Numero_de_quotas[i]= TESTE[[i]]$Numero_de_Quotas
  }
  
  
  
  Provisao_de_perda = data.frame(Nome_do_fundo= Nome_do_fundo,Previsao= Previsao,Valor_quota= Valor_quota,Numero_de_quotas=Numero_de_quotas ,Perda = perda )
  
  Provisao_de_perda2 = filter(Provisao_de_perda, Provisao_de_perda$Perda <0)
  
  
  #write.xlsx(Provisao_de_perda2, "Excluir_depois4.xlsx")
  