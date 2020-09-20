### Instalação de pacotes necessários ###

# Rodar apenas se os pacotes ainda não tiverem sido instalados na máquina

#install.packages("readxl")
#install.packages("xlsx")
#install.packages("lubridate")
#install.packages("smooth")
#install.packages("forecast")


### Carregamento dos pacotes necessários ###
library(readxl)
library(xlsx)
library(lubridate)
library(smooth)
library(forecast)

# definir diretorio de trabalho
setwd("C:/Users/User/Documents/GitHub/indicadoresdefirmas")

### Leitura e formatação dos dados ###

# Objetivo deste trecho do código: ler as planilhas existentes e transformá-las em conjuntos de dados "trabalháveis".
# Como este é um processo que faz a mesma tarefa para todas as planilhas, optou-se por utilizar uma estrutura de
# repetição (for).

for(k in 1:17){
  base = read_excel("Indicadores Bloomberg.xlsx", sheet = k, skip = 4) #Vai lendo aba por aba
  base = as.data.frame(base) #Formata como data frame
  
  seq = seq(1, ncol(base), by = 2) #selecionando linhas onde tem os tickers das empresas
  nomes = names(base)[seq] #Selecionando nomes de empresas baseado na sequencia anterior
  
  for(i in 1:ncol(base)){ #Mudando nomes das colunas para algo mais sequencial
    names(base)[seq][i] = paste0("V", i)
  }
  
  i = 1
  while(i < ncol(base)){ #Formatando colunas de data como data
    base[,i] = as.Date(base[,i], origin = "1970-01-01")
    i = i+2
  }
  
  i = 1
  while(i <= ncol(base)){ #Formatando colunas de data como data
    base[,i] = as.Date(paste0(year(base[,i]), "-",month(base[,i]), "-01"), format = "%Y-%m-%d")
    i = i + 2
  }
  
  i = 1
  vetor = NULL
  while(i <= ncol(base)){ #Pegando códigos das datas para comparação
    vetor = c(vetor,base[,i])
    i = i + 2
  }
  min = as.Date(min(vetor, na.rm = TRUE), origin = "1970-01-01") #Pegando data mais antiga
  max = as.Date(max(vetor, na.rm = TRUE), origin = "1970-01-01") #Pegando data mais nova
  seq_datas = seq.Date(min, max, by = "3 months") #Separando datas por trimestre
  
  rm(vetor, min, max)
  
  dados = data.frame(data = seq_datas) #Cria dataframe com coluna de datas
  i = 1
  while(i < ncol(base)){ #Colocando os outros dados lado a lado, de acordo com a data
    dados_aux = data.frame(data = base[,i], base[,i+1])
    names(dados_aux)[2] = paste0("base_", i+1)
    dados = merge(dados, dados_aux, all.x = TRUE)
    i = i + 2
  }
  names(dados)[-1] = nomes #Recolocando nomes nos dados
  rm(base, dados_aux, i, nomes, seq, seq_datas)
  
  for(i in 2:ncol(dados)){ #Formatando colunas como números
    if(class(dados[,i]) != "numeric"){dados[,i] = as.numeric(dados[,i])} 
  }
  
  aux = t(as.matrix(dados[,-1])) #Transpondo dados
  dados2 = as.data.frame(aux, stringsAsFactors = FALSE)
  dados2$Empresa = row.names(dados2) 
  row.names(dados2) = 1:nrow(dados2)
  dados2 = dados2[,c(ncol(dados2),1:(ncol(dados2)-1))] #Criando linhas com nomes das empresas
  
  datas = dados$data
  anos = year(datas)
  meses = month(datas)
  trim = NULL
  for(i in 1:length(datas)){ #separando rótulos com ano e trimestre
    if(meses[i] == 3){trim[i] = "T1"}
    else{if(meses[i] == 6){trim[i] = "T2"}
      else{if(meses[i] == 9){trim[i] = "T3"}
        else{trim[i] = "T4"}}}
  }
  nomes = paste(anos, trim)
  
  names(dados2) = c("Empresa", nomes) #Colocando rótulos nas colunas de datas
  
  empresas = read_excel("Empresas.xlsx") #Lendo informações das empresas
  dados2 = merge(dados2, empresas, by.x = "Empresa", by.y = "Ticker", sort = FALSE)
  n = ncol(dados2)
  dados2 = dados2[,c(1, n-1, n, 2:(n-2))] #Colocando nome e setor da empresa, além do ticker
  
  rm(aux, dados, anos, datas, i, meses, nomes, trim, empresas, n)
  
  # Guardando a planilha lida (e transformada) em um objeto separado no R
  if(k == 1){ativo_total = dados2}
  if(k == 2){EBITDA = dados2}
  if(k == 3){entr_cx_oper = dados2}
  if(k == 4){lucros_retidos = dados2}
  if(k == 5){lucros_por_acao = dados2}
  if(k == 6){div_cp = dados2}
  if(k == 7){div_lp = dados2}
  if(k == 8){despesa_juros = dados2}
  if(k == 9){patrimonio_total = dados2}
  if(k == 10){lucro_liquido = dados2}
  if(k == 11){pl_acao_b = dados2}
  if(k == 12){pl_acao_d = dados2}
  if(k == 13){patr_liq = dados2}
  if(k == 14){div_est = dados2}
  if(k == 15){div_est_tot = dados2}
  if(k == 16){deb = dados2}
  if(k == 17){div_tot = dados2}
  
  rm(dados2)
  print(k)
}

rm(k)