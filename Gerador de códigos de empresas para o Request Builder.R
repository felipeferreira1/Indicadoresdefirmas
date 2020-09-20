#Rotina para criar códigos para o request builder para várias empresas em vários trimestres
#Adaptado por: Felipe Simplício Ferreira
#última atualização: 20/09/2020

#Definindo diretórios a serem utilizados
getwd()
setwd("C:/Users/User/OneDrive/Códigos a serem testados")

#Definindo período de tempo
ano_inicial = 1994 #Mudar de acordo com a necessidade
ano_final = 2019
anos = ano_inicial:ano_final
trimestres = c("Q1","Q2","Q3","Q4")

#Coletando nomes de empresas
arquivo_entrada = read.csv2("empresas.csv")
empresas = arquivo_entrada$Ticker

#Criando códigos para cada empresa e trimestre
#Configurando os trimestres
primeiro_codigo = c()
for (trimestres in trimestres){
  trim_codigo = print(paste("|",trimestres,"|",sep = ""))
  primeiro_codigo = union(primeiro_codigo,trim_codigo)
}

#Configurando trimestres com os anos
segundo_codigo = c()
indice_segundo_codigo = 1:(ano_final-ano_inicial+1)

for (i in indice_segundo_codigo) {
  trim_codigo = print(paste(primeiro_codigo,"|", anos[i],"|",sep = ""))
  segundo_codigo = union(segundo_codigo,trim_codigo)
}

#Configurando trimestres com anos com tickers de empresas
terceiro_codigo = c()
indice_terceiro_codigo = 1:(length(empresas))

for (i in indice_terceiro_codigo){
  trim_codigo = print(paste(empresas[i],"||2|FUND_PER|EQY_FUND_YEAR",segundo_codigo,sep = ""))
  terceiro_codigo = union(terceiro_codigo,trim_codigo)
}

#Exportação de dados
write(terceiro_codigo,file="tickers.txt")
