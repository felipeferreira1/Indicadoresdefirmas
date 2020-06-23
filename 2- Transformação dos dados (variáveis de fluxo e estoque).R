########## Ativo Total ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
ativo_total2 = ativo_total
for(i in 1:nrow(ativo_total2)){
  for(j in 4:ncol(ativo_total2)){
    if(j == 4){ativo_total2[i,j] = ativo_total[i,j]}
    else{if(j == 5){ativo_total2[i,j] = mean(c(ativo_total[i,(j-1)], ativo_total[i,j]), na.rm = TRUE)}
      else{if(j == 6){ativo_total2[i,j] = mean(c(ativo_total[i,(j-2)], ativo_total[i,(j-1)], ativo_total[i,j]), na.rm = TRUE)}
        else{ativo_total2[i,j] = mean(c(ativo_total[i,(j-3)], ativo_total[i,(j-2)], ativo_total[i,(j-1)], ativo_total[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(ativo_total2)){
  v = unlist(ativo_total2[i,-c(1:3)])
  v[is.nan(v)] = NA
  ativo_total2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(ativo_total2$`Setor ICB` == cond[1] |
              ativo_total2$`Setor ICB` == cond[2] |
              ativo_total2$`Setor ICB` == cond[3] |
              ativo_total2$`Setor ICB` == cond[4] |
              ativo_total2$`Setor ICB` == cond[5] |
              ativo_total2$`Setor ICB` == cond[6] |
              ativo_total2$`Setor ICB` == cond[7] |
              ativo_total2$`Setor ICB` == cond[8] |
              ativo_total2$`Nome` == cond[9])

ativo_total2 = ativo_total2[-pos,]


########## EBITDA ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
EBITDA2 = EBITDA
for(i in 1:nrow(EBITDA2)){
  for(j in 4:ncol(EBITDA2)){
    if(j == 4){EBITDA2[i,j] = EBITDA[i,j]}
    else{if(j == 5){EBITDA2[i,j] = sum(c(EBITDA[i,(j-1)], EBITDA[i,j]), na.rm = TRUE)}
      else{if(j == 6){EBITDA2[i,j] = sum(c(EBITDA[i,(j-2)], EBITDA[i,(j-1)], EBITDA[i,j]), na.rm = TRUE)}
        else{EBITDA2[i,j] = sum(c(EBITDA[i,(j-3)], EBITDA[i,(j-2)], EBITDA[i,(j-1)], EBITDA[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(EBITDA2)){
  v = unlist(EBITDA2[i,-c(1:3)])
  v[is.nan(v)] = NA
  EBITDA2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(EBITDA2$`Setor ICB` == cond[1] |
              EBITDA2$`Setor ICB` == cond[2] |
              EBITDA2$`Setor ICB` == cond[3] |
              EBITDA2$`Setor ICB` == cond[4] |
              EBITDA2$`Setor ICB` == cond[5] |
              EBITDA2$`Setor ICB` == cond[6] |
              EBITDA2$`Setor ICB` == cond[7] |
              EBITDA2$`Setor ICB` == cond[8] |
              EBITDA2$`Nome` == cond[9])

EBITDA2 = EBITDA2[-pos,]


########## Entrada de Caixa Operacional ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
entr_cx_oper2 = entr_cx_oper
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:ncol(entr_cx_oper2)){
    if(j == 4){entr_cx_oper2[i,j] = entr_cx_oper[i,j]}
    else{if(j == 5){entr_cx_oper2[i,j] = sum(c(entr_cx_oper[i,(j-1)], entr_cx_oper[i,j]), na.rm = TRUE)}
      else{if(j == 6){entr_cx_oper2[i,j] = sum(c(entr_cx_oper[i,(j-2)], entr_cx_oper[i,(j-1)], entr_cx_oper[i,j]), na.rm = TRUE)}
        else{entr_cx_oper2[i,j] = sum(c(entr_cx_oper[i,(j-3)], entr_cx_oper[i,(j-2)], entr_cx_oper[i,(j-1)], entr_cx_oper[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(entr_cx_oper2)){
  v = unlist(entr_cx_oper2[i,-c(1:3)])
  v[is.nan(v)] = NA
  entr_cx_oper2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(entr_cx_oper2$`Setor ICB` == cond[1] |
              entr_cx_oper2$`Setor ICB` == cond[2] |
              entr_cx_oper2$`Setor ICB` == cond[3] |
              entr_cx_oper2$`Setor ICB` == cond[4] |
              entr_cx_oper2$`Setor ICB` == cond[5] |
              entr_cx_oper2$`Setor ICB` == cond[6] |
              entr_cx_oper2$`Setor ICB` == cond[7] |
              entr_cx_oper2$`Setor ICB` == cond[8] |
              entr_cx_oper2$`Nome` == cond[9])

entr_cx_oper2 = entr_cx_oper2[-pos,]


########## Lucros Retidos ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
lucros_retidos2 = lucros_retidos
for(i in 1:nrow(lucros_retidos2)){
  for(j in 4:ncol(lucros_retidos2)){
    if(j == 4){lucros_retidos2[i,j] = lucros_retidos[i,j]}
    else{if(j == 5){lucros_retidos2[i,j] = sum(c(lucros_retidos[i,(j-1)], lucros_retidos[i,j]), na.rm = TRUE)}
      else{if(j == 6){lucros_retidos2[i,j] = sum(c(lucros_retidos[i,(j-2)], lucros_retidos[i,(j-1)], lucros_retidos[i,j]), na.rm = TRUE)}
        else{lucros_retidos2[i,j] = sum(c(lucros_retidos[i,(j-3)], lucros_retidos[i,(j-2)], lucros_retidos[i,(j-1)], lucros_retidos[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(lucros_retidos2)){
  v = unlist(lucros_retidos2[i,-c(1:3)])
  v[is.nan(v)] = NA
  lucros_retidos2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(lucros_retidos2$`Setor ICB` == cond[1] |
              lucros_retidos2$`Setor ICB` == cond[2] |
              lucros_retidos2$`Setor ICB` == cond[3] |
              lucros_retidos2$`Setor ICB` == cond[4] |
              lucros_retidos2$`Setor ICB` == cond[5] |
              lucros_retidos2$`Setor ICB` == cond[6] |
              lucros_retidos2$`Setor ICB` == cond[7] |
              lucros_retidos2$`Setor ICB` == cond[8] |
              lucros_retidos2$`Nome` == cond[9])

lucros_retidos2 = lucros_retidos2[-pos,]


########## Lucros Básicos por Ação ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
lucros_por_acao2 = lucros_por_acao
for(i in 1:nrow(lucros_por_acao2)){
  for(j in 4:ncol(lucros_por_acao2)){
    if(j == 4){lucros_por_acao2[i,j] = lucros_por_acao[i,j]}
    else{if(j == 5){lucros_por_acao2[i,j] = sum(c(lucros_por_acao[i,(j-1)], lucros_por_acao[i,j]), na.rm = TRUE)}
      else{if(j == 6){lucros_por_acao2[i,j] = sum(c(lucros_por_acao[i,(j-2)], lucros_por_acao[i,(j-1)], lucros_por_acao[i,j]), na.rm = TRUE)}
        else{lucros_por_acao2[i,j] = sum(c(lucros_por_acao[i,(j-3)], lucros_por_acao[i,(j-2)], lucros_por_acao[i,(j-1)], lucros_por_acao[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(lucros_por_acao2)){
  v = unlist(lucros_por_acao2[i,-c(1:3)])
  v[is.nan(v)] = NA
  lucros_por_acao2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(lucros_por_acao2$`Setor ICB` == cond[1] |
              lucros_por_acao2$`Setor ICB` == cond[2] |
              lucros_por_acao2$`Setor ICB` == cond[3] |
              lucros_por_acao2$`Setor ICB` == cond[4] |
              lucros_por_acao2$`Setor ICB` == cond[5] |
              lucros_por_acao2$`Setor ICB` == cond[6] |
              lucros_por_acao2$`Setor ICB` == cond[7] |
              lucros_por_acao2$`Setor ICB` == cond[8] |
              lucros_por_acao2$`Nome` == cond[9])

lucros_por_acao2 = lucros_por_acao2[-pos,]


########## Dívida de Curto Prazo ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
div_cp2 = div_cp
for(i in 1:nrow(div_cp2)){
  for(j in 4:ncol(div_cp2)){
    if(j == 4){div_cp2[i,j] = div_cp[i,j]}
    else{if(j == 5){div_cp2[i,j] = mean(c(div_cp[i,(j-1)], div_cp[i,j]), na.rm = TRUE)}
      else{if(j == 6){div_cp2[i,j] = mean(c(div_cp[i,(j-2)], div_cp[i,(j-1)], div_cp[i,j]), na.rm = TRUE)}
        else{div_cp2[i,j] = mean(c(div_cp[i,(j-3)], div_cp[i,(j-2)], div_cp[i,(j-1)], div_cp[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(div_cp2)){
  v = unlist(div_cp2[i,-c(1:3)])
  v[is.nan(v)] = NA
  div_cp2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(div_cp2$`Setor ICB` == cond[1] |
              div_cp2$`Setor ICB` == cond[2] |
              div_cp2$`Setor ICB` == cond[3] |
              div_cp2$`Setor ICB` == cond[4] |
              div_cp2$`Setor ICB` == cond[5] |
              div_cp2$`Setor ICB` == cond[6] |
              div_cp2$`Setor ICB` == cond[7] |
              div_cp2$`Setor ICB` == cond[8] |
              div_cp2$`Nome` == cond[9])

div_cp2 = div_cp2[-pos,]


########## Dívida de Longo Prazo ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
div_lp2 = div_lp
for(i in 1:nrow(div_lp2)){
  for(j in 4:ncol(div_lp2)){
    if(j == 4){div_lp2[i,j] = div_lp[i,j]}
    else{if(j == 5){div_lp2[i,j] = mean(c(div_lp[i,(j-1)], div_lp[i,j]), na.rm = TRUE)}
      else{if(j == 6){div_lp2[i,j] = mean(c(div_lp[i,(j-2)], div_lp[i,(j-1)], div_lp[i,j]), na.rm = TRUE)}
        else{div_lp2[i,j] = mean(c(div_lp[i,(j-3)], div_lp[i,(j-2)], div_lp[i,(j-1)], div_lp[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(div_lp2)){
  v = unlist(div_lp2[i,-c(1:3)])
  v[is.nan(v)] = NA
  div_lp2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(div_lp2$`Setor ICB` == cond[1] |
              div_lp2$`Setor ICB` == cond[2] |
              div_lp2$`Setor ICB` == cond[3] |
              div_lp2$`Setor ICB` == cond[4] |
              div_lp2$`Setor ICB` == cond[5] |
              div_lp2$`Setor ICB` == cond[6] |
              div_lp2$`Setor ICB` == cond[7] |
              div_lp2$`Setor ICB` == cond[8] |
              div_lp2$`Nome` == cond[9])

div_lp2 = div_lp2[-pos,]


########## Despesa de Juros ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
despesa_juros2 = despesa_juros
for(i in 1:nrow(despesa_juros2)){
  for(j in 4:ncol(despesa_juros2)){
    if(j == 4){despesa_juros2[i,j] = despesa_juros[i,j]}
    else{if(j == 5){despesa_juros2[i,j] = sum(c(despesa_juros[i,(j-1)], despesa_juros[i,j]), na.rm = TRUE)}
      else{if(j == 6){despesa_juros2[i,j] = sum(c(despesa_juros[i,(j-2)], despesa_juros[i,(j-1)], despesa_juros[i,j]), na.rm = TRUE)}
        else{despesa_juros2[i,j] = sum(c(despesa_juros[i,(j-3)], despesa_juros[i,(j-2)], despesa_juros[i,(j-1)], despesa_juros[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(despesa_juros2)){
  v = unlist(despesa_juros2[i,-c(1:3)])
  v[is.nan(v)] = NA
  despesa_juros2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(despesa_juros2$`Setor ICB` == cond[1] |
              despesa_juros2$`Setor ICB` == cond[2] |
              despesa_juros2$`Setor ICB` == cond[3] |
              despesa_juros2$`Setor ICB` == cond[4] |
              despesa_juros2$`Setor ICB` == cond[5] |
              despesa_juros2$`Setor ICB` == cond[6] |
              despesa_juros2$`Setor ICB` == cond[7] |
              despesa_juros2$`Setor ICB` == cond[8] |
              despesa_juros2$`Nome` == cond[9])

despesa_juros2 = despesa_juros2[-pos,]


########## Patrimônio Total ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
patrimonio_total2 = patrimonio_total
for(i in 1:nrow(patrimonio_total2)){
  for(j in 4:ncol(patrimonio_total2)){
    if(j == 4){patrimonio_total2[i,j] = patrimonio_total[i,j]}
    else{if(j == 5){patrimonio_total2[i,j] = mean(c(patrimonio_total[i,(j-1)], patrimonio_total[i,j]), na.rm = TRUE)}
      else{if(j == 6){patrimonio_total2[i,j] = mean(c(patrimonio_total[i,(j-2)], patrimonio_total[i,(j-1)], patrimonio_total[i,j]), na.rm = TRUE)}
        else{patrimonio_total2[i,j] = mean(c(patrimonio_total[i,(j-3)], patrimonio_total[i,(j-2)], patrimonio_total[i,(j-1)], patrimonio_total[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(patrimonio_total2)){
  v = unlist(patrimonio_total2[i,-c(1:3)])
  v[is.nan(v)] = NA
  patrimonio_total2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(patrimonio_total2$`Setor ICB` == cond[1] |
              patrimonio_total2$`Setor ICB` == cond[2] |
              patrimonio_total2$`Setor ICB` == cond[3] |
              patrimonio_total2$`Setor ICB` == cond[4] |
              patrimonio_total2$`Setor ICB` == cond[5] |
              patrimonio_total2$`Setor ICB` == cond[6] |
              patrimonio_total2$`Setor ICB` == cond[7] |
              patrimonio_total2$`Setor ICB` == cond[8] |
              patrimonio_total2$`Nome` == cond[9])

patrimonio_total2 = patrimonio_total2[-pos,]


########## Lucro Líquido ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
lucro_liquido2 = lucro_liquido
for(i in 1:nrow(lucro_liquido2)){
  for(j in 4:ncol(lucro_liquido2)){
    if(j == 4){lucro_liquido2[i,j] = lucro_liquido[i,j]}
    else{if(j == 5){lucro_liquido2[i,j] = sum(c(lucro_liquido[i,(j-1)], lucro_liquido[i,j]), na.rm = TRUE)}
      else{if(j == 6){lucro_liquido2[i,j] = sum(c(lucro_liquido[i,(j-2)], lucro_liquido[i,(j-1)], lucro_liquido[i,j]), na.rm = TRUE)}
        else{lucro_liquido2[i,j] = sum(c(lucro_liquido[i,(j-3)], lucro_liquido[i,(j-2)], lucro_liquido[i,(j-1)], lucro_liquido[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(lucro_liquido2)){
  v = unlist(lucro_liquido2[i,-c(1:3)])
  v[is.nan(v)] = NA
  lucro_liquido2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(lucro_liquido2$`Setor ICB` == cond[1] |
              lucro_liquido2$`Setor ICB` == cond[2] |
              lucro_liquido2$`Setor ICB` == cond[3] |
              lucro_liquido2$`Setor ICB` == cond[4] |
              lucro_liquido2$`Setor ICB` == cond[5] |
              lucro_liquido2$`Setor ICB` == cond[6] |
              lucro_liquido2$`Setor ICB` == cond[7] |
              lucro_liquido2$`Setor ICB` == cond[8] |
              lucro_liquido2$`Nome` == cond[9])

lucro_liquido2 = lucro_liquido2[-pos,]


########## Preço/Lucros por ação básicos ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
pl_acao_b2 = pl_acao_b
for(i in 1:nrow(pl_acao_b2)){
  for(j in 4:ncol(pl_acao_b2)){
    if(j == 4){pl_acao_b2[i,j] = pl_acao_b[i,j]}
    else{if(j == 5){pl_acao_b2[i,j] = mean(c(pl_acao_b[i,(j-1)], pl_acao_b[i,j]), na.rm = TRUE)}
      else{if(j == 6){pl_acao_b2[i,j] = mean(c(pl_acao_b[i,(j-2)], pl_acao_b[i,(j-1)], pl_acao_b[i,j]), na.rm = TRUE)}
        else{pl_acao_b2[i,j] = mean(c(pl_acao_b[i,(j-3)], pl_acao_b[i,(j-2)], pl_acao_b[i,(j-1)], pl_acao_b[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(pl_acao_b2)){
  v = unlist(pl_acao_b2[i,-c(1:3)])
  v[is.nan(v)] = NA
  pl_acao_b2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(pl_acao_b2$`Setor ICB` == cond[1] |
              pl_acao_b2$`Setor ICB` == cond[2] |
              pl_acao_b2$`Setor ICB` == cond[3] |
              pl_acao_b2$`Setor ICB` == cond[4] |
              pl_acao_b2$`Setor ICB` == cond[5] |
              pl_acao_b2$`Setor ICB` == cond[6] |
              pl_acao_b2$`Setor ICB` == cond[7] |
              pl_acao_b2$`Setor ICB` == cond[8] |
              pl_acao_b2$`Nome` == cond[9])

pl_acao_b2 = pl_acao_b2[-pos,]


########## Preço/Lucros por ação diluídos ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
pl_acao_d2 = pl_acao_d
for(i in 1:nrow(pl_acao_d2)){
  for(j in 4:ncol(pl_acao_d2)){
    if(j == 4){pl_acao_d2[i,j] = pl_acao_d[i,j]}
    else{if(j == 5){pl_acao_d2[i,j] = mean(c(pl_acao_d[i,(j-1)], pl_acao_d[i,j]), na.rm = TRUE)}
      else{if(j == 6){pl_acao_d2[i,j] = mean(c(pl_acao_d[i,(j-2)], pl_acao_d[i,(j-1)], pl_acao_d[i,j]), na.rm = TRUE)}
        else{pl_acao_d2[i,j] = mean(c(pl_acao_d[i,(j-3)], pl_acao_d[i,(j-2)], pl_acao_d[i,(j-1)], pl_acao_d[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(pl_acao_d2)){
  v = unlist(pl_acao_d2[i,-c(1:3)])
  v[is.nan(v)] = NA
  pl_acao_d2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(pl_acao_d2$`Setor ICB` == cond[1] |
              pl_acao_d2$`Setor ICB` == cond[2] |
              pl_acao_d2$`Setor ICB` == cond[3] |
              pl_acao_d2$`Setor ICB` == cond[4] |
              pl_acao_d2$`Setor ICB` == cond[5] |
              pl_acao_d2$`Setor ICB` == cond[6] |
              pl_acao_d2$`Setor ICB` == cond[7] |
              pl_acao_d2$`Setor ICB` == cond[8] |
              pl_acao_d2$`Nome` == cond[9])

pl_acao_d2 = pl_acao_d2[-pos,]


########## Patrimônio Líquido (NAV) ##########

# Variável de estoque. Dessa forma, o valor da variável naquele trimestre corresponde à média dos 4 trimestres anteriores.
patr_liq2 = patr_liq
for(i in 1:nrow(patr_liq2)){
  for(j in 4:ncol(patr_liq2)){
    if(j == 4){patr_liq2[i,j] = patr_liq[i,j]}
    else{if(j == 5){patr_liq2[i,j] = mean(c(patr_liq[i,(j-1)], patr_liq[i,j]), na.rm = TRUE)}
      else{if(j == 6){patr_liq2[i,j] = mean(c(patr_liq[i,(j-2)], patr_liq[i,(j-1)], patr_liq[i,j]), na.rm = TRUE)}
        else{patr_liq2[i,j] = mean(c(patr_liq[i,(j-3)], patr_liq[i,(j-2)], patr_liq[i,(j-1)], patr_liq[i,j]), na.rm = TRUE)}}}
  }
}

for(i in 1:nrow(patr_liq2)){
  v = unlist(patr_liq2[i,-c(1:3)])
  v[is.nan(v)] = NA
  patr_liq2[i,-c(1:3)] = v
  rm(v)
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(patr_liq2$`Setor ICB` == cond[1] |
              patr_liq2$`Setor ICB` == cond[2] |
              patr_liq2$`Setor ICB` == cond[3] |
              patr_liq2$`Setor ICB` == cond[4] |
              patr_liq2$`Setor ICB` == cond[5] |
              patr_liq2$`Setor ICB` == cond[6] |
              patr_liq2$`Setor ICB` == cond[7] |
              patr_liq2$`Setor ICB` == cond[8] |
              patr_liq2$`Nome` == cond[9])

patr_liq2 = patr_liq2[-pos,]

########## Porcentagem da dívida externa na dívida total ##########
# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV e o índice IBOV
div_est2 = div_est
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(div_est2$`Setor ICB` == cond[1] |
              div_est$`Setor ICB` == cond[2] |
              div_est$`Setor ICB` == cond[3] |
              div_est$`Setor ICB` == cond[4] |
              div_est$`Setor ICB` == cond[5] |
              div_est$`Setor ICB` == cond[6] |
              div_est$`Setor ICB` == cond[7] |
              div_est$`Setor ICB` == cond[8] |
              div_est$`Nome` == cond[9])

div_est2 = div_est2[-pos,]

########## Dívida externa ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
div_est_tot2 = div_est_tot
for(i in 1:nrow(div_est_tot2)){
  for(j in 4:ncol(div_est_tot2)){
    if(j == 4){div_est_tot2[i,j] = div_est_tot[i,j]}
    else{if(j == 5){div_est_tot2[i,j] = sum(c(div_est_tot[i,(j-1)], div_est_tot[i,j]), na.rm = TRUE)}
      else{if(j == 6){div_est_tot2[i,j] = sum(c(div_est_tot[i,(j-2)], div_est_tot[i,(j-1)], div_est_tot[i,j]), na.rm = TRUE)}
        else{div_est_tot2[i,j] = sum(c(div_est_tot[i,(j-3)], div_est_tot[i,(j-2)], div_est_tot[i,(j-1)], div_est_tot[i,j]), na.rm = TRUE)}}}
  }
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(div_est_tot2$`Setor ICB` == cond[1] |
              div_est_tot2$`Setor ICB` == cond[2] |
              div_est_tot2$`Setor ICB` == cond[3] |
              div_est_tot2$`Setor ICB` == cond[4] |
              div_est_tot2$`Setor ICB` == cond[5] |
              div_est_tot2$`Setor ICB` == cond[6] |
              div_est_tot2$`Setor ICB` == cond[7] |
              div_est_tot2$`Setor ICB` == cond[8] |
              div_est_tot2$`Nome` == cond[9])

div_est_tot2 = div_est_tot2[-pos,]

########## Debentures ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
deb2 = deb
for(i in 1:nrow(deb2)){
  for(j in 4:ncol(deb2)){
    if(j == 4){deb2[i,j] = deb[i,j]}
    else{if(j == 5){deb2[i,j] = sum(c(deb[i,(j-1)], deb[i,j]), na.rm = TRUE)}
      else{if(j == 6){deb2[i,j] = sum(c(deb[i,(j-2)], deb[i,(j-1)], deb[i,j]), na.rm = TRUE)}
        else{deb2[i,j] = sum(c(deb[i,(j-3)], deb[i,(j-2)], deb[i,(j-1)], deb[i,j]), na.rm = TRUE)}}}
  }
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(deb2$`Setor ICB` == cond[1] |
              deb2$`Setor ICB` == cond[2] |
              deb2$`Setor ICB` == cond[3] |
              deb2$`Setor ICB` == cond[4] |
              deb2$`Setor ICB` == cond[5] |
              deb2$`Setor ICB` == cond[6] |
              deb2$`Setor ICB` == cond[7] |
              deb2$`Setor ICB` == cond[8] |
              deb2$`Nome` == cond[9])

deb2 = deb2[-pos,]

########## Dívida total ##########

# Variável de fluxo. Dessa forma, o valor da variável naquele trimestre corresponde à soma dos 4 trimestres anteriores.
div_tot2 = div_tot
for(i in 1:nrow(div_tot2)){
  for(j in 4:ncol(div_tot2)){
    if(j == 4){div_tot2[i,j] = div_tot[i,j]}
    else{if(j == 5){div_tot2[i,j] = sum(c(div_tot[i,(j-1)], div_tot[i,j]), na.rm = TRUE)}
      else{if(j == 6){div_tot2[i,j] = sum(c(div_tot[i,(j-2)], div_tot[i,(j-1)], div_tot[i,j]), na.rm = TRUE)}
        else{div_tot2[i,j] = sum(c(div_tot[i,(j-3)], div_tot[i,(j-2)], div_tot[i,(j-1)], div_tot[i,j]), na.rm = TRUE)}}}
  }
}

# Retirando do banco as empresas que fazem parte de setores "indesejados" e o índice IBOV
cond = c("Banks", "Financial Services", "Nonlife Insurance", "Real State Investment & Services",
         "Nonequity Investment Instruments", "Equity Investment Instruments", "#N/A N/A",
         "Real State Investment Trusts", "IBOV")
pos = which(div_tot2$`Setor ICB` == cond[1] |
              div_tot2$`Setor ICB` == cond[2] |
              div_tot2$`Setor ICB` == cond[3] |
              div_tot2$`Setor ICB` == cond[4] |
              div_tot2$`Setor ICB` == cond[5] |
              div_tot2$`Setor ICB` == cond[6] |
              div_tot2$`Setor ICB` == cond[7] |
              div_tot2$`Setor ICB` == cond[8] |
              div_tot2$`Nome` == cond[9])

div_tot2 = div_tot2[-pos,]


#rm(ativo_total, EBITDA, entr_cx_oper, lucros_retidos, lucros_por_acao, div_cp, div_lp,
#   despesa_juros, patrimonio_total,lucro_liquido, pl_acao_b, pl_acao_d, patr_liq, cond,
#   i, j, pos, deb, div_tot, div_est_tot)