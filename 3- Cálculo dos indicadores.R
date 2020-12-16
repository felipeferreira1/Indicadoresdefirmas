########## Rentabilidade versão 1 (EBITDA/Patr. Total) ##########
nomes = intersect(names(EBITDA2), names(patrimonio_total2))
entr_cx_oper = entr_cx_oper2[,nomes]
EBITDA = EBITDA2[,nomes]
patrimonio_total = patrimonio_total2[,nomes]


rentabilidade = EBITDA
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      rentabilidade[i,j] = EBITDA[i,j]/patrimonio_total[i,j]
    }
    else{rentabilidade[i,j] = NA}
    if(is.na(rentabilidade[i,j]) == FALSE){
      if(rentabilidade[i,j] == 0){rentabilidade[i,j] = NA}
    }
  }
}


########## Rentabilidade versão 2 (EBITDA/Patr. Líq.) ##########
nomes = intersect(names(EBITDA2), names(patr_liq2))
entr_cx_oper = entr_cx_oper2[,nomes]
EBITDA = EBITDA2[,nomes]
patr_liq = patr_liq2[,nomes]


v2.rentabilidade = EBITDA
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      v2.rentabilidade[i,j] = EBITDA[i,j]/patr_liq[i,j]
    }
    else{v2.rentabilidade[i,j] = NA}
    if(is.na(v2.rentabilidade[i,j]) == FALSE){
      if(v2.rentabilidade[i,j] == 0){v2.rentabilidade[i,j] = NA}
    }
  }
}


########## Rentabilidade versão 3(Lucros líquidos/Patr. Total) ##########
nomes = intersect(names(lucro_liquido2), names(patrimonio_total2))
entr_cx_oper = entr_cx_oper2[,nomes]
lucro_liquido = lucro_liquido2[,nomes]
patrimonio_total = patrimonio_total2[,nomes]


v3.rentabilidade = lucro_liquido
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      v3.rentabilidade[i,j] = lucro_liquido[i,j]/patrimonio_total[i,j]
    }
    else{v3.rentabilidade[i,j] = NA}
    if(is.na(v3.rentabilidade[i,j]) == FALSE){
      if(v3.rentabilidade[i,j] == 0){v3.rentabilidade[i,j] = NA}
    }
  }
}


########## Rentabilidade versão 4(Lucros Líquidos/Patr. Líq.) ##########
nomes = intersect(names(lucro_liquido2), names(patr_liq2))
entr_cx_oper = entr_cx_oper2[,nomes]
lucro_liquido = lucro_liquido2[,nomes]
patr_liq = patr_liq2[,nomes]


v4.rentabilidade = lucro_liquido
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      v4.rentabilidade[i,j] = lucro_liquido[i,j]/patr_liq[i,j]
    }
    else{v4.rentabilidade[i,j] = NA}
    if(is.na(v4.rentabilidade[i,j]) == FALSE){
      if(v4.rentabilidade[i,j] == 0){v4.rentabilidade[i,j] = NA}
    }
  }
}


########## Índice de Cobertura de Juros ##########
nomes = intersect(names(EBITDA2), names(despesa_juros2))
entr_cx_oper = entr_cx_oper2[,nomes]
EBITDA = EBITDA2[,nomes]
despesa_juros = despesa_juros2[,nomes]


ICJ = EBITDA
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      ICJ[i,j] = EBITDA[i,j]/despesa_juros[i,j]
    }
    else{ICJ[i,j] = NA}
    if(is.na(ICJ[i,j]) == FALSE){
      if(ICJ[i,j] == 0){ICJ[i,j] = NA}
    }
  }
}


########## Dívida Líquida/EBITDA ##########
nomes = intersect(names(EBITDA2), names(div_cp2))
nomes = intersect(nomes, names(div_lp2))
entr_cx_oper = entr_cx_oper2[,nomes]
EBITDA = EBITDA2[,nomes]
div_cp = div_cp2[,nomes]
div_lp = div_lp2[,nomes]


div_EBITDA = EBITDA
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      div_EBITDA[i,j] = (div_cp[i,j] + div_lp[i,j])/EBITDA[i,j]
    }
    else{div_EBITDA[i,j] = NA}
    if(is.na(div_EBITDA[i,j]) == FALSE){
      if(div_EBITDA[i,j] == 0){div_EBITDA[i,j] = NA}
    }
  }
}


########## Grau de endividamento ##########
nomes = intersect(names(div_cp2), names(div_lp2))
nomes = intersect(nomes, names(ativo_total2))
entr_cx_oper = entr_cx_oper2[,nomes]
ativo_total = ativo_total2[,nomes]
div_cp = div_cp2[,nomes]
div_lp = div_lp2[,nomes]


GE = ativo_total
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      GE[i,j] = (div_cp[i,j] + div_lp[i,j])/ativo_total[i,j]
    }
    else{GE[i,j] = NA}
    if(is.na(GE[i,j]) == FALSE){
      if(GE[i,j] == 0){GE[i,j] = NA}
    }
  }
}

########## Composição do endividamento ##########
nomes = intersect(names(div_cp2), names(div_lp2))
entr_cx_oper = entr_cx_oper2[,nomes]
div_cp = div_cp2[,nomes]
div_lp = div_lp2[,nomes]


CE = div_cp
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      CE[i,j] = div_cp[i,j]/(div_cp[i,j] + div_lp[i,j])
    }
    else{CE[i,j] = NA}
    if(is.na(CE[i,j]) == FALSE){
      if(CE[i,j] == 0){CE[i,j] = NA}
    }
  }
}


########## Lucros Retidos/Patrimônio Total ##########
nomes = intersect(names(lucros_retidos2), names(patrimonio_total2))
entr_cx_oper = entr_cx_oper2[,nomes]
lucros_retidos = lucros_retidos2[,nomes]
patrimonio_total = patrimonio_total2[,nomes]


lucros_patr = lucros_retidos
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      lucros_patr[i,j] = lucros_retidos[i,j]/patrimonio_total[i,j]
    }
    else{lucros_patr[i,j] = NA}
    if(is.na(lucros_patr[i,j]) == FALSE){
      if(lucros_patr[i,j] == 0){lucros_patr[i,j] = NA}
    }
  }
}


########## Lucros Retidos/Patrimônio Líquido ##########
nomes = intersect(names(lucros_retidos2), names(patr_liq2))
entr_cx_oper = entr_cx_oper2[,nomes]
lucros_retidos = lucros_retidos2[,nomes]
patr_liq = patr_liq2[,nomes]


v2.lucros_patr = lucros_retidos
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      v2.lucros_patr[i,j] = lucros_retidos[i,j]/patr_liq[i,j]
    }
    else{v2.lucros_patr[i,j] = NA}
    if(is.na(v2.lucros_patr[i,j]) == FALSE){
      if(v2.lucros_patr[i,j] == 0){v2.lucros_patr[i,j] = NA}
    }
  }
}


########## Debt/Equity Ratio ##########
nomes = intersect(names(div_lp2), names(patrimonio_total2))
entr_cx_oper = entr_cx_oper2[,nomes]
div_lp = div_lp2[,nomes]
patrimonio_total = patrimonio_total2[,nomes]


DE = div_lp
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      DE[i,j] = div_lp[i,j]/patrimonio_total[i,j]
    }
    else{DE[i,j] = NA}
    if(is.na(DE[i,j]) == FALSE){
      if(DE[i,j] == 0){DE[i,j] = NA}
    }
  }
}


########## Price/Earnings Ratio básico ##########
nomes = names(pl_acao_b2)
entr_cx_oper = entr_cx_oper2[,nomes]
pl_acao_b = pl_acao_b2

PE_basico = pl_acao_b
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      PE_basico[i,j] = pl_acao_b[i,j]
    }
    else{PE_basico[i,j] = NA}
    if(is.na(PE_basico[i,j]) == FALSE){
      if(PE_basico[i,j] == 0){PE_basico[i,j] = NA}
    }
  }
}


########## Price/Earnings Ratio diluído ##########
nomes = names(pl_acao_d2)
entr_cx_oper = entr_cx_oper2[,nomes]
pl_acao_d = pl_acao_d2

PE_diluido = pl_acao_d
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      PE_diluido[i,j] = pl_acao_d[i,j]
    }
    else{PE_diluido[i,j] = NA}
    if(is.na(PE_diluido[i,j]) == FALSE){
      if(PE_diluido[i,j] == 0){PE_diluido[i,j] = NA}
    }
  }
}

########## Dívida em moeda estrangeira/Dívida total ##########
nomes = intersect(names(div_est_tot2), names(div_tot2))
entr_cx_oper = entr_cx_oper2[,nomes]
div_est_tot = div_est_tot2[,nomes]
div_tot = div_tot2[,nomes]

EST = div_tot
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      EST[i,j] = div_est_tot[i,j]/div_tot[i,j]
    }
    else{EST[i,j] = NA}
    if(is.na(EST[i,j]) == FALSE){
      if(EST[i,j] == 0){EST[i,j] = NA}
    }
  }
}

########## Debentures/Dívida total ##########
nomes = intersect(names(deb2), names(div_tot2))
entr_cx_oper = entr_cx_oper2[,nomes]
deb = deb2[,nomes]
div_tot = div_tot2[,nomes]

DEB = div_tot
for(i in 1:nrow(entr_cx_oper2)){
  for(j in 4:length(nomes)){
    if(is.na(entr_cx_oper[i,j]) == FALSE){
      DEB[i,j] = deb[i,j]/div_tot[i,j]
    }
    else{DEB[i,j] = NA}
    if(is.na(DEB[i,j]) == FALSE){
      if(DEB[i,j] == 0){DEB[i,j] = NA}
    }
  }
}

#Cálculo considerando a soma dos dados para todas as empresas
########## Dívida em moeda estrangeira/Dívida total ##########
#div_est_tot3 = div_est_tot2
#div_est_tot4 = colSums(div_est_tot3[,(-1:-3)])

#div_tot3 = div_tot2
#div_tot4 = colSums(div_tot3[,(-1:-3)])

#est_tot = div_est_tot4/div_tot4

########## Debentures/Dívida total ##########
#deb3 = deb2
#deb4 = colSums(deb3[,(-1:-3)])

#div_tot3 = div_tot2
#div_tot4 = colSums(div_tot3[,(-1:-27)])

#deb_tot = deb4/div_tot4

#rm(ativo_total, ativo_total2, despesa_juros, despesa_juros2, div_cp, div_cp2, div_lp, div_lp2,
#   EBITDA, EBITDA2, entr_cx_oper, entr_cx_oper2, lucros_por_acao2, lucros_retidos, lucros_retidos2,
#   patrimonio_total, patrimonio_total2, lucro_liquido, lucro_liquido2, patr_liq, patr_liq2,
#   pl_acao_b2, pl_acao_b, pl_acao_d2, pl_acao_d, i, j, nomes, div_est_tot, div_est_tot2, div_tot, div_tot2, deb, deb2,)

write.xlsx(rentabilidade, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Rentabilidade", row.names = FALSE, showNA = FALSE, append = FALSE)
write.xlsx(v2.rentabilidade, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Rentabilidade (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v3.rentabilidade, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Rentabilidade (v3)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v4.rentabilidade, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Rentabilidade (v4)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ICJ, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Índ. de Cobertura de Juros", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(div_EBITDA, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Div. Líquida - EBITDA", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(GE, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Grau de Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(CE, "Indicadores de Empresas Brasileiras.xlsx", sheetName = "Comp. do Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(lucros_patr, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Lucros Retidos - Patr. Tot.", row.names = FALSE, showNA = FALSE, append = FALSE)
write.xlsx(v2.lucros_patr, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Lucros Retidos - Patr. Líq.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(DE, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Debt - Equity Ratio", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(PE_basico, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Price - Earnings Ratio (v1)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(PE_diluido, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Price - Earnings Ratio (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(div_est2, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "% da Div. em moeda estrangeira", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(EST, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Div. em moeda estrangeira - calculado", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(DEB, "Indicadores de Empresas Brasileiras(2).xlsx", sheetName = "Debentures - Dívida total", row.names = FALSE, showNA = FALSE, append = TRUE)
