ind.CE2 = CE[1:3,-c(1:3)]
ind.DE2 = DE[1:3,-c(1:3)]
ind.div_EBITDA2 = div_EBITDA[1:3,-c(1:3)]
ind.GE2 = GE[1:3,-c(1:3)]
ind.ICJ2 = ICJ[1:3,-c(1:3)]
ind.lucros_patr2 = lucros_patr[1:3,-c(1:3)]
v2.ind.lucros_patr2 = v2.lucros_patr[1:3,-c(1:3)]
ind.rentabilidade2 = rentabilidade[1:3,-c(1:3)]
v2.ind.rentabilidade2 = v2.rentabilidade[1:3,-c(1:3)]
v3.ind.rentabilidade2 = v3.rentabilidade[1:3,-c(1:3)]
v4.ind.rentabilidade2 = v4.rentabilidade[1:3,-c(1:3)]
ind.PE_basico2 = PE_basico[1:3,-c(1:3)]
ind.PE_diluido2 = PE_diluido[1:3,-c(1:3)]
ind.div_est2 = div_est2[1:3,-c(1:3)]
ind.EST2 = EST[1:3,-c(1:3)]
ind.DEB2 = DEB[1:3,-c(1:3)]

vetor = c("Q1", "Mediana", "Q3")

for(i in 1:ncol(ind.CE2)){
  x = CE[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.CE2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.CE2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.CE2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.CE2 = data.frame(Medida = vetor, ind.CE2)
names(ind.CE2) = gsub("[.]", " ", names(ind.CE2))
names(ind.CE2) = gsub("X", "", names(ind.CE2))

for(i in 1:ncol(ind.DE2)){
  x = DE[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.DE2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.DE2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.DE2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.DE2 = data.frame(Medida = vetor, ind.DE2)
names(ind.DE2) = gsub("[.]", " ", names(ind.DE2))
names(ind.DE2) = gsub("X", "", names(ind.DE2))

for(i in 1:ncol(ind.div_EBITDA2)){
  x = div_EBITDA[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.div_EBITDA2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.div_EBITDA2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.div_EBITDA2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.div_EBITDA2 = data.frame(Medida = vetor, ind.div_EBITDA2)
names(ind.div_EBITDA2) = gsub("[.]", " ", names(ind.div_EBITDA2))
names(ind.div_EBITDA2) = gsub("X", "", names(ind.div_EBITDA2))

for(i in 1:ncol(ind.GE2)){
  x = GE[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.GE2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.GE2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.GE2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.GE2 = data.frame(Medida = vetor, ind.GE2)
names(ind.GE2) = gsub("[.]", " ", names(ind.GE2))
names(ind.GE2) = gsub("X", "", names(ind.GE2))

for(i in 1:ncol(ind.ICJ2)){
  x = ICJ[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.ICJ2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.ICJ2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.ICJ2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.ICJ2 = data.frame(Medida = vetor, ind.ICJ2)
names(ind.ICJ2) = gsub("[.]", " ", names(ind.ICJ2))
names(ind.ICJ2) = gsub("X", "", names(ind.ICJ2))

for(i in 1:ncol(ind.lucros_patr2)){
  x = lucros_patr[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.lucros_patr2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.lucros_patr2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.lucros_patr2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.lucros_patr2 = data.frame(Medida = vetor, ind.lucros_patr2)
names(ind.lucros_patr2) = gsub("[.]", " ", names(ind.lucros_patr2))
names(ind.lucros_patr2) = gsub("X", "", names(ind.lucros_patr2))

for(i in 1:ncol(v2.ind.lucros_patr2)){
  x = v2.lucros_patr[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  v2.ind.lucros_patr2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  v2.ind.lucros_patr2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  v2.ind.lucros_patr2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
v2.ind.lucros_patr2 = data.frame(Medida = vetor, v2.ind.lucros_patr2)
names(v2.ind.lucros_patr2) = gsub("[.]", " ", names(v2.ind.lucros_patr2))
names(v2.ind.lucros_patr2) = gsub("X", "", names(v2.ind.lucros_patr2))

for(i in 1:ncol(ind.rentabilidade2)){
  x = rentabilidade[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.rentabilidade2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.rentabilidade2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.rentabilidade2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.rentabilidade2 = data.frame(Medida = vetor, ind.rentabilidade2)
names(ind.rentabilidade2) = gsub("[.]", " ", names(ind.rentabilidade2))
names(ind.rentabilidade2) = gsub("X", "", names(ind.rentabilidade2))

for(i in 1:ncol(v2.ind.rentabilidade2)){
  x = v2.rentabilidade[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  v2.ind.rentabilidade2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  v2.ind.rentabilidade2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  v2.ind.rentabilidade2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
v2.ind.rentabilidade2 = data.frame(Medida = vetor, v2.ind.rentabilidade2)
names(v2.ind.rentabilidade2) = gsub("[.]", " ", names(v2.ind.rentabilidade2))
names(v2.ind.rentabilidade2) = gsub("X", "", names(v2.ind.rentabilidade2))

for(i in 1:ncol(v3.ind.rentabilidade2)){
  x = v3.rentabilidade[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  v3.ind.rentabilidade2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  v3.ind.rentabilidade2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  v3.ind.rentabilidade2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
v3.ind.rentabilidade2 = data.frame(Medida = vetor, v3.ind.rentabilidade2)
names(v3.ind.rentabilidade2) = gsub("[.]", " ", names(v3.ind.rentabilidade2))
names(v3.ind.rentabilidade2) = gsub("X", "", names(v3.ind.rentabilidade2))

for(i in 1:ncol(v4.ind.rentabilidade2)){
  x = v4.rentabilidade[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  v4.ind.rentabilidade2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  v4.ind.rentabilidade2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  v4.ind.rentabilidade2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
v4.ind.rentabilidade2 = data.frame(Medida = vetor, v4.ind.rentabilidade2)
names(v4.ind.rentabilidade2) = gsub("[.]", " ", names(v4.ind.rentabilidade2))
names(v4.ind.rentabilidade2) = gsub("X", "", names(v4.ind.rentabilidade2))

for(i in 1:ncol(ind.PE_basico2)){
  x = PE_basico[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.PE_basico2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.PE_basico2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.PE_basico2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.PE_basico2 = data.frame(Medida = vetor, ind.PE_basico2)
names(ind.PE_basico2) = gsub("[.]", " ", names(ind.PE_basico2))
names(ind.PE_basico2) = gsub("X", "", names(ind.PE_basico2))

for(i in 1:ncol(ind.PE_diluido2)){
  x = PE_diluido[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.PE_diluido2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.PE_diluido2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.PE_diluido2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.PE_diluido2 = data.frame(Medida = vetor, ind.PE_diluido2)
names(ind.PE_diluido2) = gsub("[.]", " ", names(ind.PE_diluido2))
names(ind.PE_diluido2) = gsub("X", "", names(ind.PE_diluido2))

for(i in 1:ncol(ind.div_est2)){
  x = PE_diluido[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.div_est2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.div_est2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.div_est2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.div_est2 = data.frame(Medida = vetor, ind.div_est2)
names(ind.div_est2) = gsub("[.]", " ", names(ind.div_est2))
names(ind.div_est2) = gsub("X", "", names(ind.div_est2))

for(i in 1:ncol(ind.EST2)){
  x = CE[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.EST2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.EST2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.EST2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.EST2 = data.frame(Medida = vetor, ind.EST2)
names(ind.EST2) = gsub("[.]", " ", names(ind.EST2))
names(ind.EST2) = gsub("X", "", names(ind.EST2))

for(i in 1:ncol(ind.DEB2)){
  x = CE[,(i+3)]
  if(length(which(abs(x) == Inf)) != 0){x = x[-which(abs(x) == Inf)]}
  ai = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) - quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  li = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE) - (1.5*ai)
  ls = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE) + (1.5*ai)
  x = x[which(x >= li & x <= ls)]
  ind.DEB2[1,i] = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  ind.DEB2[2,i] = quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE)
  ind.DEB2[3,i] = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
ind.DEB2 = data.frame(Medida = vetor, ind.DEB2)
names(ind.DEB2) = gsub("[.]", " ", names(ind.DEB2))
names(ind.DEB2) = gsub("X", "", names(ind.DEB2))


write.xlsx(ind.rentabilidade2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Rentabilidade", row.names = FALSE, showNA = FALSE, append = FALSE)
write.xlsx(v2.ind.rentabilidade2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Rentabilidade (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v3.ind.rentabilidade2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Rentabilidade (v3)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v4.ind.rentabilidade2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Rentabilidade (v4)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.ICJ2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Índ. de Cobertura de Juros", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_EBITDA2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Div. Líquida - EBITDA", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.GE2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Grau de Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.CE2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Comp. do Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.lucros_patr2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Lucros Retidos - Patr. Tot.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v2.ind.lucros_patr2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Lucros Retidos - Patr. Líq.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DE2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Debt - Equity Ratio", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_basico2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Price - Earnings Ratio (v1)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_diluido2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Price - Earnings Ratio (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_est2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Div. em moeda estrangeira", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.EST2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Div. em moeda estrangeira - calculado", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DEB2, "Medidas de Posição (Outliers excluídos).xlsx", sheetName = "Debentures - Dívida total", row.names = FALSE, showNA = FALSE, append = TRUE)