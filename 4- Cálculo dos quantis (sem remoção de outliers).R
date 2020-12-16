ind.CE = CE[1:3,-c(1:3)]
ind.DE = DE[1:3,-c(1:3)]
ind.div_EBITDA = div_EBITDA[1:3,-c(1:3)]
ind.GE = GE[1:3,-c(1:3)]
ind.ICJ = ICJ[1:3,-c(1:3)]
ind.lucros_patr = lucros_patr[1:3,-c(1:3)]
v2.ind.lucros_patr = v2.lucros_patr[1:3,-c(1:3)]
ind.rentabilidade = rentabilidade[1:3,-c(1:3)]
v2.ind.rentabilidade = v2.rentabilidade[1:3,-c(1:3)]
v3.ind.rentabilidade = v3.rentabilidade[1:3,-c(1:3)]
v4.ind.rentabilidade = v4.rentabilidade[1:3,-c(1:3)]
ind.PE_basico = PE_basico[1:3,-c(1:3)]
ind.PE_diluido = PE_diluido[1:3,-c(1:3)]
ind.div_est = div_est2[1:3,-c(1:3)]
ind.EST = EST[1:3,-c(1:3)]
ind.DEB = DEB[1:3,-c(1:3)]

vetor = c("Q1", "Mediana", "Q3")

for(i in 1:ncol(ind.CE)){
ind.CE[1,i] = quantile(CE[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.CE[2,i] = quantile(CE[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.CE[3,i] = quantile(CE[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.CE = data.frame(Medida = vetor, ind.CE)
names(ind.CE) = gsub("[.]", " ", names(ind.CE))
names(ind.CE) = gsub("X", "", names(ind.CE))


for(i in 1:ncol(ind.DE)){
ind.DE[1,i] = quantile(DE[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.DE[2,i] = quantile(DE[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.DE[3,i] = quantile(DE[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.DE = data.frame(Medida = vetor, ind.DE)
names(ind.DE) = gsub("[.]", " ", names(ind.DE))
names(ind.DE) = gsub("X", "", names(ind.DE))


for(i in 1:ncol(ind.div_EBITDA)){
ind.div_EBITDA[1,i] = quantile(div_EBITDA[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.div_EBITDA[2,i] = quantile(div_EBITDA[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.div_EBITDA[3,i] = quantile(div_EBITDA[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.div_EBITDA = data.frame(Medida = vetor, ind.div_EBITDA)
names(ind.div_EBITDA) = gsub("[.]", " ", names(ind.div_EBITDA))
names(ind.div_EBITDA) = gsub("X", "", names(ind.div_EBITDA))


for(i in 1:ncol(ind.GE)){
ind.GE[1,i] = quantile(GE[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.GE[2,i] = quantile(GE[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.GE[3,i] = quantile(GE[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.GE = data.frame(Medida = vetor, ind.GE)
names(ind.GE) = gsub("[.]", " ", names(ind.GE))
names(ind.GE) = gsub("X", "", names(ind.GE))


for(i in 1:ncol(ind.ICJ)){
ind.ICJ[1,i] = quantile(ICJ[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.ICJ[2,i] = quantile(ICJ[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.ICJ[3,i] = quantile(ICJ[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.ICJ = data.frame(Medida = vetor, ind.ICJ)
names(ind.ICJ) = gsub("[.]", " ", names(ind.ICJ))
names(ind.ICJ) = gsub("X", "", names(ind.ICJ))


for(i in 1:ncol(ind.lucros_patr)){
ind.lucros_patr[1,i] = quantile(lucros_patr[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.lucros_patr[2,i] = quantile(lucros_patr[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.lucros_patr[3,i] = quantile(lucros_patr[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.lucros_patr = data.frame(Medida = vetor, ind.lucros_patr)
names(ind.lucros_patr) = gsub("[.]", " ", names(ind.lucros_patr))
names(ind.lucros_patr) = gsub("X", "", names(ind.lucros_patr))


for(i in 1:ncol(v2.ind.lucros_patr)){
v2.ind.lucros_patr[1,i] = quantile(v2.lucros_patr[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
v2.ind.lucros_patr[2,i] = quantile(v2.lucros_patr[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
v2.ind.lucros_patr[3,i] = quantile(v2.lucros_patr[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
v2.ind.lucros_patr = data.frame(Medida = vetor, v2.ind.lucros_patr)
names(v2.ind.lucros_patr) = gsub("[.]", " ", names(v2.ind.lucros_patr))
names(v2.ind.lucros_patr) = gsub("X", "", names(v2.ind.lucros_patr))


for(i in 1:ncol(ind.rentabilidade)){
ind.rentabilidade[1,i] = quantile(rentabilidade[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.rentabilidade[2,i] = quantile(rentabilidade[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.rentabilidade[3,i] = quantile(rentabilidade[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.rentabilidade = data.frame(Medida = vetor, ind.rentabilidade)
names(ind.rentabilidade) = gsub("[.]", " ", names(ind.rentabilidade))
names(ind.rentabilidade) = gsub("X", "", names(ind.rentabilidade))


for(i in 1:ncol(v2.ind.rentabilidade)){
v2.ind.rentabilidade[1,i] = quantile(v2.rentabilidade[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
v2.ind.rentabilidade[2,i] = quantile(v2.rentabilidade[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
v2.ind.rentabilidade[3,i] = quantile(v2.rentabilidade[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
v2.ind.rentabilidade = data.frame(Medida = vetor, v2.ind.rentabilidade)
names(v2.ind.rentabilidade) = gsub("[.]", " ", names(v2.ind.rentabilidade))
names(v2.ind.rentabilidade) = gsub("X", "", names(v2.ind.rentabilidade))


for(i in 1:ncol(v3.ind.rentabilidade)){
v3.ind.rentabilidade[1,i] = quantile(v3.rentabilidade[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
v3.ind.rentabilidade[2,i] = quantile(v3.rentabilidade[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
v3.ind.rentabilidade[3,i] = quantile(v3.rentabilidade[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
v3.ind.rentabilidade = data.frame(Medida = vetor, v3.ind.rentabilidade)
names(v3.ind.rentabilidade) = gsub("[.]", " ", names(v3.ind.rentabilidade))
names(v3.ind.rentabilidade) = gsub("X", "", names(v3.ind.rentabilidade))


for(i in 1:ncol(v4.ind.rentabilidade)){
v4.ind.rentabilidade[1,i] = quantile(v4.rentabilidade[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
v4.ind.rentabilidade[2,i] = quantile(v4.rentabilidade[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
v4.ind.rentabilidade[3,i] = quantile(v4.rentabilidade[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
v4.ind.rentabilidade = data.frame(Medida = vetor, v4.ind.rentabilidade)
names(v4.ind.rentabilidade) = gsub("[.]", " ", names(v4.ind.rentabilidade))
names(v4.ind.rentabilidade) = gsub("X", "", names(v4.ind.rentabilidade))


for(i in 1:ncol(ind.PE_basico)){
ind.PE_basico[1,i] = quantile(PE_basico[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.PE_basico[2,i] = quantile(PE_basico[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.PE_basico[3,i] = quantile(PE_basico[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.PE_basico = data.frame(Medida = vetor, ind.PE_basico)
names(ind.PE_basico) = gsub("[.]", " ", names(ind.PE_basico))
names(ind.PE_basico) = gsub("X", "", names(ind.PE_basico))


for(i in 1:ncol(ind.PE_diluido)){
ind.PE_diluido[1,i] = quantile(PE_diluido[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.PE_diluido[2,i] = quantile(PE_diluido[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.PE_diluido[3,i] = quantile(PE_diluido[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.PE_diluido = data.frame(Medida = vetor, ind.PE_diluido)
names(ind.PE_diluido) = gsub("[.]", " ", names(ind.PE_diluido))
names(ind.PE_diluido) = gsub("X", "", names(ind.PE_diluido))

for(i in 1:ncol(ind.div_est)){
ind.div_est[1,i] = quantile(div_est2[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
ind.div_est[2,i] = quantile(div_est2[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
ind.div_est[3,i] = quantile(div_est2[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.div_est = data.frame(Medida = vetor, ind.div_est)
names(ind.div_est) = gsub("[.]", " ", names(ind.div_est))
names(ind.div_est) = gsub("X", "", names(ind.div_est))

for(i in 1:ncol(ind.EST)){
  ind.EST[1,i] = quantile(EST[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
  ind.EST[2,i] = quantile(EST[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
  ind.EST[3,i] = quantile(EST[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.EST = data.frame(Medida = vetor, ind.EST)
names(ind.EST) = gsub("[.]", " ", names(ind.EST))
names(ind.EST) = gsub("X", "", names(ind.EST))

for(i in 1:ncol(ind.DEB)){
  ind.DEB[1,i] = quantile(DEB[,(i+3)], probs = 0.25, na.rm = TRUE, names = TRUE)
  ind.DEB[2,i] = quantile(DEB[,(i+3)], probs = 0.5, na.rm = TRUE, names = TRUE)
  ind.DEB[3,i] = quantile(DEB[,(i+3)], probs = 0.75, na.rm = TRUE, names = TRUE)
}
ind.DEB = data.frame(Medida = vetor, ind.DEB)
names(ind.DEB) = gsub("[.]", " ", names(ind.DEB))
names(ind.DEB) = gsub("X", "", names(ind.DEB))


write.xlsx(ind.rentabilidade, "Medidas de Posição.xlsx", sheetName = "Rentabilidade", row.names = FALSE, showNA = FALSE, append = FALSE)
write.xlsx(v2.ind.rentabilidade, "Medidas de Posição.xlsx", sheetName = "Rentabilidade (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v3.ind.rentabilidade, "Medidas de Posição.xlsx", sheetName = "Rentabilidade (v3)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v4.ind.rentabilidade, "Medidas de Posição.xlsx", sheetName = "Rentabilidade (v4)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.ICJ, "Medidas de Posição.xlsx", sheetName = "Índ. de Cobertura de Juros", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_EBITDA, "Medidas de Posição.xlsx", sheetName = "Div. Líquida - EBITDA", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.GE, "Medidas de Posição.xlsx", sheetName = "Grau de Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.CE, "Medidas de Posição.xlsx", sheetName = "Comp. do Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.lucros_patr, "Medidas de Posição.xlsx", sheetName = "Lucros Retidos - Patr. Tot.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v2.ind.lucros_patr, "Medidas de Posição.xlsx", sheetName = "Lucros Retidos - Patr. Líq.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DE, "Medidas de Posição.xlsx", sheetName = "Debt - Equity Ratio", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_basico, "Medidas de Posição.xlsx", sheetName = "Price - Earnings Ratio (v1)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_diluido, "Medidas de Posição.xlsx", sheetName = "Price - Earnings Ratio (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_est, "Medidas de Posição.xlsx", sheetName = "Div. em moeda estrangeira", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.EST, "Medidas de Posição.xlsx", sheetName = "Div. em moeda estrangeira - calculado", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DEB, "Medidas de Posição.xlsx", sheetName = "Debentures - Dívida total", row.names = FALSE, showNA = FALSE, append = TRUE)