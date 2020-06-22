ind.CE_MM = ind.CE2
ind.DE_MM = ind.DE2
ind.div_EBITDA_MM = ind.div_EBITDA2
ind.GE_MM = ind.GE2
ind.ICJ_MM = ind.ICJ2
ind.lucros_patr_MM = ind.lucros_patr2
v2.ind.lucros_patr_MM = v2.ind.lucros_patr2
ind.rentabilidade_MM = ind.rentabilidade2
v2.ind.rentabilidade_MM = v2.ind.rentabilidade2
v3.ind.rentabilidade_MM = v3.ind.rentabilidade2
v4.ind.rentabilidade_MM = v4.ind.rentabilidade2
ind.PE_basico_MM = ind.PE_basico2
ind.PE_diluido_MM = ind.PE_diluido2
ind.div_est_MM = ind.div_est2
ind.EST_MM = ind.EST2
ind.DEB_MM = ind.DEB2

r = 4

ano_i = as.numeric(substring(names(ind.rentabilidade_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.rentabilidade_MM)[length(names(ind.rentabilidade_MM))],1,4))
q_i = as.numeric(substring(names(ind.rentabilidade_MM)[2],7))
q_f = as.numeric(substring(names(ind.rentabilidade_MM)[length(names(ind.rentabilidade_MM))],7))
x = ind.rentabilidade_MM[1,-1]
y = ind.rentabilidade_MM[2,-1]
z = ind.rentabilidade_MM[3,-1]
ind.rentabilidade_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.rentabilidade_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.rentabilidade_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(v2.ind.rentabilidade_MM)[2],1,4))
ano_f = as.numeric(substring(names(v2.ind.rentabilidade_MM)[length(names(v2.ind.rentabilidade_MM))],1,4))
q_i = as.numeric(substring(names(v2.ind.rentabilidade_MM)[2],7))
q_f = as.numeric(substring(names(v2.ind.rentabilidade_MM)[length(names(v2.ind.rentabilidade_MM))],7))
x = v2.ind.rentabilidade_MM[1,-1]
y = v2.ind.rentabilidade_MM[2,-1]
z = v2.ind.rentabilidade_MM[3,-1]
v2.ind.rentabilidade_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v2.ind.rentabilidade_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v2.ind.rentabilidade_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(v3.ind.rentabilidade_MM)[2],1,4))
ano_f = as.numeric(substring(names(v3.ind.rentabilidade_MM)[length(names(v3.ind.rentabilidade_MM))],1,4))
q_i = as.numeric(substring(names(v3.ind.rentabilidade_MM)[2],7))
q_f = as.numeric(substring(names(v3.ind.rentabilidade_MM)[length(names(v3.ind.rentabilidade_MM))],7))
x = v3.ind.rentabilidade_MM[1,-1]
y = v3.ind.rentabilidade_MM[2,-1]
z = v3.ind.rentabilidade_MM[3,-1]
v3.ind.rentabilidade_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v3.ind.rentabilidade_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v3.ind.rentabilidade_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(v4.ind.rentabilidade_MM)[2],1,4))
ano_f = as.numeric(substring(names(v4.ind.rentabilidade_MM)[length(names(v4.ind.rentabilidade_MM))],1,4))
q_i = as.numeric(substring(names(v4.ind.rentabilidade_MM)[2],7))
q_f = as.numeric(substring(names(v4.ind.rentabilidade_MM)[length(names(v4.ind.rentabilidade_MM))],7))
x = v4.ind.rentabilidade_MM[1,-1]
y = v4.ind.rentabilidade_MM[2,-1]
z = v4.ind.rentabilidade_MM[3,-1]
v4.ind.rentabilidade_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v4.ind.rentabilidade_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v4.ind.rentabilidade_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.ICJ_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.ICJ_MM)[length(names(ind.ICJ_MM))],1,4))
q_i = as.numeric(substring(names(ind.ICJ_MM)[2],7))
q_f = as.numeric(substring(names(ind.ICJ_MM)[length(names(ind.ICJ_MM))],7))
x = ind.ICJ_MM[1,-1]
y = ind.ICJ_MM[2,-1]
z = ind.ICJ_MM[3,-1]
ind.ICJ_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.ICJ_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.ICJ_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.div_EBITDA_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.div_EBITDA_MM)[length(names(ind.div_EBITDA_MM))],1,4))
q_i = as.numeric(substring(names(ind.div_EBITDA_MM)[2],7))
q_f = as.numeric(substring(names(ind.div_EBITDA_MM)[length(names(ind.div_EBITDA_MM))],7))
x = ind.div_EBITDA_MM[1,-1]
y = ind.div_EBITDA_MM[2,-1]
z = ind.div_EBITDA_MM[3,-1]
ind.div_EBITDA_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.div_EBITDA_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.div_EBITDA_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.CE_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.CE_MM)[length(names(ind.CE_MM))],1,4))
q_i = as.numeric(substring(names(ind.CE_MM)[2],7))
q_f = as.numeric(substring(names(ind.CE_MM)[length(names(ind.CE_MM))],7))
x = ind.CE_MM[1,-1]
y = ind.CE_MM[2,-1]
z = ind.CE_MM[3,-1]
ind.CE_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.CE_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.CE_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.GE_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.GE_MM)[length(names(ind.GE_MM))],1,4))
q_i = as.numeric(substring(names(ind.GE_MM)[2],7))
q_f = as.numeric(substring(names(ind.GE_MM)[length(names(ind.GE_MM))],7))
x = ind.GE_MM[1,-1]
y = ind.GE_MM[2,-1]
z = ind.GE_MM[3,-1]
ind.GE_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.GE_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.GE_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.lucros_patr_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.lucros_patr_MM)[length(names(ind.lucros_patr_MM))],1,4))
q_i = as.numeric(substring(names(ind.lucros_patr_MM)[2],7))
q_f = as.numeric(substring(names(ind.lucros_patr_MM)[length(names(ind.lucros_patr_MM))],7))
x = ind.lucros_patr_MM[1,-1]
y = ind.lucros_patr_MM[2,-1]
z = ind.lucros_patr_MM[3,-1]
ind.lucros_patr_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.lucros_patr_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.lucros_patr_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(v2.ind.lucros_patr_MM)[2],1,4))
ano_f = as.numeric(substring(names(v2.ind.lucros_patr_MM)[length(names(v2.ind.lucros_patr_MM))],1,4))
q_i = as.numeric(substring(names(v2.ind.lucros_patr_MM)[2],7))
q_f = as.numeric(substring(names(v2.ind.lucros_patr_MM)[length(names(v2.ind.lucros_patr_MM))],7))
x = v2.ind.lucros_patr_MM[1,-1]
y = v2.ind.lucros_patr_MM[2,-1]
z = v2.ind.lucros_patr_MM[3,-1]
v2.ind.lucros_patr_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v2.ind.lucros_patr_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
v2.ind.lucros_patr_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.DE_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.DE_MM)[length(names(ind.DE_MM))],1,4))
q_i = as.numeric(substring(names(ind.DE_MM)[2],7))
q_f = as.numeric(substring(names(ind.DE_MM)[length(names(ind.DE_MM))],7))
x = ind.DE_MM[1,-1]
y = ind.DE_MM[2,-1]
z = ind.DE_MM[3,-1]
ind.DE_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.DE_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.DE_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.PE_basico_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.PE_basico_MM)[length(names(ind.PE_basico_MM))],1,4))
q_i = as.numeric(substring(names(ind.PE_basico_MM)[2],7))
q_f = as.numeric(substring(names(ind.PE_basico_MM)[length(names(ind.PE_basico_MM))],7))
x = ind.PE_basico_MM[1,-1]
y = ind.PE_basico_MM[2,-1]
z = ind.PE_basico_MM[3,-1]
ind.PE_basico_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.PE_basico_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.PE_basico_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.PE_diluido_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.PE_diluido_MM)[length(names(ind.PE_diluido_MM))],1,4))
q_i = as.numeric(substring(names(ind.PE_diluido_MM)[2],7))
q_f = as.numeric(substring(names(ind.PE_diluido_MM)[length(names(ind.PE_diluido_MM))],7))
x = ind.PE_diluido_MM[1,-1]
y = ind.PE_diluido_MM[2,-1]
z = ind.PE_diluido_MM[3,-1]
ind.PE_diluido_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.PE_diluido_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.PE_diluido_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.div_est_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.div_est_MM)[length(names(ind.div_est_MM))],1,4))
q_i = as.numeric(substring(names(ind.div_est_MM)[2],7))
q_f = as.numeric(substring(names(ind.div_est_MM)[length(names(ind.div_est_MM))],7))
x = ind.div_est_MM[1,-1]
y = ind.div_est_MM[2,-1]
z = ind.div_est_MM[3,-1]
ind.div_est_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.div_est_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.div_est_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.EST_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.EST_MM)[length(names(ind.EST_MM))],1,4))
q_i = as.numeric(substring(names(ind.EST_MM)[2],7))
q_f = as.numeric(substring(names(ind.EST_MM)[length(names(ind.EST_MM))],7))
x = ind.EST_MM[1,-1]
y = ind.EST_MM[2,-1]
z = ind.EST_MM[3,-1]
ind.EST_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.EST_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.EST_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted

ano_i = as.numeric(substring(names(ind.DEB_MM)[2],1,4))
ano_f = as.numeric(substring(names(ind.DEB_MM)[length(names(ind.DEB_MM))],1,4))
q_i = as.numeric(substring(names(ind.DEB_MM)[2],7))
q_f = as.numeric(substring(names(ind.DEB_MM)[length(names(ind.DEB_MM))],7))
x = ind.DEB_MM[1,-1]
y = ind.DEB_MM[2,-1]
z = ind.DEB_MM[3,-1]
ind.DEB_MM[1,-1] = sma(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.DEB_MM[2,-1] = sma(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted
ind.DEB_MM[3,-1] = sma(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4), r)$fitted


write.xlsx(ind.rentabilidade_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Rentabilidade", row.names = FALSE, showNA = FALSE, append = FALSE)
write.xlsx(v2.ind.rentabilidade_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Rentabilidade (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v3.ind.rentabilidade_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Rentabilidade (v3)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v4.ind.rentabilidade_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Rentabilidade (v4)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.ICJ_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Índ. de Cobertura de Juros", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_EBITDA_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Div. Líquida - EBITDA", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.GE_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Grau de Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.CE_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Comp. do Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.lucros_patr_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Lucros Retidos - Patr. Tot.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v2.ind.lucros_patr_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Lucros Retidos - Patr. Líq.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DE_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Debt - Equity Ratio", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_basico_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Price - Earnings Ratio (v1)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_diluido_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Price - Earnings Ratio (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_est_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Div. em moeda estrangeira", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.EST_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Div. em moeda estrangeira - calculado", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DEB_MM, "Medidas de Posição (Outliers excluídos - Média Móvel).xlsx", sheetName = "Debentures - Dívida total", row.names = FALSE, showNA = FALSE, append = TRUE)