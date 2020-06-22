ind.CE_SARIMA = ind.CE2
ind.DE_SARIMA = ind.DE2
ind.div_EBITDA_SARIMA = ind.div_EBITDA2
ind.GE_SARIMA = ind.GE2
ind.ICJ_SARIMA = ind.ICJ2
ind.lucros_patr_SARIMA = ind.lucros_patr2
v2.ind.lucros_patr_SARIMA = v2.ind.lucros_patr2
ind.rentabilidade_SARIMA = ind.rentabilidade2
v2.ind.rentabilidade_SARIMA = v2.ind.rentabilidade2
v3.ind.rentabilidade_SARIMA = v3.ind.rentabilidade2
v4.ind.rentabilidade_SARIMA = v4.ind.rentabilidade2
ind.PE_basico_SARIMA = ind.PE_basico2
ind.PE_diluido_SARIMA = ind.PE_diluido2
ind.div_est_SARIMA = ind.div_est2
ind.EST_SARIMA = ind.EST2
ind.DEB_SARIMA = ind.DEB2

ano_i = as.numeric(substring(names(ind.rentabilidade_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.rentabilidade_SARIMA)[length(names(ind.rentabilidade_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.rentabilidade_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.rentabilidade_SARIMA)[length(names(ind.rentabilidade_SARIMA))],7))
x = ind.rentabilidade_SARIMA[1,-1]
y = ind.rentabilidade_SARIMA[2,-1]
z = ind.rentabilidade_SARIMA[3,-1]
ind.rentabilidade_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.rentabilidade_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.rentabilidade_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(v2.ind.rentabilidade_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(v2.ind.rentabilidade_SARIMA)[length(names(v2.ind.rentabilidade_SARIMA))],1,4))
q_i = as.numeric(substring(names(v2.ind.rentabilidade_SARIMA)[2],7))
q_f = as.numeric(substring(names(v2.ind.rentabilidade_SARIMA)[length(names(v2.ind.rentabilidade_SARIMA))],7))
x = v2.ind.rentabilidade_SARIMA[1,-1]
y = v2.ind.rentabilidade_SARIMA[2,-1]
z = v2.ind.rentabilidade_SARIMA[3,-1]
v2.ind.rentabilidade_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v2.ind.rentabilidade_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v2.ind.rentabilidade_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(v3.ind.rentabilidade_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(v3.ind.rentabilidade_SARIMA)[length(names(v3.ind.rentabilidade_SARIMA))],1,4))
q_i = as.numeric(substring(names(v3.ind.rentabilidade_SARIMA)[2],7))
q_f = as.numeric(substring(names(v3.ind.rentabilidade_SARIMA)[length(names(v3.ind.rentabilidade_SARIMA))],7))
x = v3.ind.rentabilidade_SARIMA[1,-1]
y = v3.ind.rentabilidade_SARIMA[2,-1]
z = v3.ind.rentabilidade_SARIMA[3,-1]
v3.ind.rentabilidade_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v3.ind.rentabilidade_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v3.ind.rentabilidade_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(v4.ind.rentabilidade_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(v4.ind.rentabilidade_SARIMA)[length(names(v4.ind.rentabilidade_SARIMA))],1,4))
q_i = as.numeric(substring(names(v4.ind.rentabilidade_SARIMA)[2],7))
q_f = as.numeric(substring(names(v4.ind.rentabilidade_SARIMA)[length(names(v4.ind.rentabilidade_SARIMA))],7))
x = v4.ind.rentabilidade_SARIMA[1,-1]
y = v4.ind.rentabilidade_SARIMA[2,-1]
z = v4.ind.rentabilidade_SARIMA[3,-1]
v4.ind.rentabilidade_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v4.ind.rentabilidade_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v4.ind.rentabilidade_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.ICJ_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.ICJ_SARIMA)[length(names(ind.ICJ_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.ICJ_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.ICJ_SARIMA)[length(names(ind.ICJ_SARIMA))],7))
x = ind.ICJ_SARIMA[1,-1]
y = ind.ICJ_SARIMA[2,-1]
z = ind.ICJ_SARIMA[3,-1]
ind.ICJ_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.ICJ_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.ICJ_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.div_EBITDA_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.div_EBITDA_SARIMA)[length(names(ind.div_EBITDA_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.div_EBITDA_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.div_EBITDA_SARIMA)[length(names(ind.div_EBITDA_SARIMA))],7))
x = ind.div_EBITDA_SARIMA[1,-1]
y = ind.div_EBITDA_SARIMA[2,-1]
z = ind.div_EBITDA_SARIMA[3,-1]
ind.div_EBITDA_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.div_EBITDA_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.div_EBITDA_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.CE_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.CE_SARIMA)[length(names(ind.CE_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.CE_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.CE_SARIMA)[length(names(ind.CE_SARIMA))],7))
x = ind.CE_SARIMA[1,-1]
y = ind.CE_SARIMA[2,-1]
z = ind.CE_SARIMA[3,-1]
ind.CE_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.CE_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.CE_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.GE_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.GE_SARIMA)[length(names(ind.GE_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.GE_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.GE_SARIMA)[length(names(ind.GE_SARIMA))],7))
x = ind.GE_SARIMA[1,-1]
y = ind.GE_SARIMA[2,-1]
z = ind.GE_SARIMA[3,-1]
ind.GE_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.GE_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.GE_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.lucros_patr_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.lucros_patr_SARIMA)[length(names(ind.lucros_patr_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.lucros_patr_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.lucros_patr_SARIMA)[length(names(ind.lucros_patr_SARIMA))],7))
x = ind.lucros_patr_SARIMA[1,-1]
y = ind.lucros_patr_SARIMA[2,-1]
z = ind.lucros_patr_SARIMA[3,-1]
ind.lucros_patr_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.lucros_patr_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.lucros_patr_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(v2.ind.lucros_patr_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(v2.ind.lucros_patr_SARIMA)[length(names(v2.ind.lucros_patr_SARIMA))],1,4))
q_i = as.numeric(substring(names(v2.ind.lucros_patr_SARIMA)[2],7))
q_f = as.numeric(substring(names(v2.ind.lucros_patr_SARIMA)[length(names(v2.ind.lucros_patr_SARIMA))],7))
x = v2.ind.lucros_patr_SARIMA[1,-1]
y = v2.ind.lucros_patr_SARIMA[2,-1]
z = v2.ind.lucros_patr_SARIMA[3,-1]
v2.ind.lucros_patr_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v2.ind.lucros_patr_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
v2.ind.lucros_patr_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.DE_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.DE_SARIMA)[length(names(ind.DE_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.DE_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.DE_SARIMA)[length(names(ind.DE_SARIMA))],7))
x = ind.DE_SARIMA[1,-1]
y = ind.DE_SARIMA[2,-1]
z = ind.DE_SARIMA[3,-1]
ind.DE_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.DE_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.DE_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.PE_basico_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.PE_basico_SARIMA)[length(names(ind.PE_basico_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.PE_basico_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.PE_basico_SARIMA)[length(names(ind.PE_basico_SARIMA))],7))
x = ind.PE_basico_SARIMA[1,-1]
y = ind.PE_basico_SARIMA[2,-1]
z = ind.PE_basico_SARIMA[3,-1]
ind.PE_basico_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.PE_basico_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.PE_basico_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.PE_diluido_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.PE_diluido_SARIMA)[length(names(ind.PE_diluido_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.PE_diluido_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.PE_diluido_SARIMA)[length(names(ind.PE_diluido_SARIMA))],7))
x = ind.PE_diluido_SARIMA[1,-1]
y = ind.PE_diluido_SARIMA[2,-1]
z = ind.PE_diluido_SARIMA[3,-1]
ind.PE_diluido_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.PE_diluido_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.PE_diluido_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.div_est_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.div_est_SARIMA)[length(names(ind.div_est_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.div_est_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.div_est_SARIMA)[length(names(ind.div_est_SARIMA))],7))
x = ind.div_est_SARIMA[1,-1]
y = ind.div_est_SARIMA[2,-1]
z = ind.div_est_SARIMA[3,-1]
ind.div_est_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.div_est_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.div_est_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.EST_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.EST_SARIMA)[length(names(ind.EST_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.EST_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.EST_SARIMA)[length(names(ind.EST_SARIMA))],7))
x = ind.EST_SARIMA[1,-1]
y = ind.EST_SARIMA[2,-1]
z = ind.EST_SARIMA[3,-1]
ind.EST_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.EST_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.EST_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted

ano_i = as.numeric(substring(names(ind.DEB_SARIMA)[2],1,4))
ano_f = as.numeric(substring(names(ind.DEB_SARIMA)[length(names(ind.DEB_SARIMA))],1,4))
q_i = as.numeric(substring(names(ind.DEB_SARIMA)[2],7))
q_f = as.numeric(substring(names(ind.DEB_SARIMA)[length(names(ind.DEB_SARIMA))],7))
x = ind.DEB_SARIMA[1,-1]
y = ind.DEB_SARIMA[2,-1]
z = ind.DEB_SARIMA[3,-1]
ind.DEB_SARIMA[1,-1] = auto.arima(ts(as.vector(x, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.DEB_SARIMA[2,-1] = auto.arima(ts(as.vector(y, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted
ind.DEB_SARIMA[3,-1] = auto.arima(ts(as.vector(z, mode = "numeric"), start = c(ano_i,q_i), end = c(ano_f,q_f), deltat = 1/4))$fitted




write.xlsx(ind.rentabilidade_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Rentabilidade", row.names = FALSE, showNA = FALSE, append = FALSE)
write.xlsx(v2.ind.rentabilidade_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Rentabilidade (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v3.ind.rentabilidade_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Rentabilidade (v3)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v4.ind.rentabilidade_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Rentabilidade (v4)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.ICJ_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Índ. de Cobertura de Juros", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_EBITDA_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Div. Líquida - EBITDA", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.GE_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Grau de Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.CE_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Comp. do Endividamento", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.lucros_patr_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Lucros Retidos - Patr. Tot.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(v2.ind.lucros_patr_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Lucros Retidos - Patr. Líq.", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DE_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Debt - Equity Ratio", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_basico_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Price - Earnings Ratio (v1)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.PE_diluido_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Price - Earnings Ratio (v2)", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.div_est_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Div. em moeda estrangeira", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.EST_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Div. em moeda estrangeira - calculado", row.names = FALSE, showNA = FALSE, append = TRUE)
write.xlsx(ind.DEB_SARIMA, "Medidas de Posição (Outliers excluídos - SARIMA).xlsx", sheetName = "Debentures - Dívida total", row.names = FALSE, showNA = FALSE, append = TRUE)