load("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output/vor_sanierung.RData")
s0 <- dl_misa

load("Y:/AUFTRAEGE/_Auftraege_laufend/MISA3/Exchange/Misa_auswertung/output/s0.RData")
vs <- dl_misa

plot(x = s0$E2$hours$below_0.5, y = vs$E2$hours$below_0.5, pch = 20)
