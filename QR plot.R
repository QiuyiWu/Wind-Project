# Quantile Regression plot

p <- c(0.5, 0.75, 0.95)
label <- c("CA_coast", "ID", "OR_coast", "CO", "TX_GP", "ND_GP", "NC_mountain", "FL_coast","NE_coast", "OH_Erie")
wdir <- c("S", "SW", "W", "NW", "N", "NE", "E", "SE", "S")
xg = seq(-pi, pi, 0.01)
cols <- 4:2

###############################################


QRpred_p = QRpred_f = array(dim = c(10,629,3))
for (i in 1:10){
  #pdf(file=paste("qr.gem.win.era1.loc",i, ".pdf", sep = '')) 
  Wind_p <- data.frame(WS = WS_present[[i]], WD = WD_present[[i]])
  Wind_f <- data.frame(WS = WS_future[[i]], WD = WD_future[[i]])
  s <- 12 ## tunning parameter here
  QRFit_p <- rq(WS ~ pbs(WD, df = s, Boundary.knots = c(-pi,pi)), tau = p, data = Wind_p)
  QRFit_f <- rq(WS ~ pbs(WD, df = s, Boundary.knots = c(-pi,pi)), tau = p, data = Wind_f)
  
  QRpred_p[i,,] <- predict(QRFit_p, data.frame(WD = xg))
  QRpred_f[i,,] <- predict(QRFit_f, data.frame(WD = xg))
  par(mar = c(3.6, 3.6, 1, 0.6))
  plot(xg, QRpred_p[i, ,1], pch = 20, cex = .2, col = "gray", las = 1,
       xlab = "", ylab = "", xaxt = "n", main = label[i], ylim = c(0,20))
  mtext("Wind direction", 1, line = 2)
  mtext("Wind speed (m/s)", 2, line = 2)
  for (j in 1:3) {
    lines(xg,  QRpred_p[i, ,j], col = cols[j])
    #points(thetaG_present2[[i]], WS_WD_quantiles_p2[[i]]$x[, j], col = cols[j], pch = 16, cex = 0.6)
  }
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  
  # xg = seq(-pi, pi, 0.01)
  # for (j in 1:3){
  #   lines(xg, (Weibull_q_wls_f[i,, j] - Weibull_q_wls_p[i,, j]) / Weibull_q_wls_p[i,, j], col = cols[j], lwd = 1, lty = 2)
  # }
  
  legend("topleft", c("median", "75%-quantile", "95%-quantile"), col = cols, pch = 20, bty = "n", title = "Empirical Est")
  # legend("top", c("Quantile Regression", "Binned Weibull + WLS"),
  #        lty = c(1, 2), bty = "n")
  
  #dev.off()
}



QRpred_p = QRpred_f = array(dim = c(10,629,3))
for (i in 1:10){
  Wind_p <- data.frame(WS = WS_present[[i]], WD = WD_present[[i]])
  s <- 12 ## tunning parameter here
  QRFit_p <- rq(WS ~ pbs(WD, df = s, Boundary.knots = c(-pi,pi)), tau = p, data = Wind_p)
  
  QRpred_p[i,,] <- predict(QRFit_p, data.frame(WD = xg))
  par(mar = c(3.6, 3.6, 1, 0.6))
  plot(xg, QRpred_p[i, ,1], pch = 20, cex = .2, col = "gray", las = 1,
       xlab = "", ylab = "", xaxt = "n", main = label.narr[i], 
       ylim = c(0,20))
  mtext("Wind direction", 1, line = 2)
  mtext("Wind speed (m/s)", 2, line = 2)
  for (j in 1:3) {
    lines(xg,  QRpred_p[i, ,j], col = cols[j])
  }
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  
  
  legend("topleft", c("median", "75%-quantile", "95%-quantile"), col = cols, pch = 20, bty = "n", title = "Empirical Est")
  # legend("top", c("Quantile Regression", "Binned Weibull + WLS"),
  #        lty = c(1, 2), bty = "n")

}



# 
# 
 loc.nldas = c(2,4,5,6,7,8,10)
 loc.narr = c(1,3,9)

WD_present <- list(wd.narr.era1.win[[1]],
                   wd.nldas.era1.win[[1]],
                   wd.narr.era1.win[[2]],
                   wd.nldas.era1.win[[2]],
                   wd.nldas.era1.win[[3]],
                   wd.nldas.era1.win[[4]],
                   wd.nldas.era1.win[[5]],
                   wd.nldas.era1.win[[6]],
                   wd.narr.era1.win[[3]],
                   wd.nldas.era1.win[[7]])
WS_present <-  list(ws.narr.era1.win[[1]],
                    ws.nldas.era1.win[[1]],
                    ws.narr.era1.win[[2]],
                    ws.nldas.era1.win[[2]],
                    ws.nldas.era1.win[[3]],
                    ws.nldas.era1.win[[4]],
                    ws.nldas.era1.win[[5]],
                    ws.nldas.era1.win[[6]],
                    ws.narr.era1.win[[3]],
                    ws.nldas.era1.win[[7]])

# 
# WD_future <- list(wd.narr.era3.win[[1]],
#                    wd.nldas.era3.win[[1]],
#                    wd.narr.era3.win[[2]],
#                    wd.nldas.era3.win[[2]],
#                    wd.nldas.era3.win[[3]],
#                    wd.nldas.era3.win[[4]],
#                    wd.nldas.era3.win[[5]],
#                    wd.nldas.era3.win[[6]],
#                    wd.narr.era3.win[[3]],
#                    wd.nldas.era3.win[[7]])
# WS_present <-  list(ws.narr.era3.win[[1]],
#                     ws.nldas.era3.win[[1]],
#                     ws.narr.era3.win[[2]],
#                     ws.nldas.era3.win[[2]],
#                     ws.nldas.era3.win[[3]],
#                     ws.nldas.era3.win[[4]],
#                     ws.nldas.era3.win[[5]],
#                     ws.nldas.era3.win[[6]],
#                     ws.narr.era3.win[[3]],
#                     ws.nldas.era3.win[[7]])
# 

WD_present <- list(wd.narr.era1.sum[[1]],
                   wd.nldas.era1.sum[[1]],
                   wd.narr.era1.sum[[2]],
                   wd.nldas.era1.sum[[2]],
                   wd.nldas.era1.sum[[3]],
                   wd.nldas.era1.sum[[4]],
                   wd.nldas.era1.sum[[5]],
                   wd.nldas.era1.sum[[6]],
                   wd.narr.era1.sum[[3]],
                   wd.nldas.era1.sum[[7]])
WS_present <-  list(ws.narr.era1.sum[[1]],
                    ws.nldas.era1.sum[[1]],
                    ws.narr.era1.sum[[2]],
                    ws.nldas.era1.sum[[2]],
                    ws.nldas.era1.sum[[3]],
                    ws.nldas.era1.sum[[4]],
                    ws.nldas.era1.sum[[5]],
                    ws.nldas.era1.sum[[6]],
                    ws.narr.era1.sum[[3]],
                    ws.nldas.era1.sum[[7]])
# 
# 
# WD_future <- list(wd.narr.era3.sum[[1]],
#                    wd.nldas.era3.sum[[1]],
#                    wd.narr.era3.sum[[2]],
#                    wd.nldas.era3.sum[[2]],
#                    wd.nldas.era3.sum[[3]],
#                    wd.nldas.era3.sum[[4]],
#                    wd.nldas.era3.sum[[5]],
#                    wd.nldas.era3.sum[[6]],
#                    wd.narr.era3.sum[[3]],
#                    wd.nldas.era3.sum[[7]])
# WS_future <-  list(ws.narr.era3.sum[[1]],
#                     ws.nldas.era3.sum[[1]],
#                     ws.narr.era3.sum[[2]],
#                     ws.nldas.era3.sum[[2]],
#                     ws.nldas.era3.sum[[3]],
#                     ws.nldas.era3.sum[[4]],
#                     ws.nldas.era3.sum[[5]],
#                     ws.nldas.era3.sum[[6]],
#                     ws.narr.era3.sum[[3]],
#                     ws.nldas.era3.sum[[7]])

# 
 #  QRpred_p_GFDL = QRpred_p
 #  save(QRpred_p_GFDL, file = "QRpred_p_GFDL.RData")
 # #
 #  QRpred_f_GFDL = QRpred_f
 #  save(QRpred_f_GFDL, file = "QRpred_f_GFDL.RData")

###############################################

#### ALL IN ONE PLOT

for (i in 1:10){
  #pdf(file=paste("qrall.sum.era1.loc",i, ".pdf", sep = '')) 
  par(mar = c(3.6, 3.6, 1, 0.6))
  plot(xg, QRpred_p_CCSM[i, ,1], pch = 20, cex = .2, col = "white", las = 1, 
       xlab = "", ylab = "", xaxt = "n", main = label[i], ylim = c(0,20))
  mtext("Wind direction", 1, line = 2)
  mtext("Wind speed (m/s)", 2, line = 2)
  for (j in c(1,3)) {
    lines(xg, QRpred_p_Bench[i, ,j], col = cols[j], lty = 1)
    lines(xg, QRpred_p_CCSM[i, ,j], col = cols[j], lty = 2)
    lines(xg, QRpred_p_GFDL[i, ,j], col = cols[j], lty = 3)
    lines(xg, QRpred_p_GEM[i, ,j], col = cols[j], lty = 4)
  }
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  
  legend("topleft", c("median", "95%-quantile"), col = c(4,2), pch = 20, bty = "n")
  legend("top", c("Benchmark","CCSM", "GFDL", "GEM"),
         lty = c(1:4), bty = "n")
  
  #dev.off()
}

