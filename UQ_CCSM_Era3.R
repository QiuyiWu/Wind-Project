
# 95% range: 2.5% x upper bound & 2.5% x lower bound 

new.max = function(x){quantile(x, probs = 0.975, na.rm = T)}
new.min = function(x){quantile(x, probs = 0.025, na.rm = T)}

n=500
# Weibull Data preprocessing
WB.50 = WB.95 = array(dim = c(10,n,629))
for (k in 1:n) {
  for (i in 1:10) {
    WB.50[i,k,] <- WB.f.sim[[k]][i,, 1]
    WB.95[i,k,] <- WB.f.sim[[k]][i,, 3]
  }
}
WB.50.U = WB.50.L = WB.95.U = WB.95.L = array(dim = c(10,629))
for (i in 1:10) {
  WB.50.U[i,] <- apply(WB.50[i,,],2,new.max)
  WB.50.L[i,] <- apply(WB.50[i,,],2,new.min)
  WB.95.U[i,] <- apply(WB.95[i,,],2,new.max)
  WB.95.L[i,] <- apply(WB.95[i,,],2,new.min)
}
# Quantile Regression Data preprocessing
QR.50 = QR.95 = array(dim = c(10,n,629))
for (k in 1:n) {
  for (i in 1:10) {
    QR.50[i,k,] <- QR.f.sim[[k]][i,, 1]
    QR.95[i,k,] <- QR.f.sim[[k]][i,, 3]
  }
}
QR.50.U = QR.50.L = QR.95.U = QR.95.L = array(dim = c(10,629))
for (i in 1:10) {
  QR.50.U[i,] <- apply(QR.50[i,,],2,new.max)
  QR.50.L[i,] <- apply(QR.50[i,,],2,new.min)
  QR.95.U[i,] <- apply(QR.95[i,,],2,new.max)
  QR.95.L[i,] <- apply(QR.95[i,,],2,new.min)
}






# Quantile Regression Data preprocessing (EMPIRICAL POINTS)
QR.pt.50 = QR.pt.95 = array(dim = c(10,n,18))
for (k in 1:n) {
  for (i in 1:10) {
    QR.pt.50[i,k,] <- QR.f.sim.emp[[k]][[i]]$x[, 1]
    QR.pt.95[i,k,] <- QR.f.sim.emp[[k]][[i]]$x[, 3]
  }
}
QR.pt.50.U = QR.pt.50.L = QR.pt.95.U = QR.pt.95.L = array(dim = c(10,18))
for (i in 1:10) {
  QR.pt.50.U[i,] <- apply(QR.pt.50[i,,],2,new.max)
  QR.pt.50.L[i,] <- apply(QR.pt.50[i,,],2,new.min)
  QR.pt.95.U[i,] <- apply(QR.pt.95[i,,],2,new.max)
  QR.pt.95.L[i,] <- apply(QR.pt.95[i,,],2,new.min)
}


# Weibull Data preprocessing (EMPIRICAL POINTS)
WB.pt.50 =WB.pt.95 = array(dim = c(10,n,18))
for (k in 1:n) {
  for (i in 1:10) {
    WB.pt.50[i,k,] <- WB.f.sim.emp[[k]][[i]]$x[, 1]
    WB.pt.95[i,k,] <- WB.f.sim.emp[[k]][[i]]$x[, 3]
  }
}
WB.pt.50.U = WB.pt.50.L = WB.pt.95.U = WB.pt.95.L = array(dim = c(10,18))
for (i in 1:10) {
  WB.pt.50.U[i,] <- apply(WB.pt.50[i,,],2,new.max)
  WB.pt.50.L[i,] <- apply(WB.pt.50[i,,],2,new.min)
  WB.pt.95.U[i,] <- apply(WB.pt.95[i,,],2,new.max)
  WB.pt.95.L[i,] <- apply(WB.pt.95[i,,],2,new.min)
}




# WB: quantile 50
for (i in 1:10){
  pdf(file=paste("uq_wb.f.ccsm.win.loc",i, ".50.pdf", sep = ''))
  
  plot(xg,Weibull_q_wls_f[i,, 1], 
       type = "l", las = 1, xlab = "Wind direction", ylab = "Wind Speed (m/s)", xaxt = "n", 
       ylim = c(1, 15), 
       main = paste("WB 50%Q: ",label[i]))
  polygon(c(xg, rev(xg)), c(WB.50.L[i,], rev(WB.50.U[i,])), density = 50, col = "grey75", border = NA)
  lines(xg, Weibull_q_wls_f[i,, 1], col='red', lwd=3)
  lines(xg, WB.50.U[i,], col = 'red', lty = 'dashed')     
  lines(xg, WB.50.L[i,], col = 'red', lty = 'dashed')     
  par(new=T)
  plotrix::plotCI(thetaG_future[[i]], WS_WD_quantiles_f[[i]]$x[, 1], 
         ui = WB.pt.50.U[i,], li = WB.pt.50.L[i,], ylim = c(1, 15), 
         pch = 16, cex = 0.6, 
         ylab="", xlab="", xaxt='n', yaxt='n')
  
  
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  dev.off() 
  
  
  pdf(file=paste("uq_wb.f.ccsm.win.loc",i, ".95.pdf", sep = ''))
  # WB: quantile 95
  plot(xg, Weibull_q_wls_f[i,, 3], 
       type = "l", las = 1, xlab = "Wind direction", ylab = "Wind Speed (m/s)", xaxt = "n", 
       ylim = c(3, 20), 
       main = paste("WB 95%Q: ",label[i]))
  polygon(c(xg, rev(xg)), c(WB.95.L[i,], rev(WB.95.U[i,])), density = 50, col = "grey75", border = NA)
  lines(xg, Weibull_q_wls_f[i,, 3], col='red', lwd=3)
  lines(xg, WB.95.U[i,], col = 'red', lty = 'dashed')     
  lines(xg, WB.95.L[i,], col = 'red', lty = 'dashed') 
  par(new=T)
  plotrix::plotCI(thetaG_future[[i]], WS_WD_quantiles_f[[i]]$x[, 3], 
         ui = WB.pt.95.U[i,], li = WB.pt.95.L[i,], ylim = c(3, 20), 
         pch = 16, cex = 0.6, 
         ylab="", xlab="", xaxt='n', yaxt='n')
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  
  dev.off() 
}








#xg <- seq(-0.995 * pi, 0.995 * pi, 0.01)
for (i in 1:10) {
  pdf(file=paste("uq_qr.f.ccsm.win.loc",i, ".50.pdf", sep = ''))
  # QR: quantile 50
  plot(xg, QRpred_f[i,, 1], 
       type = "l", las = 1, xlab = "Wind direction", ylab = "Wind Speed (m/s)", xaxt = "n", 
       ylim = c(1, 15), main = paste("QR 50%Q: ",label[i]))
  polygon(c(xg, rev(xg)), c(QR.50.L[i,], rev(QR.50.U[i,])), density = 50, col = "grey75", border = NA)
  lines(xg, QRpred_f[i,, 1], col='red', lwd=3)
  lines(xg, QR.50.U[i,], col = 'red', lty = 'dashed')     
  lines(xg, QR.50.L[i,], col = 'red', lty = 'dashed') 
  par(new=T)
  plotrix::plotCI(thetaG_future2[[i]], WS_WD_quantiles_f2[[i]]$x[, 1], 
         ui = QR.pt.50.U[i,], li = QR.pt.50.L[i,], ylim = c(1, 15), pch = 16, cex = 0.6, 
         ylab="", xlab="", xaxt='n', yaxt='n')
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  dev.off() 
  
  
  pdf(file=paste("uq_qr.f.ccsm.win.loc",i, ".95.pdf", sep = ''))
  # QR: quantile 95
  plot(xg, QRpred_f[i,, 3], 
       type = "l", las = 1, xlab = "Wind direction", ylab = "Wind Speed (m/s)", xaxt = "n", 
       ylim = c(5, 20), main = paste("QR 95%Q: ",label[i]))
  polygon(c(xg, rev(xg)), c(QR.95.L[i,], rev(QR.95.U[i,])), density = 50, col = "grey75", border = NA)
  lines(xg, QRpred_f[i,, 3], col='red', lwd=3)
  lines(xg, QR.95.U[i,], col = 'red', lty = 'dashed')     
  lines(xg, QR.95.L[i,], col = 'red', lty = 'dashed')  
  par(new=T)
  plotrix::plotCI(thetaG_future2[[i]], WS_WD_quantiles_f2[[i]]$x[, 3], 
         ui = QR.pt.95.U[i,], li = QR.pt.95.L[i,], ylim = c(5, 20), pch = 16, cex = 0.6, 
         ylab="", xlab="", xaxt='n', yaxt='n')
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  dev.off() 
}
