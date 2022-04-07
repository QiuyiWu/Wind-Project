# Weibull WLS regression plot

library(fitdistrplus)
library(scales)

label <- c("CA_coast", "ID", "OR_coast", "CO", "TX_GP", "ND_GP", "NC_mountain", "FL_coast","NE_coast", "OH_Erie")
wdir <- c("S", "SW", "W", "NW", "N", "NE", "E", "SE", "S")
xg = seq(-pi, pi, 0.01)
cols <- 4:2

loc.nldas = c(2,4,5,6,7,8,10)
loc.narr = c(1,3,9)
# 

# Bin the wind direction by 10 degree intervals
thetaBin_present <- lapply(WD_present, function(x){
  findInterval(x, seq(-pi, pi, len = 19), rightmost.closed = TRUE)
}) 


WSDdat_present <- array(dim = c(10, 7200, 3))
for (i in 1:10){
  WSDdat_present[i,,] = cbind(WS_present[[i]], WD_present[[i]], thetaBin_present[[i]])
}

# Summary wind data by wind direction bin
r_theta_summary_present <- apply(WSDdat_present, 1, function(x) aggregate(x[, 1:2], list(x[, 3]), summary))
# Fit Weibull separately by each direction bin
Weibull_MLE_dir_present <- apply(WSDdat_present[,, c(1, 3)], 1, function(x){
  aggregate(x[, 1], by = list(x[, 2]), FUN = function(z) fitdist(z, "weibull")$estimate)
})
Weibull_Se_dir_present <- apply(WSDdat_present[,, c(1, 3)], 1, function(x){
  aggregate(x[, 1], by = list(x[, 2]), FUN = function(z) fitdist(z, "weibull")$sd)
})
thetaG_present <- lapply(r_theta_summary_present, function(x) x$V2[, 3]) 
# Use the median of wind direction of each direction bin as x
# Create a harmonic matrix 
Harmonic <- function(theta, K){
  t <- outer(theta, 1:K)
  return(cbind(apply(t, 2, cos), apply(t, 2, sin)))
}
K = 4
X_present <- lapply(thetaG_present, function(x) Harmonic(x, K))
# you can tune the number K
# Fit least square to get a quick idea on how the Weibull parameters vary with angle
xg = seq(-pi, pi, 0.01)
wdir <- c("S", "SW", "W", "NW", "N", "NE", "E", "SE", "S")
for (i in 1:10){
  WLS_data_present <- cbind(Weibull_MLE_dir_present[[i]]$x, X_present[[i]])
  se_p <- Weibull_Se_dir_present[[i]]
  shape_wls_p <- lm(WLS_data_present[, 1] ~ WLS_data_present[, 3:10], weights = 1/se_p$x[, 1]^2)
  scale_wls_p <- lm(WLS_data_present[, 2] ~ WLS_data_present[, 3:10], weights = 1/se_p$x[, 2]^2)
  yg_shape_p <- cbind(rep(1, 629), Harmonic(xg, K)) %*% shape_wls_p$coefficients
  yg_scale_p <- cbind(rep(1, 629), Harmonic(xg, K)) %*% scale_wls_p$coefficients
}
## Compare the estimated directional quantiles
p <- c(0.5, 0.75, 0.95)
WS_WD_quantiles_p <- apply(WSDdat_present, 1, function(x) aggregate(x[, 1], list(x[, 3]), quantile, probs = p))
Weibull_q_wls_p <- Weibull_q_wls_f <- array(dim = c(10, 629, 3))
col = c("black", "blue", "red")
for (i in 1:10){
  WLS_data_p <- cbind(Weibull_MLE_dir_present[[i]]$x, X_present[[i]])
  se_p <- Weibull_Se_dir_present[[i]]
  shape_wls_p <- lm(WLS_data_p[, 1] ~ WLS_data_p[, 3:10], weights = 1/se_p$x[, 1]^2)
  scale_wls_p <- lm(WLS_data_p[, 2] ~ WLS_data_p[, 3:10], weights = 1/se_p$x[, 2]^2)
  yg_shape_p <- cbind(rep(1, 629), Harmonic(xg, K)) %*% shape_wls_p$coefficients
  yg_scale_p <- cbind(rep(1, 629), Harmonic(xg, K)) %*% scale_wls_p$coefficients
  
  for (j in 1:3){
    Weibull_q_wls_p[i,, j] <- qweibull(p[j], yg_shape_p, yg_scale_p)
  } 
}




# 
  WBpred_p_Bench = Weibull_q_wls_p
  save(WBpred_p_Bench, file = "WB.bench.win.RData")
# #
  WBpred_f_CCSM = Weibull_q_wls_f
  save(WBpred_f_CCSM, file = "WBpred_f_CCSM.RData")

###############################################

#### ALL IN ONE PLOT

for (i in 1:10){
  pdf(file=paste("wball.sum.era1.loc",i, ".pdf", sep = '')) 
  par(mar = c(3.6, 3.6, 1, 0.6))
  plot(xg, WBpred_p_CCSM[i, ,1], pch = 20, cex = .2, col = "white", las = 1, 
       xlab = "", ylab = "", xaxt = "n", main = label[i], ylim = c(0,20))
  mtext("Wind direction", 1, line = 2)
  mtext("Wind speed (m/s)", 2, line = 2)
  for (j in c(1,3)) {
    lines(xg, WBpred_p_Bench[i, ,j], col = cols[j], lty = 1)
    lines(xg, WBpred_p_CCSM[i, ,j], col = cols[j], lty = 2)
    lines(xg, WBpred_p_GFDL[i, ,j], col = cols[j], lty = 3)
    lines(xg, WBpred_p_GEM[i, ,j], col = cols[j], lty = 4)
  }
  axis(1, at = seq(-pi, pi, pi / 4), labels = wdir)
  
  legend("topleft", c("median", "95%-quantile"), col = c(4,2), pch = 20, bty = "n")
  legend("top", c("Benchmark","CCSM", "GFDL", "GEM"),
         lty = c(1:4), bty = "n")
  
  dev.off()
}

