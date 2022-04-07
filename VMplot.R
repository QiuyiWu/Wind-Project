# WS = list(ws.nldas.era1.sum[[7]], ws.ccsm.era1.sum[[10]], ws.gfdl.era1.sum[[10]], ws.gem.era1.sum[[10]], 
#           ws.ccsm.era3.sum[[10]], ws.gfdl.era3.sum[[10]], ws.gem.era3.sum[[10]])
# 
# WD = list(wd.nldas.era1.sum[[7]], wd.ccsm.era1.sum[[10]], wd.gfdl.era1.sum[[10]], wd.gem.era1.sum[[10]],
#           wd.ccsm.era3.sum[[10]], wd.gfdl.era3.sum[[10]], wd.gem.era3.sum[[10]])
# 
# 
# 
# ####
# season = 'summer'
# #season = 'winter'
# num.loc = 10
# location = paste('Location ', num.loc, sep = '')
# loc = paste('loc', num.loc, sep = '')
# bench = ifelse(num.loc == 1, 'NARR', ifelse(num.loc == 3, 'NARR', ifelse(num.loc == 9, 'NARR', 'NLDAS')))
# item_name =c(bench,'CCSM1','GFDL1','GEM1','CCSM3','GFDL3','GEM3')
# 
# 
# 





pdf(file=paste("flatVM.sum.era1.loc10.pdf", sep = ''), width=16, height=3) 
#pdfFonts(height=4,width=12)
layout(matrix(1:4, nrow = 1, ncol = 4))

for (i in 1:4) {
  
  ws = WS[[i]]
  wd = WD[[i]]
  
  u.sum <- c(ws*cos(wd))
  v.sum <- c(ws*sin(wd))
  dat <- cbind(u.sum, v.sum)
  test <- movMF(dat, 2)
  
  mu <- atan2(test$theta[, 2], test$theta[,1])
  kappa <- sqrt(rowSums(test$theta^2))
  alpha <- test$alpha
  loc3.t1.sum.a <- as.circular(c(wd), zero = pi/2, rotation = 'clock')
  
  
  # plot (flatten plots)
  ff.mix  <- function(x) dvonmises(x, mu=circular(mu[1]), kappa=kappa[1])*alpha[1] + 
    dvonmises(x, mu=circular(mu[2]), kappa=kappa[2])*alpha[2]
  
  plot(wd, ff.mix(wd), xaxt = "n", ylab = "Density", pch = ".", main= bquote(atop(.(item_name[i])~","~~~.(season)~','~~~.(location),
                                                                                  atop(hat(mu)[1]== .(round(mu[1],2))~~~~~hat(kappa)[1]== .(round(kappa[1],2))~~~~~ hat(alpha)[1]== .(round(alpha[1],2)),  
                                                                                       hat(mu)[2]== .(round(mu[2],2))~~~~~hat(kappa)[2]== .(round(kappa[2],2))~~~~~ hat(alpha)[2]== .(round(alpha[2],2))))))
  axis(1, at = seq(-pi, pi, pi / 4), labels = c("S", "SW", "W", "NW", "N", "NE", "E", "SE", "S"))
  hist(wd, freq = F, col=rgb(0.8,0.8,0.8,0.5), add = T)
  
  legend("topright", legend = c('density', 'data'), 
         bty = "n",col = c("black", NA),lty = c(1,NA),density=c(0,1000),
         fill = c("green", rgb(0.8,0.8,0.8,0.5)),border = c(NA,"black"),x.intersp=c(0.5,-1.5))
}

dev.off()






pdf(file=paste("flatVM.sum.era3.loc10.pdf", sep = ''), width=16, height=3) 
#pdfFonts(height=4,width=12)
layout(matrix(0:3, nrow = 1, ncol = 4))

for (i in 5:7) {
  
  ws = WS[[i]]
  wd = WD[[i]]
  
  u.sum <- c(ws*cos(wd))
  v.sum <- c(ws*sin(wd))
  dat <- cbind(u.sum, v.sum)
  test <- movMF(dat, 2)
  
  mu <- atan2(test$theta[, 2], test$theta[,1])
  kappa <- sqrt(rowSums(test$theta^2))
  alpha <- test$alpha
  loc3.t1.sum.a <- as.circular(c(wd), zero = pi/2, rotation = 'clock')
  
  
  # plot (flatten plots)
  ff.mix  <- function(x) dvonmises(x, mu=circular(mu[1]), kappa=kappa[1])*alpha[1] + 
    dvonmises(x, mu=circular(mu[2]), kappa=kappa[2])*alpha[2]
  
  plot(wd, ff.mix(wd), xaxt = "n", ylab = "Density", pch = ".", main= bquote(atop(.(item_name[i])~","~~~.(season)~','~~~.(location),
                                                                                  atop(hat(mu)[1]== .(round(mu[1],2))~~~~~hat(kappa)[1]== .(round(kappa[1],2))~~~~~ hat(alpha)[1]== .(round(alpha[1],2)),  
                                                                                       hat(mu)[2]== .(round(mu[2],2))~~~~~hat(kappa)[2]== .(round(kappa[2],2))~~~~~ hat(alpha)[2]== .(round(alpha[2],2))))))
  axis(1, at = seq(-pi, pi, pi / 4), labels = c("S", "SW", "W", "NW", "N", "NE", "E", "SE", "S"))
  hist(wd, freq = F, col=rgb(0.8,0.8,0.8,0.5), add = T)
  
  legend("topright", legend = c('density', 'data'), 
         bty = "n",col = c("black", NA),lty = c(1,NA),density=c(0,1000),
         fill = c("green", rgb(0.8,0.8,0.8,0.5)),border = c(NA,"black"),x.intersp=c(0.5,-1.5))
}

dev.off()








