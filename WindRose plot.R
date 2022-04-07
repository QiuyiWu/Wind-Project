library('fields')
library('circular')
library('movMF')
library('fitdistrplus')
library('maps')
library('mapdata')
library('fields')
library('scales')
library(ggplot2)
library(circular)
library(clifro)
require(gridExtra)
library(gdata)

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

convert <- function(x) {
  x[x<0] = x[x<0]+2*pi 
  return(x)
}

## load WS and WD as instructed above
# [season, location]
WS = list(ws.nldas.era1.win[[7]], ws.ccsm.era1.win[[10]], ws.gfdl.era1.win[[10]], ws.gem.era1.win[[10]],
          ws.ccsm.era3.win[[10]], ws.gfdl.era3.win[[10]], ws.gem.era3.win[[10]])

WD = list(wd.nldas.era1.win[[7]], wd.ccsm.era1.win[[10]], wd.gfdl.era1.win[[10]], wd.gem.era1.win[[10]],
          wd.ccsm.era3.win[[10]], wd.gfdl.era3.win[[10]], wd.gem.era3.win[[10]])





####
#season = 'summer'
season = 'winter'
num.loc = 10
location = paste('Location ', num.loc, sep = '')
loc = paste('loc', num.loc, sep = '')
bench = ifelse(num.loc == 1, 'NARR', ifelse(num.loc == 3, 'NARR', ifelse(num.loc == 9, 'NARR', 'NLDAS')))
item_name =c(bench,'CCSM1','GFDL1','GEM1','CCSM3','GFDL3','GEM3')



#####################################################################
### Rose  
#####################################################################

filename = paste(loc,'.rose.t1.',season,'.eps',sep='')
setEPS()
postscript(filename,height=4,width=12)

######### Era 1 #########
i=1
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot1 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")

i=2
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot2 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")

legend <- get_legend( windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1)) + 
                        ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
                        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                              text = element_text(size = 12), legend.position = "top"))


i=3
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot3 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")

i=4
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot4 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")


grid.arrange(legend, plot1, plot2, plot3, plot4, ncol=4, nrow = 2, 
             layout_matrix = rbind(c(1,1,1,1), c(2,3,4,5)),
             widths = c(2.7, 2.7,2.7,2.7), heights = c(0.2,1))

dev.off()


######### Era 3 #########
filename = paste(loc,'.rose.t3.',season,'.eps',sep='')
setEPS()
postscript(filename,height=4,width=12)

i=5
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot1 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")

i=6
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot2 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")

legend <- get_legend( windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1)) + 
                        ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
                        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                              text = element_text(size = 12), legend.position = "top"))


i=7
ws = c(WS[[i]])
wd = convert(WD[[i]])

wind <- data.frame(cbind(ws, wd))
colnames(wind) <- c("Speed", "Direction")
plot3 <- windrose(ws, wd*180/pi, speed_cuts = seq(0,6,1), ggtheme='minimal') + 
  ggtitle(paste(item_name[i], season, location, sep = ', ')) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        text = element_text(size = 12), legend.position = "none")

#blank <- plot(0,type='n',axes=FALSE,ann=FALSE)
grid.arrange(legend, plot1, plot2, plot3, ncol=4, nrow = 2, 
             layout_matrix = rbind(c(1,1,1,1), c(5,2,3,4)),
             widths = c(2.7, 2.7,2.7,2.7), heights = c(0.2,1))
dev.off()








#####################################################################
### VM
#####################################################################

filename = paste(loc,'.vm.t1.',season,'.eps',sep='')
setEPS()
postscript(filename,height=4,width=12)
par(mfrow=c(1,4), mar=c(0,0,8,0))
#layout(matrix(1:4,1,4))

# Era 1

for (i in 1:4) {
  ws = WS[[i]]
  wd = WD[[i]]
  
  u.bench.win <- c(ws*cos(wd))
  v.bench.win <- c(ws*sin(wd))
  dat <- cbind(u.bench.win, v.bench.win)
  test <- movMF(dat, 2)
  
  mu <- atan2(test$theta[, 2], test$theta[,1])
  kappa <- sqrt(rowSums(test$theta^2))
  alpha <- test$alpha
  loc3.t1.win.a <- as.circular(c(wd), zero = pi/2, rotation = 'clock')
  
  # plot 
  plot.circular(loc3.t1.win.a, shrink = 2, sep =0.05, bins =360 ,axes = F, cex =.01, rotation = 'clock', 
                zero =  pi/2, main= bquote(atop(.(item_name[i])~","~~~.(season)~','~~~.(location),
                                                atop(hat(mu)[1]== .(round(mu[1],2))~~~~~hat(kappa)[1]== .(round(kappa[1],2))~~~~~ hat(alpha)[1]== .(round(alpha[1],2)),  
                                                     hat(mu)[2]== .(round(mu[2],2))~~~~~hat(kappa)[2]== .(round(kappa[2],2))~~~~~ hat(alpha)[2]== .(round(alpha[2],2))))))
  rose.diag(loc3.t1.win.a, pch = 16, ticks = F, cex = 1, axes = F , shrink = 1,
            col = "#c5c1f4" , prop = 2.5 , bins =18 , upper = TRUE , add = T, rotation = 'clock', zero =  pi / 2)
  axis.circular ( at = circular ( seq (0 , 2*pi - pi / 2 , pi / 2), rotation = 'clock', zero =  pi / 2) ,
                  labels = c( "N" , "E" , "S" , "W" ),
                  tick =T , tcl.text =.18)
  
  ff.mix1 <- function(x) dvonmises(x, mu=circular(mu[1]), kappa=kappa[1])
  ff.mix2 <- function(x) dvonmises(x, mu=circular(mu[2]), kappa=kappa[2])
  ff.mix  <- function(x) ff.mix1(x)*alpha[1] + ff.mix2(x)*alpha[2]
  curve.circular(ff.mix, add = T, col="indianred1", lwd=1.5, rotation = 'clock', zero = pi/2)
}

dev.off()



# Era 3
filename = paste(loc,'.vm.t3.',season,'.eps',sep='')
setEPS()
postscript(filename,height=4,width=12)
par(mar=c(0,0,8,0))
layout(matrix(c(0, 1, 2, 3), nrow = 1, ncol = 4))

for (i in 5:7) {
 # par(mfg = c(1,i-3))
  ws = WS[[i]]
  wd = WD[[i]]
  
  u.bench.win <- c(ws*cos(wd))
  v.bench.win <- c(ws*sin(wd))
  dat <- cbind(u.bench.win, v.bench.win)
  test <- movMF(dat, 2)
  
  mu <- atan2(test$theta[, 2], test$theta[,1])
  kappa <- sqrt(rowSums(test$theta^2))
  alpha <- test$alpha
  loc3.t1.win.a <- as.circular(c(wd), zero = pi/2, rotation = 'clock')
  
  # plot 
  
  plot.circular(loc3.t1.win.a, shrink = 2, sep =0.05, bins =360 ,axes = F, cex =.01, rotation = 'clock', 
                zero =  pi/2, main= bquote(atop(.(item_name[i])~","~~~.(season)~','~~~.(location),
                                                atop(hat(mu)[1]== .(round(mu[1],2))~~~~~hat(kappa)[1]== .(round(kappa[1],2))~~~~~ hat(alpha)[1]== .(round(alpha[1],2)),  
                                                     hat(mu)[2]== .(round(mu[2],2))~~~~~hat(kappa)[2]== .(round(kappa[2],2))~~~~~ hat(alpha)[2]== .(round(alpha[2],2))))))
  rose.diag(loc3.t1.win.a, pch = 16, ticks = F, cex = 1, axes = F , shrink = 1,
            col = "#c5c1f4" , prop = 2.5 , bins =18 , upper = TRUE , add = T, rotation = 'clock', zero =  pi / 2)
  axis.circular ( at = circular ( seq (0 , 2*pi - pi / 2 , pi / 2), rotation = 'clock', zero =  pi / 2) ,
                  labels = c( "N" , "E" , "S" , "W" ),
                  tick =T , tcl.text =.18)
  
  ff.mix1 <- function(x) dvonmises(x, mu=circular(mu[1]), kappa=kappa[1])
  ff.mix2 <- function(x) dvonmises(x, mu=circular(mu[2]), kappa=kappa[2])
  ff.mix  <- function(x) ff.mix1(x)*alpha[1] + ff.mix2(x)*alpha[2]
  curve.circular(ff.mix, add = T, col="indianred1", lwd=1.5, rotation = 'clock', zero = pi/2)
  
  }

dev.off()



#####################################################################
### scatter
#####################################################################

# Era 1
filename = paste('scatterWDWS_',loc,'_',season,'_Era1.eps',sep='')
setEPS()
postscript(filename,height=5,width=20)
layout(matrix(1:4,1,4))

for (i in 1:4) {
  WSi = c(WS[[i]])
  WDi = c(WD[[i]])
  theta_sort <- sort(WDi)
  theta_id <- findInterval(WDi, unique(quantile(WDi, seq(0, 1, length.out = 18 + 1))), rightmost.closed = TRUE)
  temp <- cbind(WDi, WSi, theta_id)
  r_agg_mean <- aggregate(temp, list(theta_id), mean, na.rm = T)
  r_agg_q95 <- aggregate(temp, list(theta_id), function(x) quantile(x, p = 0.95))
  ui = WSi*cos(WDi)
  vi = WSi*sin(WDi)
  dat <- cbind(ui,vi)
  test <- movMF(dat, 2)
  mu <- atan2(test$theta[, 2], test$theta[,1])
  #
  plot(WDi,WSi,xlim=c(-pi,pi),ylim=c(0,15),cex=0.2,las=1,xlab="Wind direction",ylab="wind speed (m/s)",xaxt='n')
  axis(1,at=c(-pi,-pi/2,0,pi/2,pi),c('S','W','N','E','S'))
  lines(r_agg_mean[, 2], r_agg_mean[, 3], col = "blue")
  lines(r_agg_mean[, 2], r_agg_q95[, 3], col = "red")
  abline(v = mu, lty = 2, col = "green")
  title(paste(item_name[i],'-',season,'-',location))
}

dev.off()




# Era 3

filename = paste('scatterWDWS_',loc,'_',season,'_Era3.eps',sep='')
setEPS()
postscript(filename,height=5,width=20)
layout(matrix(c(0, 1, 2, 3), nrow = 1, ncol = 4))
for (i in 5:7) {
  WSi = c(WS[[i]])
  WDi = c(WD[[i]])
  theta_sort <- sort(WDi)
  theta_id <- findInterval(WDi, unique(quantile(WDi, seq(0, 1, length.out = 18 + 1))), rightmost.closed = TRUE)
  temp <- cbind(WDi, WSi, theta_id)
  r_agg_mean <- aggregate(temp, list(theta_id), mean, na.rm = T)
  r_agg_q95 <- aggregate(temp, list(theta_id), function(x) quantile(x, p = 0.95))
  ui = WSi*cos(WDi)
  vi = WSi*sin(WDi)
  dat <- cbind(ui,vi)
  test <- movMF(dat, 2)
  mu <- atan2(test$theta[, 2], test$theta[,1])
  #
  plot(WDi,WSi,xlim=c(-pi,pi),ylim=c(0,15),cex=0.2,las=1,xlab="Wind direction",ylab="wind speed (m/s)",xaxt='n')
  axis(1,at=c(-pi,-pi/2,0,pi/2,pi),c('S','W','N','E','S'))
  lines(r_agg_mean[, 2], r_agg_mean[, 3], col = "blue")
  lines(r_agg_mean[, 2], r_agg_q95[, 3], col = "red")
  abline(v = mu, lty = 2, col = "green")
  title(paste(item_name[i],'-',season,'-',location))
}

dev.off()




#####################################################################
### q95
#####################################################################

q95 = lapply(X=WS,FUN=quantile,probs=0.95)
ind.q95 = NULL
WS.q95 <- NULL ; WD.q95 <- NULL
for (i in 1:length(q95)){
  ind.q95[[i]] <- which(WS[[i]]>q95[[i]])
  WS.q95[[i]] <- WS[[i]][ind.q95[[i]]]
  WD.q95[[i]] <- WD[[i]][ind.q95[[i]]]   }

main.title=paste(season,' - ',location)
xlim=c(-pi,pi) ; ylim=range(WS.q95)
#
filename1 = paste('q95VSwd_',season,'_',loc,'_era1.eps',sep='')
setEPS()
postscript(filename1,height=6,width=24)
layout(matrix(1:3,1,3))
plot(WD.q95[[1]],WS.q95[[1]],xlim=xlim,ylim=ylim,pch=20,xlab='Wind direction',ylab='Wind speed (m/s)',main=main.title,xaxt='n',cex=2,cex.axis=3,cex.lab=2,cex.main=2)
axis(1,at=(pi*(-4:4)/4),c('S','SW','W','NW','N','NE','E','SE','S'),cex.axis=2)
points(WD.q95[[2]],WS.q95[[2]],col=2,pch=20,cex=2)
abline(h=q95[c(1:2)],col=1:2,lwd=1.5)
legend(x='topright',c(paste(item_name[1],'-Era1', sep = ''),'CCSM-Era1','95%-quantile'),pch=c(20,20,NA),col=c(1:2,1),lty=c(NA,NA,1),cex=2.5)
#
plot(WD.q95[[1]],WS.q95[[1]],xlim=xlim,ylim=ylim,pch=20,xlab='Wind direction',ylab='Wind speed (m/s)',main=main.title,xaxt='n',cex=2,cex.axis=3,cex.lab=2,cex.main=2)
axis(1,at=(pi*(-4:4)/4),c('S','SW','W','NW','N','NE','E','SE','S'),cex.axis=2)
points(WD.q95[[3]],WS.q95[[3]],col=2,pch=20,cex=2)
abline(h=q95[c(1,3)],col=1:4,lwd=1.5)
legend(x='topright',c(paste(item_name[1],'-Era1', sep = ''),'GFDL-Era1','95%-quantile'),pch=c(20,20,NA),col=c(1:2,1),lty=c(NA,NA,1),cex=2.5)
#
plot(WD.q95[[1]],WS.q95[[1]],xlim=xlim,ylim=ylim,pch=20,xlab='Wind direction',ylab='Wind speed (m/s)',main=main.title,xaxt='n',cex=2,cex.axis=3,cex.lab=2,cex.main=2)
axis(1,at=(pi*(-4:4)/4),c('S','SW','W','NW','N','NE','E','SE','S'),cex.axis=2)
points(WD.q95[[4]],WS.q95[[4]],col=2,pch=20,cex=2)
abline(h=q95[c(1,4)],col=1:4,lwd=1.5)
legend(x='topright',c(paste(item_name[1],'-Era1', sep = ''),'HadGEM-Era1','95%-quantile'),pch=c(20,20,NA),col=c(1:2,1),lty=c(NA,NA,1),cex=2.5)
dev.off()
##



##
filename2 = paste('q95VSwd_',season,'_',loc,'_era13.eps',sep='')
setEPS()
postscript(filename2,height=6,width=24)
layout(matrix(1:3,1,3))
#
plot(WD.q95[[2]],WS.q95[[2 ]],xlim=xlim,ylim=ylim,pch=20,xlab='Wind direction',ylab='Wind speed (m/s)',main=main.title,xaxt='n',cex=2,cex.axis=3,cex.lab=2,cex.main=2)
axis(1,at=(pi*(-4:4)/4),c('S','SW','W','NW','N','NE','E','SE','S'),cex.axis=2)
points(WD.q95[[5]],WS.q95[[5]],col=2,pch=20,cex=2)
abline(h=q95[c(2,5)],col=1:2,lwd=1.5)
legend(x='topright',c('CCSM-Era1','CCSM-Era3','95%-quantile'),pch=c(20,20,NA),col=c(1:2,1),lty=c(NA,NA,1),cex=2.5)


plot(WD.q95[[3]],WS.q95[[3]],xlim=xlim,ylim=ylim,pch=20,xlab='Wind direction',ylab='Wind speed (m/s)',main=main.title,xaxt='n',cex=2,cex.axis=3,cex.lab=2,cex.main=2)
axis(1,at=(pi*(-4:4)/4),c('S','SW','W','NW','N','NE','E','SE','S'),cex.axis=2)
points(WD.q95[[6]],WS.q95[[6]],col=2,pch=20,cex=2)
abline(h=q95[c(3,6)],col=1:4,lwd=1.5)
legend(x='topright',c('GFDL-Era1','GFDL-Era3','95%-quantile'),pch=c(20,20,NA),col=c(1:2,1),lty=c(NA,NA,1),cex=2.5)
#
plot(WD.q95[[4]],WS.q95[[4]],xlim=xlim,ylim=ylim,pch=20,xlab='Wind direction',ylab='Wind speed (m/s)',main=main.title,xaxt='n',cex=2,cex.axis=3,cex.lab=2,cex.main=2)
axis(1,at=(pi*(-4:4)/4),c('S','SW','W','NW','N','NE','E','SE','S'),cex.axis=2)
points(WD.q95[[7]],WS.q95[[7]],col=2,pch=20,cex=2)
abline(h=q95[c(4,7)],col=1:4,lwd=1.5)
legend(x='topright',c('HadGEM-Era1','HadGEM-Era3','95%-quantile'),pch=c(20,20,NA),col=c(1:2,1),lty=c(NA,NA,1),cex=2.5)
dev.off()



