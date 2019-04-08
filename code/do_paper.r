rm(list=ls(all=TRUE))
options(stringsAsFactors=FALSE)
dat <- read.csv("factprod_data.csv")
dat <- dat[order(dat$year),]
library(lattice)

set <- trellis.par.get()
set$fontsize$text <- 40
set$axis.text$font <- 1
set$axis.text$fontfamily <- "serif"

set$par.xlab.text$font <- 1
set$par.xlab.text$fontfamily <- "serif"

set$par.ylab.text$font <- 1
set$par.ylab.text$fontfamily <- "serif"


prodplot <- xyplot(exp(prodind)~year, data=dat, type="l", col="black", xlab="Year",
                   ylab="Productivity (2012=1.0)", lwd=3, cex=3,
                   lty="dashed",
                   panel=function (...)
                   {
                     panel.lines(...)
                     panel.lines(dat$year, exp(smooth(dat$prodind)), col="black",
                                 lwd=7, lty="solid")
                     panel.abline(h=1.0, lwd=3, lty="dotted")
                   },
                   scales=list(y=list(at=seq(from=0.8,to=1.2,by=0.02)),
                               x=list(at=seq(from=1987,to=2016,by=3))))

prodplot <- update(prodplot, par.settings=set)

png("../figures/productivity.png",width=1024,height=1024)
print(prodplot)
dev.off()

prodgrowthplot <- xyplot(I(prodind[-1]-prodind[-nrow(dat)])~year[-1], data=dat,
                         type="l", col="black", xlab="Year",
                         ylab="Log Productivity Growth (Log Points)", lwd=3, cex=3,
                         lty="dashed",
                         scales=list(y=list(at=seq(from=-0.10,to=0.10,by=0.02)),
                                     x=list(at=seq(from=1987,to=2016,by=3))),
                         ylim=c(-0.05,0.05),
                         panel=function(...)
                         {
                           panel.lines(...)
                           panel.lines(dat$year,
                                       smooth(dat$prodind[-1] - dat$prodind[-nrow(dat)]),
                                       lwd=7, col="black")
                           panel.abline(h=0, col="black", lwd=3, lty="dotted")
                         })

prodgrowthplot <- update(prodgrowthplot, par.settings=set)


png("../figures/productivity_growth.png",width=1024,height=1024)
print(prodgrowthplot)
dev.off()

outputplot <- xyplot(exp(outputind)~year, data=dat, type="l", col="black", xlab="Year", ylab="Output-Per-Hour Worked (2012=1.0)", lwd=3, cex=3,
                   lty="dashed",
                   panel=function (...)
                   {
                     panel.lines(...)
                     panel.lines(dat$year, exp(smooth(dat$outputind)), col="black",lwd=7, lty="solid")
                     panel.abline(h=1.0, lwd=3, lty="dotted")
                   },
                   scales=list(y=list(at=seq(from=0.8,to=1.2,by=0.02)),
                               x=list(at=seq(from=1987,to=2016,by=3))))


outputplot <- update(outputplot, par.settings=set)

png("../figures/output.png", width=1024,height=1024)
print(outputplot)
dev.off()

outputgrowthplot <- xyplot(I(outputind[-1]-outputind[-nrow(dat)])~year[-1], data=dat, type="l", col="black", xlab="Year", ylab="Log Output-Per-Hour Worked Growth (Log Points)", lwd=3, cex=3, lty="dashed",
               scales=list(y=list(at=seq(from=-0.10,to=0.10,by=0.02)),
                           x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(-0.05,0.05),
               panel=function(...)
               {
                 panel.lines(...)
                 panel.lines(dat$year, smooth(dat$outputind[-1] - dat$outputind[-nrow(dat)]), lwd=7, col="black")
                 panel.abline(h=0, col="black", lwd=3, lty="dotted")
               })


outputgrowthplot <- update(outputgrowthplot, par.settings=set)

png("../figures/output_growth.png",width=1024,height=1024)
print(outputgrowthplot)
dev.off()



costprodplot <- xyplot(exp(costprodind)~year, data=dat, type="l", col="black", xlab="Year", ylab="Real Cost of 2012 Productivity (2012=1.0)", lwd=7, cex=3,
               scales=list(y=list(at=seq(from=0.0,to=8,by=0.25)),
                           x=list(at=seq(from=1987,to=2016,by=3))))

png("../figures/costprod.png")
print(costprodplot)
dev.off()

c0plot <- xyplot(exp(c0ind)~year, data=dat, type="l", col="black", xlab="Year", ylab="Real Wage of Productivity (2012=1.0)", lwd=7, cex=3,
               scales=list(y=list(at=seq(from=0.0,to=8,by=0.25)),
                           x=list(at=seq(from=1987,to=2016,by=3))))

png("../figures/c0.png")
print(c0plot)
dev.off()


gammaplot <- xyplot(gamma~year, data=dat, type="l", col="black",
                    lty="dashed",
                    xlab="Year", ylab="Elasticity of non-capital and non-labor inputs", lwd=7, cex=3,
               scales=list(y=list(at=seq(from=0.0,to=0.1,by=0.02)),
                           x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0,0.1))

gammaplot <- update(gammaplot, par.settings=set)

png("../figures/gamma.png", width=1024, height=1024)
print(gammaplot)
dev.off()

labelastplot <- xyplot(labelast~year, data=dat, type="l", col="black",
                    xlab="Year", ylab="Elasticity of labor", lwd=7, cex=3,
               scales=list(y=list(at=seq(from=0.3,to=1.0,by=0.02)),
                           x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.5,0.7))

labelastplot <- update(labelastplot, par.settings=set)

png("../figures/labelast.png", width=1024, height=1024)
print(labelastplot)
dev.off()

labpriceplot <- xyplot(I(exp(log(wage/cpi)-log(wage[year==2012]/cpi[year==2012])))~year, data=dat, type="l", col="black",
                    xlab="Year", ylab="Real Labor Price (2012 = 1.0)", lwd=7, cex=3,
               scales=list(y=list(at=seq(from=0.8,to=1.2,by=0.04)),
                           x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.8,1.1),
               panel=function(...)
               {
                 panel.lines(...)
                 panel.abline(h=1.0, lty="dotted", lwd=3, col="black")
               })


labpriceplot <- update(labpriceplot, par.settings=set)

png("../figures/labprice.png", width=1024, height=1024)
print(labpriceplot)
dev.off()

cappriceplot <- xyplot(I(exp(log(capprice/cpi)-log(capprice[year==2012]/cpi[year==2012])))~year, data=dat, type="l", col="black",
                    xlab="Year", ylab="Real Capital Price (2012 = 1.0)", lwd=7, cex=3,
               scales=list(y=list(at=seq(from=0.8,to=1.5,by=0.04)),
                           x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.85,1.4),
               panel=function(...)
               {
                 panel.lines(...)
                 panel.abline(h=1.0, lty="dotted", lwd=3, col="black")
               })


cappriceplot <- update(cappriceplot, par.settings=set)

png("../figures/capprice.png", width=1024, height=1024)
print(cappriceplot)
dev.off()



capelastplot <- xyplot(capelast~year, data=dat, type="l", col="black",
                   xlab="Year", ylab="Elasticity of capital", lwd=7, cex=3,
                   scales=list(y=list(at=seq(from=0.0,to=0.5,by=0.02)),
                               x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.22,0.40))

capelastplot <- update(capelastplot, par.settings=set)

png("../figures/capelast.png", width=1024, height=1024)
print(capelastplot)
dev.off()

## decomposition plots

dat$part.output.ind <- dat$part.output - dat$part.output[dat$year==2012]
dat$part.lab.ind <- dat$part.lab - dat$part.lab[dat$year==2012]
dat$part.cap.ind <- dat$part.cap - dat$part.cap[dat$year==2012]
dat$part.cost.ind <- dat$part.cost - dat$part.cost[dat$year==2012]
dat$part.noncost.ind <- dat$part.noncost - dat$part.noncost[dat$year==2012]


outputpartplot <- xyplot(exp(part.output.ind)~year, data=dat, type="l", col="black",
                         xlab="Year", ylab="Output part of productivity", lwd=3, cex=3,
                         lty="dashed",
                         panel=function(...)
                         {
                           panel.lines(...)
                           panel.lines(dat$year, exp(smooth(dat$part.output.ind)), lwd=7, lty="solid", col="black")
                           panel.abline(h=1.0, lwd=3, lty="dotted",col="black")
                         },
                         scales=list(y=list(at=seq(from=0.92,to=1.04,by=0.01)),
                                     x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.92,1.04))


outputpartplot <- update(outputpartplot, par.settings=set)


png("../figures/decomp_output.png",width=1024,height=1024)
print(outputpartplot)
dev.off()


labpartplot <- xyplot(exp(part.lab.ind)~year, data=dat, type="l", col="black",
                         xlab="Year", ylab="Labor price part of productivity", lwd=3, cex=3,
                         lty="dashed",
                         panel=function(...)
                         {
                           panel.lines(...)
                           panel.lines(dat$year, exp(smooth(dat$part.lab.ind)), lwd=7, lty="solid", col="black")
                           panel.abline(h=1.0, lwd=3, lty="dotted",col="black")
                         },
                         scales=list(y=list(at=seq(from=0.92,to=1.04,by=0.01)),
                                     x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.92,1.04))


labpartplot <- update(labpartplot, par.settings=set)

png("../figures/decomp_lab.png",width=1024,height=1024)
print(labpartplot)
dev.off()


cappartplot <- xyplot(exp(part.cap.ind)~year, data=dat, type="l", col="black",
                         xlab="Year", ylab="Capital price part of productivity", lwd=3, cex=3,
                         lty="dashed",
                         panel=function(...)
                         {
                           panel.lines(...)
                           panel.lines(dat$year, exp(smooth(dat$part.cap.ind)), lwd=7, lty="solid", col="black")
                           panel.abline(h=1.0, lwd=3, lty="dotted",col="black")
                         },
                         scales=list(y=list(at=seq(from=0.92,to=1.04,by=0.01)),
                                     x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.92,1.04))


cappartplot <- update(cappartplot, par.settings=set)


png("../figures/decomp_cap.png",width=1024,height=1024)
print(cappartplot)
dev.off()


costpartplot <- xyplot(exp(-1*part.cost.ind)~year, data=dat, type="l", col="black",
                         xlab="Year", ylab="Price of productivity part of productivity", lwd=3, cex=3,
                         lty="dashed",
                         panel=function(...)
                         {
                           panel.lines(...)
                           panel.lines(dat$year, exp(smooth(-1*dat$part.cost.ind)), lwd=7, lty="solid", col="black")
                           panel.abline(h=1.0, lwd=3, lty="dotted",col="black")
                         },
                         scales=list(y=list(at=seq(from=0.95,to=1.10,by=0.01)),
                                     x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.95,1.10))


costpartplot <- update(costpartplot, par.settings=set)

png("../figures/decomp_cost.png",width=1024,height=1024)
print(costpartplot)
dev.off()

noncostpartplot <- xyplot(exp(part.noncost.ind)~year, data=dat, type="l", col="black",
                         xlab="Year", ylab="Non price of productivity part of productivity", lwd=3, cex=3,
                         lty="dashed",
                         panel=function(...)
                         {
                           panel.lines(...)
                           panel.lines(dat$year, exp(smooth(dat$part.noncost.ind)), lwd=7, lty="solid", col="black")
                           panel.abline(h=1.0, lwd=3, lty="dotted",col="black")
                         },
                         scales=list(y=list(at=seq(from=0.92,to=1.04,by=0.01)),
                                     x=list(at=seq(from=1987,to=2016,by=3))), ylim=c(0.92,1.04))


noncostpartplot <- update(noncostpartplot, par.settings=set)

png("../figures/decomp_noncost.png",width=1024,height=1024)
print(noncostpartplot)
dev.off()


prod.share.output <- (dat$part.output[nrow(dat)] - dat$part.output[1])/ (dat$year[nrow(dat)] - dat$year[1])

prod.share.lab <- (dat$part.lab[nrow(dat)] - dat$part.lab[1])/ (dat$year[nrow(dat)] - dat$year[1])

prod.share.cap <- (dat$part.cap[nrow(dat)] - dat$part.cap[1])/ (dat$year[nrow(dat)] - dat$year[1])

prod.share.cost <- (dat$part.cost[nrow(dat)] - dat$part.cost[1])/ (dat$year[nrow(dat)] - dat$year[1])

## 1987 - 2005

prod.share.prod.pre <- (dat$prod[dat$year==2005] - dat$prod[1])/(2005-dat$year[1])

prod.share.output.pre <- (dat$part.output[dat$year==2005] - dat$part.output[1])/(2005-dat$year[1])

prod.share.lab.pre <- (dat$part.lab[dat$year==2005] - dat$part.lab[1])/(2005-dat$year[1])

prod.share.cap.pre <- (dat$part.cap[dat$year==2005] - dat$part.cap[1])/(2005-dat$year[1])

prod.share.cost.pre <- (dat$part.cost[dat$year==2005] - dat$part.cost[1])/(2005-dat$year[1])

## 2006 -

prod.share.prod.post <- (dat$prod[nrow(dat)] - dat$prod[dat$year==2006])/(dat$year[nrow(dat)] - 2006)

prod.share.output.post <- (dat$part.output[nrow(dat)] - dat$part.output[dat$year==2006])/(dat$year[nrow(dat)] - 2006)

prod.share.lab.post <- (dat$part.lab[nrow(dat)] - dat$part.lab[dat$year==2006])/(dat$year[nrow(dat)] - 2006)

prod.share.cap.post <- (dat$part.cap[nrow(dat)] - dat$part.cap[dat$year==2006])/(dat$year[nrow(dat)] - 2006)

prod.share.cost.post <- (dat$part.cost[nrow(dat)] - dat$part.cost[dat$year==2006])/(dat$year[nrow(dat)] - 2006)



