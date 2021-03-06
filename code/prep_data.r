rm(list=ls(all=TRUE))
options(stringsAsFactors=FALSE)

nomcap <- read.csv("../data/capital.csv")
cpi <- read.csv("../data/cpi.csv")
nomlab <- read.csv("../data/laborspend.csv")
output <- read.csv("../data/nomoutput.csv")
lab <- read.csv("../data/hours_worked.csv")
cap <- read.csv("../data/realcap.csv")

## set names
names(nomcap)[1] <- "DATE"
names(nomcap)[2] <- "nomcapital"
names(cpi)[2] <- "cpi"
names(nomlab)[2] <- "nomlabor"
names(output)[2] <- "nomoutput"
names(cap)[2] <- "cap"

names(lab) <- c("DATE", "labor")
lab$DATE <- paste0(lab$DATE, "-01-01")

dat <- merge(nomcap,cpi, by="DATE")
dat <- merge(dat,nomlab,by="DATE")
dat <- merge(dat,output,by="DATE")
dat <- merge(dat,lab,by="DATE")
dat <- merge(dat,cap,by="DATE")

dat$cpi <- as.numeric(dat$cpi)

## make real output
dat$output <- dat$nomoutput / (dat$cpi/100)

## compute capital, labor, and productivity elasticities
dat$capelast <- dat$nomcapital / dat$nomoutput
dat$labelast <- dat$nomlabor / dat$nomoutput
dat$gamma <- 1-dat$capelast-dat$labelast

## format year
dat$year <- as.numeric(substr(dat$DATE,1,regexpr("-",dat$DATE)-1))

## Compute log(F_0) (the normalizing constant)

dat$F0 <- -1*dat$capelast*log(dat$cap[dat$year==2012]) - dat$labelast*log(dat$labor[dat$year==2012])

## compute productivity
dat$prod <- log(dat$output) -
  dat$capelast*log(dat$cap) -
  dat$labelast*log(dat$labor) - dat$F0

## compute (nominal) prices for inputs
dat$wage <- (dat$nomlabor / dat$labor)
dat$capprice <- (dat$nomcapital / dat$cap)

dat$c0 <- log(dat$nomoutput) + log(dat$gamma) -
  (1/dat$gamma)*dat$prod


## labor productivity
dat$labprod <- dat$output / dat$labor
dat$capprod <- dat$output / dat$cap
dat$prodprod <- dat$output / exp(dat$prod)

dat$outputind <- log(dat$output) - log(dat$labor) - ((log(dat$output)-log(dat$labor))[dat$year==2012])
dat$prodind <- dat$prod - dat$prod[dat$year==2012]
dat$c0ind <- dat$c0 - dat$c0[dat$year==2012]
dat$wageind <- log(dat$wage) - log(dat$wage[dat$year==2012])
dat$cappriceind <- log(dat$capprice) - log(dat$capprice[dat$year==2012])
dat$gammaind <- log(dat$gamma) - log(dat$gamma[dat$year==2012])
dat$capelastind <- log(dat$capelast) - log(dat$capelast[dat$year==2012])
dat$labelastind <- log(dat$labelast) - log(dat$labelast[dat$year==2012])

## components of productivity

dat$part.output <- dat$gamma*log(dat$output)
dat$part.cost <- -1*dat$gamma*(1-dat$gamma)*(dat$c0 - log(dat$gamma))
dat$part.cap <- dat$gamma*dat$capelast*log(dat$capprice/dat$capelast)
dat$part.lab <- dat$gamma*dat$labelast*log(dat$wage/dat$labelast)
dat$part.tech <- -1*dat$gamma*dat$F0
dat$part.noncost <- dat$part.output + dat$part.cap + dat$part.lab + dat$part.tech


write.csv(dat, file="factprod_data.csv", row.names=FALSE)
