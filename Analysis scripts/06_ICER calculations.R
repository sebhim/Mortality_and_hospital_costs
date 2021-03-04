###############################################################################
# inlezen input
###############################################################################
rm(list=ls())
library(MASS)
library(lme4)
setwd("C:\\Users\\20108pvb\\Dropbox\\papers in wording\\paper COI & mort\\bayesian\\")
dcoi  <- read.table("coi.txt",header=T) # COI data uit Health Policy artikel 
dmort <- read.table("mort.txt",header=T,sep="\t") # sterfte
dqol  <- read.table("qol ttd pols mdm.txt" ,header =T) # QoL by TTD uit MDM artikel van Maria
dpaid <- read.table("paid 1.1 hospitals no cvd.txt",header=T,sep="\t") # sterfte
#load("Outputs Bootstrap JAGS.RData")
load("Outputs Bootstrap JAGS Complete cases.RData")
load("Outputs Bootstrap JAGS Additional sensitivity.RData")
load("Threshold bootstraps.RData")

###############################################################################
# 1 file maken
###############################################################################
dall <- dmort
dall$mr <- dall$totmort/dall$pop
dall$lnm <- log(dall$mr+0.000000001)
dall$hcpc[dall$dis=="hvz"] <- dcoi$hcpc[dcoi$diag==7]
dall$hcpc[dall$dis=="can"] <- dcoi$hcpc[dcoi$diag==2]
dall$hcpc[dall$dis=="lun"] <- dcoi$hcpc[dcoi$diag==8]
dall$hcpc[dall$dis=="tot"] <- aggregate(hcpc~year+age+gender,data=dcoi,FUN=sum)$hcpc
dall$lnc <- log(dall$hcpc)
dall$id  <- dall$age; dall$id[dall$gender==1] <- dall$id[dall$gender==1]+21
dhvz <- dall[dall$dis=="hvz",]

# trends in share of deaths
sum(dall$totmort[dall$dis=="hvz"&dall$year==1994]) / sum(dall$totmort[dall$dis=="tot"&dall$year==1994])
sum(dall$totmort[dall$dis=="hvz"&dall$year==2010]) / sum(dall$totmort[dall$dis=="tot"&dall$year==2010])

# spendings summaries
sum((dall$hcpc*dall$pop)[dall$dis=="tot"&dall$year==2010]) / sum((dall$hcpc*dall$pop)[dall$dis=="tot"&dall$year==1994])
sum((dall$hcpc*dall$pop)[dall$dis=="hvz"&dall$year==2010]) / sum((dall$hcpc*dall$pop)[dall$dis=="hvz"&dall$year==1994])
sum((dall$hcpc*dall$pop)[dall$dis=="hvz"&dall$year==2010]) / sum((dall$hcpc*dall$pop)[dall$dis=="tot"&dall$year==2010])

######################################################################
# vertalen naar threshold --> ook PAID!
# basisjaar 2010
###################################################################
mr    <- dall$totmort[dall$dis=="tot"&dall$year==2010]/dall$pop[dall$dis=="tot"&dall$year==2010]
mrhvz <- dall$totmort[dall$dis=="hvz"&dall$year==2010]/dall$pop[dall$dis=="tot"&dall$year==2010]
ctot  <- dall$hcpc[dall$dis=="tot"&dall$year==2010]
chvz  <- dall$hcpc[dall$dis=="hvz"&dall$year==2010]
sq <- c(rep(dqol$qol[dqol$age==50&dqol$ttd==10&dqol$gender==1],49),dqol$qol[dqol$ttd==10&dqol$gender==1],
        rep(dqol$qol[dqol$age==90&dqol$ttd==10&dqol$gender==1],10),
        rep(dqol$qol[dqol$age==50&dqol$ttd==10&dqol$gender==2],49),dqol$qol[dqol$ttd==10&dqol$gender==2],
        rep(dqol$qol[dqol$age==90&dqol$ttd==10&dqol$gender==2],10)      )
dq <- c(rep(dqol$qol[dqol$age==50&dqol$ttd==0&dqol$gender==1],49),dqol$qol[dqol$ttd==0&dqol$gender==1],
        rep(dqol$qol[dqol$age==90&dqol$ttd==0&dqol$gender==1],10),
        rep(dqol$qol[dqol$age==50&dqol$ttd==0&dqol$gender==2],49),dqol$qol[dqol$ttd==0&dqol$gender==2],
        rep(dqol$qol[dqol$age==90&dqol$ttd==0&dqol$gender==2],10)      )

f.smooth <- function(y0){ #van 21 naar 95 klassen
  a1<-c(1,4,(1:19)*5+2)
  y1<-predict(smooth.spline(a1,log(y0)),x=1:97)$y
  exp(c(y1,rep(y1[97],3)))
}

f.calcICER <- function(m=1,a=65,dre=1.015,drc=1.04,el=-.13) {
  # indices creeren mannen en vrouwen
  i1 <- 1:21 ; i2 <- 1:100
  sc <- dpaid$scm; dc <-dpaid$dcm
  if (m==0) {
    i1 <- 22:42; i2 <- 101:200
    sc <- dpaid$scf; dc <-dpaid$dcf
  }
  
  # kosten en qalys berekenen 
  mr1   <- f.smooth(mr[i1])[a:100]
  surv1 <- cumprod(1-pexp(mr1))
  mort1 <- -diff(c(1,surv1))
  q1    <- surv1*sq[i2][a:100]+mort1*dq[i2][a:100]
  uc1   <- surv1*sc[a:100]+mort1*dc[a:100] # unrelated costs
  rc1   <- f.smooth(chvz[i1])[a:100]*surv1 
  c1    <- uc1+rc1
  
  mr2   <- f.smooth(0.1*el*mrhvz[i1]+mr[i1])[a:100] 
  surv2 <- cumprod(1-pexp(mr2))
  mort2 <- -diff(c(1,surv2))
  q2    <- surv2*sq[i2][a:100]+mort2*dq[i2][a:100]
  uc2   <- surv2*sc[a:100]+mort2*dc[a:100] # unrelated costs
  rc2   <- 1.1*f.smooth(chvz[i1])[a:100]*surv2 
  c2    <- uc2+rc2
  
  #icers berekenen 
  # let op na leeftijd 100 sterfte constant en geen ziekenhuis kosten meer, laagste QoL in laatste levensjaar  
  l1    <-  0:length(99:a)
  correctionS <- (surv2-surv1)[length(l1)]/max(mr1)*dre^(-length(l1)) # ]LY CORRECTION
  correctionQ <- (surv2-surv1)[length(l1)]/max(mr1)*min(dq)*dre^(-length(l1)) # QALY CORRECTION
  list(
       #le0 = c(sum(surv1),sum(surv2)), 
       le = c(sum(surv1)+min(surv1)/max(mr1),sum(surv2)+min(surv2)/max(mr1)), 
       #qale0=c(sum(q1),sum(q2)), 
       qale=c(sum(q1)+min(surv1)/max(mr1)*min(dq),sum(q2)+min(surv2)/max(mr1)*min(dq)), 
       cost = c(sum(c1),sum(c2)),
       dc = c2-c1,
       drc= rc2-rc1,
       duc= uc2-uc1,
       ds = surv2-surv1,
       dq = q2-q1,
       dcq = c( sum((c2-c1)/drc^(l1)),sum((q2-q1)/dre^(l1))+correctionQ,sum((surv2-surv1)/dre^(l1))+correctionS ),
       #icer0=sum((c2-c1)/drc^(l1))/sum((q2-q1)/dre^(l1)),
       icer =sum((c2-c1)/drc^(l1))/(sum((q2-q1)/dre^(l1))+correctionQ),
       icer2=sum((rc2-rc1)/drc^(l1))/sum((q2-q1)/dre^(l1))
         )
}

w1 <- f.smooth(dall$pop[dall$year==2010&dall$dis=="hvz"&dall$gender==0])[65:95]
w2 <- f.smooth(dall$pop[dall$year==2010&dall$dis=="hvz"&dall$gender==1])[65:95]
wts <- c(w1,w2)/sum(w1+w2)

f.iceage <- function(el=-.13){
  a1 <- length(65:95)
  icer <- dc <- dq<-ds <- matrix(0,2,a1)
  for (g in 1:2){
    for (a in 1:a1){
      temp <- f.calcICER(m=g-1,a=a+65,el=el)
      icer[g,a] <- temp$icer
      dc[g,a] <- temp$dcq[1]
      dq[g,a] <- temp$dcq[2]
      ds[g,a] <- temp$dcq[3]
    }
  }
  #sum(rbind(w1/sum(w1+w2),w2/sum(w1+w2))*icer)
  c(sum(rbind(w1/sum(w1+w2),w2/sum(w1+w2))*dc),
    sum(rbind(w1/sum(w1+w2),w2/sum(w1+w2))*dq),
    sum(rbind(w1/sum(w1+w2),w2/sum(w1+w2))*ds))
}

f.calcICER(65,m=1)$dcq
f.iceage()

###############################################################################
# bootstrappen voor de ICERS
###############################################################################
#vague 
dcq.v <- f.iceage(mean(c(vague.sim[[1]][,"btot"],vague.sim[[2]][,"btot"])))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(vague.sim[[1]][,"btot"],vague.sim[[2]][,"btot"]),0.025))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(vague.sim[[1]][,"btot"],vague.sim[[2]][,"btot"]),0.975))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.vague <- sapply(vague.sim[[1]][1:5000,"btot"],f.iceage)
mean(dcq.vague[1,])/mean(dcq.vague[2,])
mean(dcq.vague[1,])/mean(dcq.vague[3,])

#gallett 
dcq.v <- f.iceage(mean(c(gallett.sim[[1]][,"btot"],gallett.sim[[2]][,"btot"])))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(gallett.sim[[1]][,"btot"],gallett.sim[[2]][,"btot"]),0.025))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(gallett.sim[[1]][,"btot"],gallett.sim[[2]][,"btot"]),0.975))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.gallett <- sapply(gallett.sim[[1]][1:5000,"btot"],f.iceage)
mean(dcq.gallett[1,])/mean(dcq.gallett[2,])
mean(dcq.gallett[1,])/mean(dcq.gallett[3,])

#Claxton 
dcq.v <- f.iceage(mean(c(claxton.sim[[1]][,"btot"],claxton.sim[[2]][,"btot"])))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(claxton.sim[[1]][,"btot"],claxton.sim[[2]][,"btot"]),0.025))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(claxton.sim[[1]][,"btot"],claxton.sim[[2]][,"btot"]),0.975))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.claxton <- sapply(claxton.sim[[1]][1:5000,"btot"],f.iceage)
mean(dcq.claxton[1,])/mean(dcq.claxton[2,])
mean(dcq.claxton[1,])/mean(dcq.claxton[3,])

#vague short 
dcq.v <- f.iceage(mean(c(short.sim[[1]][,"btot"],short.sim[[2]][,"btot"])))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(short.sim[[1]][,"btot"],short.sim[[2]][,"btot"]),0.025))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(short.sim[[1]][,"btot"],short.sim[[2]][,"btot"]),0.975))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.short <- sapply(short.sim[[1]][1:5000,"btot"],f.iceage)
mean(dcq.short[1,])/mean(dcq.short[2,])
mean(dcq.short[1,])/mean(dcq.short[3,])

#fixed effects 
dcq.v <- f.iceage(mean(c(bs1[,2]+bs1[,3])))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(bs1[,2]+bs1[,3]),0.025))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(bs1[,2]+bs1[,3]),0.975))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.fixed <- sapply(c(bs1[1:5000,2]+bs1[1:5000,3]),f.iceage)
mean(dcq.fixed[1,])/mean(dcq.fixed[2,])
mean(dcq.fixed[1,])/mean(dcq.fixed[3,])

# no LAGS 
dcq.v <- f.iceage(mean(c(nolag.sim[[1]][,"btot"],nolag.sim[[2]][,"btot"])))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(nolag.sim[[1]][,"btot"],nolag.sim[[2]][,"btot"]),0.025))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.v <- f.iceage(quantile(c(nolag.sim[[1]][,"btot"],nolag.sim[[2]][,"btot"]),0.975))
dcq.v[1]/dcq.v[2]
dcq.v[1]/dcq.v[3]
dcq.nolag <- sapply(nolag.sim[[1]][1:5000,"btot"],f.iceage)
# costs per QALY
mean(dcq.nolag[1,])/mean(dcq.nolag[2,])
quantile(dcq.nolag[1,],0.975)/quantile(dcq.nolag[2,],0.975)
quantile(dcq.nolag[1,],0.025)/quantile(dcq.nolag[2,],0.025)
# costs per LY
mean(dcq.nolag[1,])/mean(dcq.nolag[3,])
quantile(dcq.nolag[1,],0.975)/quantile(dcq.nolag[3,],0.975)
quantile(dcq.nolag[1,],0.025)/quantile(dcq.nolag[3,],0.025)

save.image("Threshold bootstraps.RData")


# betrouwbaarheidsintervallen coefficienten fixed effects
round(quantile(c(bs1[,2]),c(0.025,0.975)),3)
round(quantile(c(bs1[,3]),c(0.025,0.975)),3)
round(quantile(c(bs1[,2]+bs1[,3]),c(0.025,0.975)),3)

#################################################
# plaatjes + CEAC
#################################################
v1 <- (0:100)*1000
f.ceac <- function(ce,v=50000){
  nmb <- ce[1,]-ce[2,]*v
  length(nmb[nmb<0])/length(nmb)
}
par(mfrow = c(1,1),mex=0.9,cex.axis=1.,cex.lab=1.1,mar = c(4.5,4.5, 2.5, 1.5))
f.ceac1 <- function(v1){f.ceac(ce=dcq.vague,v=v1)} 
plot(sapply(v1,f.ceac1),type="l",ylab="Probability",xlab="\u20ac 1,000",lwd=2);grid()
f.ceac2 <- function(v1){f.ceac(ce=dcq.gallett,v=v1)} 
lines(sapply(v1,f.ceac2),type="l",col="Gray",lwd=2)
f.ceac3 <- function(v1){f.ceac(ce=dcq.claxton,v=v1)} 
lines(sapply(v1,f.ceac3),type="l",col="Gray",lwd=2,lty=2)
f.ceac4 <- function(v1){f.ceac(ce=dcq.short,v=v1)} 
lines(sapply(v1,f.ceac4),type="l",col=1,lwd=2,lty=2)
#f.ceac5 <- function(v1){f.ceac(ce=dcq.nolag,v=v1)} 
#lines(sapply(v1,f.ceac5),type="l",col=1,lwd=2,lty=3)
legend("bottomright",c("Base case","Scenario 1","Scenario 2","Scenario 3"),
       col=c(1,"Gray","Gray",1),lwd=2,lty=c(1,1,2,2),bty="n")

par(mfrow = c(2,1),mex=0.9,cex.axis=1.,cex.lab=1.1,mar = c(4.5,4.5, 2.5, 1.5))
hist(dcq.vague[1,],main="Vague costs")
hist(dcq.vague[2,],main="Vague effects")

# kans 0.5 zoeken mwah, niet nodig, symmetrishce distributies zal wel ongeveer t gemiddelde zijn
f.ceac2 <- function(ce,v=50000){
  nmb <- ce[1,]-ce[2,]*v
  length(nmb[nmb<0])/length(nmb)
}
