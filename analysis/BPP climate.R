#### quickly plot long-term climate trends from Moose Jaw, SK, station
### Annual temperature
### Summer, winter, annual precip

library(ggplot2) ## for plotting
library(cowplot)
library(mgcv) ## basic model

### Figure theme
setwd("buffalo-pelican/data")
source('figure-theme.R')

#### temperature model
## data
temp <- read.csv(file.choose(), header=TRUE)

### annual temperature
mjT <- gam(Annual ~s(Year), data = temp, family=gaussian(), method="REML")

summary(mjT) 
plot(mjT) 
gam.check(mjT) 

pred <- with(temp, data.frame(Year = seq(min(Year), max(Year),
                                          length.out = 200)))
pred <- cbind(pred,
                data.frame(predict(mjT, pred, se.fit = TRUE)))
pred <- transform(pred,
                    upper = fit + (2 * se.fit),
                    lower = fit - (2 * se.fit))

### plot
MAT<-
  ggplot(temp)+
  geom_point(aes(Year, Annual), size=2, pch = 21, color='black', bg="red")+
  geom_ribbon(data=pred, aes(ymin=lower, ymax=upper, x=Year), alpha=0.2)+
  geom_line(data=pred, aes(Year, fit), size=1, color="black")+
  scale_x_continuous(breaks=seq(1900,2020,by=20))+
  ylab('Mean annual temperature (C)')+
  xlab('Year C.E.')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
MAT
ggsave("mjtemperature.pdf", MAT, dpi=300, scale=1)

#### precipitation data
precip <- read.csv(file.choose(), header=TRUE)

## Annual model
mjP<-gam(Annual~s(Year, bs="tp"), family=Gamma(link="log"), 
         data = precip, method = "REML")

summary(mjP) 
plot(mjP)
gam.check(mjP)

##backtransform family
fam<-family(mjP)
fam
str(fam) 
ilink <- fam$linkinv
ilink 
Gamma()$linkinv 
ilink <- family(mjP)$linkinv 

pred1 <- with(precip, data.frame(Year = seq(min(Year), max(Year),
                                          length.out = 200)))
pred1 <- cbind(pred1,
               data.frame(predict(mjP, pred1, se.fit = TRUE))) 
pred1 <- transform(pred1, fit = ilink(fit), 
                    upper = ilink(fit + (2 * se.fit)),
                    lower = ilink(fit - (2 * se.fit)))

### plot
mjP<-
  ggplot(precip)+
  geom_point(aes(Year, Annual), size=1.25, pch = 21, color='black', bg="blue")+
  geom_ribbon(data=pred1, aes(ymin=lower, ymax=upper, x=Year), alpha=0.2)+
  geom_line(data=pred1, aes(Year, fit), size=1, color="black")+
  ylab('Annual precipitation (mm)')+
  xlab('Year C.E.')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
mjP

### Summer precipitation model
mjS<-gam(Summer~s(Year, bs="tp"), family=Gamma(link="log"), 
         data = precip, method = "REML")

summary(mjS) 
plot(mjS) 
gam.check(mjS) 

##backtransform family
fam<-family(mjS)
fam
str(fam) 
ilink <- fam$linkinv
ilink 
Gamma()$linkinv 
ilink <- family(mjS)$linkinv 

pred2 <- with(precip, data.frame(Year = seq(min(Year), max(Year),
                                            length.out = 200)))
pred2 <- cbind(pred2,
               data.frame(predict(mjS, pred2, se.fit = TRUE))) 
pred2 <- transform(pred2, fit = ilink(fit), 
                   upper = ilink(fit + (2 * se.fit)),
                   lower = ilink(fit - (2 * se.fit)))

### plot
mjS<-
  ggplot(precip)+
  geom_point(aes(Year, Summer), size=1.25, pch = 21, color='black', bg="blue")+
  geom_ribbon(data=pred2, aes(ymin=lower, ymax=upper, x=Year), alpha=0.2)+
  geom_line(data=pred2, aes(Year, fit), size=1, color="black")+
  ylab('Summer precipitation (mm)')+
  xlab('Year C.E.')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
mjS


#### Winter precipitation 
mjW<-gam(Winter~s(Year, bs="tp"), family=Gamma(link="log"), 
         data = precip, method = "REML")

summary(mjW) 
plot(mjW) 
gam.check(mjW) 

##backtransform family
fam<-family(mjW)
fam
str(fam) 
ilink <- fam$linkinv
ilink 
Gamma()$linkinv 
ilink <- family(mjW)$linkinv 

pred3 <- with(precip, data.frame(Year = seq(min(Year), max(Year),
                                            length.out = 200)))
pred3 <- cbind(pred3,
               data.frame(predict(mjW, pred3, se.fit = TRUE))) 
pred3 <- transform(pred3, fit = ilink(fit), 
                   upper = ilink(fit + (2 * se.fit)),
                   lower = ilink(fit - (2 * se.fit)))

### plot
mjW<-
  ggplot(precip)+
  geom_point(aes(Year, Winter), size=1.25, pch = 21, color='black', bg="blue")+
  geom_ribbon(data=pred3, aes(ymin=lower, ymax=upper, x=Year), alpha=0.2)+
  geom_line(data=pred3, aes(Year, fit), size=1, color="black")+
  ylab('Winter precipitation (mm)')+
  xlab('Year C.E.')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
mjW

mjprecip <- plot_grid(mjP, mjS, mjW, ncol=1, nrow=3)
mjprecip

ggsave("mjprecp.pdf", mjprecip, dpi=300, scale=1)
