
# packages ####
library(dplyr)
library(circular)
library(data.table)
library(circular)
library(ggplot2)
library(NISTunits)
library(data.table)

# Custom functions
# Apply the circle.split 
circle.split <- function(x) {
  ifelse(x == -180, 180,
         ifelse(x == 360, 0,
                ifelse(x>=0 & x <= 180, as.numeric(x), 
                       ifelse(x>180 & x <= 360, as.numeric(x) - 360, #added =
                              ifelse(x<0 & x > (-180), as.numeric(x),
                                     ifelse(x < (-180) & x >= -360, as.numeric(x) + 360, NA))))))
  
}

# single function for hatk (kurtosis) ####
hatk_fx <- function(x) {
  t2t <- trigonometric.moment(x, p=2, center=TRUE)
  abar2 <- t2t$cos
  Rbar <- rho.circular(x, na.rm=T)
  V <- 1-Rbar 
  (abar2-Rbar^4)/(V^2)
}


# single function for abar2 (pre cursor to hatk) 
abar2_fx <- function(x) {
  t2t <- trigonometric.moment(x, p=2, center=T)
  abar2 <- t2t$cos
  abar2
}

# does mixing pos and neg angles mess up abar2_fx?
test.rads <- circular(rep(c(pi/4,pi/4,3*pi/4,3*pi/4,NA,NA,NA,pi/2,pi/2,pi/2,0,pi),each=5))
plot.circular(test.rads,stack=T,shrink = 2)
test.rads.2 <- circular(rep(c(pi/4,pi/4,3*pi/4,3*pi/4,NA,NA,NA,pi/2,-3*pi/2,pi/2,0,-pi),each=5))
plot.circular(test.rads.2,stack=T,shrink = 2)

abar2_fx(test.rads)
abar2_fx(test.rads.2)
# doesn't seem to...
# how does it go negative???
# this paper suggests circular kurtosis can be negative (as low -4)
# https://www.maths.univ-evry.fr/prepubli/350.pdf

# Reading in main df ####
flights3.circ <- read.csv("C:/Users/17196/Box/Monarch Data/R_Projects/Flightmill_Study/flights3.circ.csv")# Skip first measurement then use previous to track angle change

#for JSA machine:
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#flights3.circ <- read.csv("flights3.circ.csv")

flights3.circ <- flights3.circ %>%
  filter(id != '66' & id != '176') # these seem to have no manual wind infoView(flights3.circ)
# Making df circular
circular(flights3.circ,units = "degrees",rotation ="clock",template="none")

# Calculating turn angles ####
flights3.circ$converted_butt_dir <- circular(flights3.circ$converted_butt_dir,units = "degrees",rotation ="clock",template="none")
  
flights3.circ.short <- flights3.circ %>%
  select(2:4,converted_butt_dir) %>%
  group_by(id) %>%
  mutate(turn_angle = circular(circle.split(circle.split(converted_butt_dir) - circle.split((shift(converted_butt_dir,1,type='lag')))),
                               units = "degrees",rotation ="clock",template="none")) %>%
  ungroup()

str(flights3.circ.short)


# Plotting individual histograms for turn angles ####
flights3.circ.short.0<- filter(flights3.circ.short, dist==0, is.na(turn_angle)==F)
View(flights3.circ.short.0)
ggplot(data=flights3.circ.short.0,aes(x= turn_angle,after_stat(density))) +
  geom_histogram(binwidth = 25) +
  geom_density(alpha=.2, fill="#FF6666") +
  #coord_polar()+
  facet_wrap(~id) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks=c(-180, -90 , 0, 90, 180)
,limits=c(-180, 180))



flights3.circ.short.3<- filter(flights3.circ.short, dist==3,is.na(turn_angle)==F)
ggplot(data=flights3.circ.short.3,aes(x=turn_angle,after_stat(density))) +
  geom_histogram(binwidth = 25) +
  geom_density(alpha=.2, fill="#FF6666") +
  #coord_polar()+
  facet_wrap(~id) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks=c(-180, -90 , 0, 90, 180)
                     ,limits=c(-180, 180))

flights3.circ.short.10<- filter(flights3.circ.short, dist==10,is.na(turn_angle)==F)
ggplot(data=flights3.circ.short.10,aes(x=turn_angle,after_stat(density))) +
  geom_histogram(binwidth = 25) +
  geom_density(alpha=.2, fill="#FF6666") +
  #coord_polar()+
  facet_wrap(~id) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks=c(-180, -90 , 0, 90, 180)
                     ,limits=c(-180, 180))

flights3.circ.short.25<- filter(flights3.circ.short, dist==25,is.na(turn_angle)==F)
ggplot(data=flights3.circ.short.25,aes(x=turn_angle,after_stat(density))) +
  geom_histogram(binwidth = 25) +
  geom_density(alpha=.2, fill="#FF6666") +
  #coord_polar()+
  facet_wrap(~id) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks=c(-180, -90 , 0, 90, 180)
                     ,limits=c(-180, 180))

# Creating a radians column ####
flights3.rad<- flights3.circ.short %>%
  group_by(id,dist) %>%
  mutate(turn_angle.rad = NISTdegTOradian(turn_angle)) %>%
  ungroup()

str(flights3.rad)

# Making angles into clockwise radians
flights3.rad$turn_angle.rad<-circular(flights3.rad$turn_angle.rad,units = "radians",rotation ="clock",template="none")

# what if we bin degrees into wider pie slices, e.g. 10deg
# flights3.rad$turn_angle.binned <- ceiling(flights3.rad$turn_angle/30)*30
# max(flights3.rad$turn_angle.binned, na.rm=T);min(flights3.rad$turn_angle.binned, na.rm=T)

# Summary table
turn_angle_table <- flights3.rad %>%
  group_by(dist, id) %>%
  summarize(mean_turn_angle = mean(turn_angle,na.rm=T),
            rho = rho.circular(turn_angle.rad, na.rm = T),
            ang.dev = angular.deviation(turn_angle.rad, na.rm = T),
            pval = rayleigh.test(turn_angle.rad, mu=circular(0,type='angle',units='radian',rotation='clock'))[[2]],
            kurtosis = hatk_fx(turn_angle.rad),
            #kurtosis.binned = hatk_fx(turn_angle.binned),
            abar2 = abar2_fx(turn_angle.rad))%>%
            #abar2.binned = abar2_fx(turn_angle.binned))%>%
  ungroup()


# Wallraff test on turn angle and distance groups ####
# null hyp is that samples in groups were drawn 
# from distributions with same concentration (akin to variance)
# if p < 0.05, evidence that concentrations in groups differ

cdat <- turn_angle_table$mean_turn_angle 
ndat <- as.numeric(tapply(turn_angle_table$mean_turn_angle,turn_angle_table$dist,length)) # counts the number of individuals at each distance
g <- n_distinct(turn_angle_table$dist) # counts number of groups (ie different distances tested)

WallraffTest <- function(cdat, ndat, g) {
  N <- length(cdat) ; ndatcsum <- cumsum(ndat) ; tbar <- circular(0) ; distdat <- 0
  for (k in 1:g) {
    dist <- 0 ; sample <- circular(0)  
    if (k==1) {low <- 0} else
      if (k > 1) {low <- ndatcsum[k-1]}
    for (j in 1:ndat[k]) { sample[j] <- cdat[j+low] }
    tm1 <- trigonometric.moment(sample, p=1) ; tbar[k] <- tm1$mu
    for (j in 1:ndat[k]) { dist[j] <- pi-abs(pi-abs(sample[j]-tbar[k])) }
    distdat <- c(distdat, dist)
  }
  distdat <- distdat[-1]
  gID <- rep(c(1:length(ndat)), ndat) # NOTE: replaced original code from Pewsey book, b/c it was specific to the number of groups and sample sizes used in their examples. This code derives the appropriate numbers from the data input into the function, so is flexible to different numbers of groups and different sample sizes. Their code was: c(rep(1,n1), rep(2,n2), rep(3,n3))
  TestRes <- kruskal.test(distdat, g=gID)
  return(TestRes)
} 

WallraffTest(cdat, ndat, g)
# Kruskal-Wallis rank sum test
# data:  distdat and gID
# Kruskal-Wallis chi-squared = 4.386, df = 3, p-value = 0.2227
# Wallraff test indicates mean turn angle does not differ between distance groups


# linear models (ANOVAs) on rho and hatk

rho.lm <- lm(rho ~ as.factor(dist), data = turn_angle_table)
anova(rho.lm)
#                Df Sum Sq  Mean Sq F value Pr(>F)
#as.factor(dist)  3 0.1056 0.035207   0.859 0.4655
#Residuals       89 3.6477 0.040985

summary(rho.lm)
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.33216    0.04772   6.961 5.54e-10 ***
#as.factor(dist)3   0.04456    0.06116   0.729    0.468    
#as.factor(dist)10  0.08213    0.06160   1.333    0.186    
#as.factor(dist)25  0.09289    0.06577   1.412    0.161

# Variability in turn angle, measured as resultant length,
# did not appear to differ with distance from the resource patch 
# (F3,89 = 0.86, p = 0.47, r2 = 0.03).  

# ANOVA on hatk
hatk.lm <- lm(kurtosis ~ as.factor(dist), data = turn_angle_table)
anova(hatk.lm)
#Response: kurtosis
# Df Sum Sq Mean Sq F value Pr(>F)
# as.factor(dist)  3  1.116  0.3720  1.1855 0.3199
# Residuals       89 27.928  0.3138 
# Variability in turn angle, measured as resultant length,
# did not appear to differ with distance from the resource patch
# (F3,89 = 1.116, p = 0.47, r2 = 0.03).  


summary(hatkb.lm)
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.11772    0.13204   0.892    0.375
# as.factor(dist)3   0.06474    0.16924   0.383    0.703
# as.factor(dist)10 -0.07527    0.17046  -0.442    0.660
# as.factor(dist)25  0.22918    0.18200   1.259    0.211

# Residual standard error: 0.5602 on 89 degrees of freedom
# Multiple R-squared:  0.03842,	Adjusted R-squared:  0.006011 
# F-statistic: 1.185 on 3 and 89 DF,  p-value: 0.3199

# Variability in turn angle, measured as kurtosis, 
# did not appear to differ with distance from the 
# resource patch (F3,89 = 0.86, p = 0.32, r2 = 0.006).  

# ANOVA on angular deviation
dev.lm <- lm(ang.dev ~ as.factor(dist), data = turn_angle_table)
anova(dev.lm)

# Response: ang.dev
#                 Df Sum Sq  Mean Sq F value Pr(>F)
# as.factor(dist)  3 0.1229 0.040978  1.0233 0.3863
# Residuals       89 3.5641 0.04004

summary(dev.lm)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        1.14683    0.04717  24.314   <2e-16 ***
#  as.factor(dist)3  -0.04317    0.06046  -0.714    0.477    
# as.factor(dist)10 -0.09283    0.06089  -1.525    0.131    
# as.factor(dist)25 -0.09248    0.06502  -1.422    0.158    

# Residual standard error: 0.2001 on 89 degrees of freedom
# Multiple R-squared:  0.03334,	Adjusted R-squared:  0.0007583 
# F-statistic: 1.023 on 3 and 89 DF,  p-value: 0.3863
# Variability in turn angle, measured as angular deviation,
# did not appear to differ with distance from the resource patch
# (F3,89 = 1.0233, p = 0.38, r2 = 0.03).

# Watson's test of common concentration on turn angles
#non-bootstrap version
################################################################################
# Calculation of Yg test statistic for Watson's test for a common mean direction
################################################################################

YgVal <- function(cdat, ndat, g) {
  N <- length(cdat) ; ndatcsum <- cumsum(ndat) 
  delhat <- 0 ; tbar <- 0
  for (k in 1:g) {
    sample <- circular(0)
    if (k==1) {low <- 0} else
      if (k > 1) {low <- ndatcsum[k-1]}
    for (j in 1:ndat[k]) { sample[j] <- cdat[j+low] }
    tm1 <- trigonometric.moment(sample, p=1)
    tm2 <- trigonometric.moment(sample, p=2)
    Rbar1 <- tm1$rho; Rbar2 <- tm2$rho ; tbar[k] <- tm1$mu
    delhat[k] <- (1-Rbar2)/(2*Rbar1*Rbar1)
  }
  dhatmax <- max(delhat) ; dhatmin <- min(delhat)
  if (dhatmax/dhatmin <= 4) {
    CP <- 0 ; SP <- 0 ; dhat0 <- 0
    for (k in 1:g) {
      CP <- CP + ndat[k]*cos(tbar[k])
      SP <- SP + ndat[k]*sin(tbar[k])
      dhat0 <- dhat0 + ndat[k]*delhat[k] 
    }
    dhat0 <- dhat0/N
    RP <- sqrt(CP*CP+SP*SP)
    Yg <- 2*(N-RP)/dhat0
    return(Yg) } 
  else if (dhatmax/dhatmin > 4) {
    CM <- 0 ; SM <- 0 ; Yg <- 0
    for (k in 1:g) {
      CM <- CM + (ndat[k]*cos(tbar[k])/delhat[k])
      SM <- SM + (ndat[k]*sin(tbar[k])/delhat[k])
      Yg <- Yg + (ndat[k]/delhat[k]) 
    }
    RM <- sqrt(CM*CM+SM*SM)
    Yg <- 2*(Yg-RM)
    return(Yg) }
}

# Test of common mean turn angle
YgObs <- YgVal(cdat = NISTdegTOradian(turn_angle_table$mean_turn_angle), ndat = c(18, 28, 27, 20), g = 4)
pchisq(YgObs, 4-1, lower.tail=F)
#Yg= 11.77738, p= 0.008185993 # This is difference in mean wind direction across distances

# the data don't look different at all, why is the above giving low p?
# is it b/c the Watson's test requires radians? See result below...
ggplot(turn_angle_table, aes(x=as.factor(dist), y=mean_turn_angle)) +
  geom_violin() + 
  geom_point()

ggplot(turn_angle_table, aes(x=mean_turn_angle)) +
  geom_histogram() + 
  coord_polar(start=0) +
  facet_wrap(~as.factor(dist))

#Watson's test on mean turn angle (Watsons tests for a circular uniform distribution)
YgObs <- YgVal(cdat = NISTdegTOradian(turn_angle_table$mean_turn_angle), ndat = c(18, 28, 27, 20), g = 4)
pchisq(YgObs, 4-1, lower.tail=F)
#Yg= 0.8545586, p= 0.8363777

# in clockwise degrees and angles
kurtosis <- flights3.circ.short %>%
  group_by(dist, id) %>%
  summarize(hatk = hatk_fx(NISTdegTOradian(turn_angle)), .groups = 'drop')
str(kurtosis)

ggplot(kurtosis, aes(x=as.factor(dist), y = hatk)) +
  geom_boxplot() +
  geom_point()+
  xlab ("Patch distance") +
  ylab ("Kurtosis") +
  scale_x_discrete(labels=c("No resource", "3m", "10m", "25m"))

# what does rho look like by dist
ggplot(turn_angle_pvals, aes(x=as.factor(dist), rho)) +
  geom_boxplot() +
  geom_point() +
  xlab ("Patch distance") +
  ylab ("Resultant length") +
  scale_x_discrete(labels=c("No resource", "3m", "10m", "25m"))

View(turn_angle_pvals)


# angular dev plot
ggplot(turn_angle_pvals, aes(x=as.factor(dist), y= ang.dev)) +
  geom_boxplot() +
  geom_point() +
  xlab("Patch distance") +
  ylab("Angular deviation") +
  scale_x_discrete(labels=c("No resource", "3m", "10m", "25m"))
