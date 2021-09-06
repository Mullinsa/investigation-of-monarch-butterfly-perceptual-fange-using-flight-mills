# packages I may need ####
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(circular)
library(ggplot2)
library(rstudioapi)
library(NISTunits)



# reading in Master files ####
#flights <- read.csv("C:/Users/17196/Box/Monarch Data/R_Projects/Flightmill_Study/Master_Flight_Data.csv")

flights<- read.csv("C:/Users/mullinsa/Box/Monarch Data/R_Projects/Flightmill_Study/Master_Flight_Data.csv")
names(flights) <- tolower(names(flights))
View(flights)

unique(flights$butt_dir)
# This will show if there are any character values.

# Loading 19 and 20 combined weather file
weather <- read.csv("C:/Users/17196/Box/Monarch Data/R_Projects/Flightmill_Study/Master_Weather_Data.csv")
names(weather) <- tolower(names(weather))

# making dates into the same format ####
flights2 <- unite(flights, col = date_time, date, time, sep = ' ')
flights2$date_time <- mdy_hms(flights2$date_time)
flights2$date_time <- floor_date(flights2$date_time, unit = "minute")

master_weather <- unite(weather, col = date_time, date, time, sep = ' ')
master_weather$date_time <- parse_date_time(master_weather$date_time, 'mdy I:M p')

# adding weather to flight data by date_time
flights3 <- left_join(flights2, master_weather, by = 'date_time')
str(flights3) # Make sure numerical and character collumns 

# converting weather station direction to degrees ####
key <- read.csv("C:/Users/17196/Box/Monarch Data/2019_Season/Analysis First Pass 20191021/Direction to Angle Key 20191021.csv")
str(key)

matches <- match(flights3$wind_dir,key$wind_direction)
flights3$wind_angle <- key$wind_angle[matches]

# converting degrees to circular data ####
flights3.circ <- flights3 
flights3.circ$butt_dir <- circular(flights3.circ$butt_dir, units = "degrees",rotation ="clock",template="none")
flights3.circ$manual_wind_dir <- circular(flights3.circ$manual_wind_dir, units = "degrees",rotation ="clock",template="none")
flights3.circ$wind_angle <- circular(flights3.circ$wind_angle, units = "degrees",rotation ="clock",template="none")
class(flights3.circ$butt_dir)
str(flights3.circ)

#### Step 3 ####
# How do we keep the "shortest" path when finding the difference?
# Created function: "circle.split"

flights3.circ$butt_dir_rad<-conversion.circular(flights3.circ$butt_dir)
flights3.circ$manual_wind_dir_rad<-conversion.circular(flights3.circ$manual_wind_dir)

flights3.circ$butt_minus_wind_rad <- flights3.circ$butt_dir_rad - flights3.circ$manual_wind_dir_rad
str(flights3.circ)
# End 1-27-21


####02/03/2021
# Convert degrees to radians or vice versa
#NISTdegTOradian(-90)
#NISTradianTOdeg( -0.785398)

NISTradianTOdeg(asin(sin(NISTdegTOradian(10)-NISTdegTOradian(150))))

circle.split <- function(x) {
  ifelse(x == -180, 180,
         ifelse(x == 360, 0,
                ifelse(x>=0 & x <= 180, as.numeric(x), 
                     ifelse(x>180 & x <= 360, as.numeric(x) - 360, #added =
                              ifelse(x<0 & x > (-180), as.numeric(x),
                                     ifelse(x < (-180) & x >= -360, as.numeric(x) + 360, NA))))))
                      
}

#If x = -180 then its 180
#if x = 360 then its 0
#if x is greater than 0 AND less than or equal to 180 then its x
#if x is greater than 180 AND less than (or =) 360 then subtract 360 from x
#if x is less than 0 AND greater than -180 then its x
#if x is less than -180 AND greater than or equal to -360 then add 360 to x
#if none of the above, then NA


# Test runs:

#circle.split(-290)  #should be 70

# circle.split(10)-circle.split(350) #should be 20
# circle.split(circle.split(240)-circle.split(350)) # should be -110
# circle.split(circle.split(135)-circle.split(225)) # should be -90
# circle.split(circle.split(225)-circle.split(135)) # should be 90

# Checking 'circle.split' function with values from each circular quadrant

#test.data <- data.frame(expand.grid(as.numeric(c(0,35, 45, 55, 125, 135, 145, 215, 225, 235, 305, 315, 325)),
#                                    as.numeric(c(0,35, 45, 55, 125, 135, 145, 215, 225, 235, 305, 315, 325))))
#names(test.data) <- c('fake.butt','fake.wind')
#str(test.data)

#test.data$fake.butt.minus.fake.wind <-
#  circle.split(circle.split(test.data[,1])-circle.split(test.data[,2]))


# converting directions ####
# NOTE: for 2020 butterflies, need to do butt_dir - res_dir (res_dir will always
#be equal to 0 or 360 because it was set @ cart 0)
# as did for angular diff wind

# Creating columns for converted degrees
flights3.circ$converted_butt_dir<-
  circular(circle.split(flights3.circ$butt_dir),
    units = "degrees",rotation ="clock",template="none")

flights3.circ$converted_manual_wind_dir<-
  circular(circle.split(flights3.circ$manual_wind_dir),
    units = "degrees",rotation ="clock",template="none")

#2020 Off-wind butterflies
#flights3.circ$converted_res_dir<-
#  circular(circle.split(flights3.circ$res_dir),
#           units = "degrees",rotation ="clock",template="none")

#Angular difference collunm for butterfly and wind
flights3.circ$angular_diff_wind <-
 circular(circle.split(circle.split(flights3.circ$converted_butt_dir) -
  circle.split(flights3.circ$converted_manual_wind_dir)),
    units = "degrees",rotation ="clock",template="none")

#End 02/03/21






#Start 02/10/21
#### Step 4 ####
# modified  on 20210217 to include mean boundaries (-45 and 45) for butt_orientation
# and added collums for orienting_resource and orienting_wind
# Generate p-value for wind and resource for each individual
names(flights3.circ)
p_vals <- flights3.circ %>%
  filter(id != '66' & id != '176') %>% # these have no manual wind info
  group_by(dist, id, sex) %>%
    summarize(mean_butt_dir = mean.circular(converted_butt_dir,na.rm = T),
              p_val_resource = unlist(rayleigh.test(converted_butt_dir,
                                 mu=circular(0))[2]),
              orienting_resource =ifelse(mean_butt_dir >= (-45) & mean_butt_dir <= 45 & p_val_resource <= .05, 1,0),
              mean_wind_dir = mean.circular(converted_manual_wind_dir, na.rm = T),
 #             mean_wind_dir2 = mean.circular(manual_wind_dir, na.rm = T),
              mean_angular_diff_wind = mean.circular(angular_diff_wind, na.rm =T),
              p_val_wind = unlist(rayleigh.test(angular_diff_wind,
                                 mu=circular(0))[2]),
              orienting_wind =ifelse(mean_angular_diff_wind >= (-45) & mean_angular_diff_wind <= 45 & p_val_wind <= .05, 1,0)) %>%
  ungroup()

plot.circular(p_vals$mean_wind_dir)
plot.circular(p_vals$mean_wind_dir2)
plot.circular(p_vals[p_vals$dist==0,]$mean_wind_dir2)
plot.circular(p_vals[p_vals$dist==0,]$mean_wind_dir)


str(flights3.circ)
str(p_vals)

# step 5 #### modified  on 20210217 based on above criteria (within 45 degrees)
# Classify individuals as orienting to R,W,B,N 
p_vals$orienting <- ifelse(p_vals$orienting_resource == 1 & p_vals$orienting_wind == 1, 'B',
                      ifelse(p_vals$orienting_resource == 0 & p_vals$orienting_wind == 0, 'N',
                        ifelse(p_vals$orienting_resource == 1 & p_vals$orienting_wind == 0, 'R',
                          ifelse(p_vals$orienting_resource == 0 & p_vals$orienting_wind == 1, 'W',NA))))

# step 6 ####
# table of num orienting by dist
summary_table <- table(p_vals$dist,p_vals$orienting)


#chi sq testfor 2019 butterflies
chisq.test(summary_table)
#    B  N  W
#0   1  0 10
#3   5  2  7
#10  6  0  2
#25  6  0  4

#20210218
# 2019 + 2020
#   B  N  R  W
#0   1  2  1 14
#3   8  4  0 16
#10 11  3  0 13
#25  8  2  0 10

# #20210217----Looks like those without a resource (0m) are more likely to just respond to 
# the wind. This is what we would expect given that there are no competing cues.
# Would wind variability explain 0?
# To find out: Include mean wind direction collumn for each butterfly and test whether variability is 
# different between groups.

#Pearson's Chi-squared tests

#2019 only
#data:  summary_table
#X-squared = 14.442, df = 6, p-value = 0.02507
# What this tells us is that the groups (distances) ARE different.

#20210218 
#2019 + 2020
#data:  summary_table
#X-squared = 11.582, df = 9, p-value = 0.2379

# End 02/10/21


# step 7 ####
# 2/23/2021
# Using individual means and p_vals dataframe from Step 6,
# Calculate group means by distance. This uses the individual means in calculating
# group means. I then used tese group means to caluclate their
#resultant length (rho)

# Inclusion of the resultant length (rho) as an additional measure of dispersion
# mean of individual means including resultant length
# did this based on group means instead of calculating an individual rho
# for each individual. 
group_means<- p_vals %>%
  group_by(dist) %>%
  summarize(mean_group_dir = mean.circular(mean_butt_dir,na.rm = T),
            group_mean_angular_diff_wind = mean.circular(mean_angular_diff_wind,na.rm = T),
            group_mean_wind = mean.circular(mean_wind_dir,na.rm = T),
            wind_disp = rho.circular(mean_wind_dir, na.rm = T),
            butt_disp = rho.circular(mean_butt_dir,na.rm = T),
            angular_disp = rho.circular(mean_angular_diff_wind))
          
# Chi-squared test on resultant lengths 
# wind
chisq.test(group_means$wind_disp)
# X-squared = 0.11068, df = 3, p-value = 0.9905

# butterfly
chisq.test(group_means$butt_disp)
#X-squared = 0.19127, df = 3, p-value = 0.979

# angular difference
chisq.test(group_means$angular_disp)
#X-squared = 0.0030824, df = 3, p-value = 1



#Test ---just numbers being converted to radians, but our data is degrees and
# is read as such during analysis. All about the input.
#mean.circular(circular(c(0, -90), units = "degrees",rotation ="clock",template="none"))
#[1] -1.017703
#NISTradianTOdeg(-1.017703) = -58.3101
# Normal mean is 45, must be something specific with circular data
#mean(c(0,-90)) = -45



#####################################################################
# code below uses "p_vals" which is fine b/c it doesnt use rho


# Plots
#plot.circular(select(filter(p_vals,dist == 0),mean_butt_dir),units="degrees",zero= pi/2,rotation='clock',pch=16,cex=0.75,sep=0.06,stack = TRUE,shrink = 1.2,main="0m")
#points.circular(select(filter(p_vals,dist == 0),mean_wind_dir),units="degrees",zero= pi/2,rotation='clock',pch=16,cex=0.75,sep=0.06,stack = TRUE,shrink = 1.2,main="0m")

#Control plots of wind and butt dir
plot.circular(p_vals[p_vals$dist==0,]$mean_butt_dir,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = " No resources")
points.circular(p_vals[p_vals$dist==0,]$mean_wind_dir,zero = pi/2,rotation = 'clock',pch = 1,col="black",next.points = -0.1,cex=0.75,sep=0.06,stack=TRUE)
arrows.circular(mean.circular(p_vals[p_vals$dist==0,]$mean_wind_dir),col="black",zero=pi/2) 
arrows.circular(mean.circular(p_vals[p_vals$dist==0,]$mean_butt_dir),col="red",zero=pi/2)
#rose.diag(p_vals[p_vals$dist==0,]$mean_butt_dir, bins=NULL, col="darkgrey",units= "degrees",
#          zero=pi/2,rotation='clock', cex=0.75, prop=1.3, add=TRUE)
#3m
plot.circular(p_vals[p_vals$dist==3,]$mean_butt_dir,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "3m")
points.circular(p_vals[p_vals$dist==3,]$mean_wind_dir,zero = pi/2,rotation = 'clock',pch = 1,col="black",next.points = -0.1,cex=0.75,sep=0.06,stack=TRUE)
arrows.circular(mean.circular(p_vals[p_vals$dist==3,]$mean_wind_dir),col="black",zero=pi/2) 
arrows.circular(mean.circular(p_vals[p_vals$dist==3,]$mean_butt_dir),col="red",zero=pi/2)


#10m
plot.circular(p_vals[p_vals$dist==10,]$mean_butt_dir,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "10m")
points.circular(p_vals[p_vals$dist==10,]$mean_wind_dir,zero = pi/2,rotation = 'clock',pch = 1,col="black",next.points = -0.1,cex=0.75,sep=0.06,stack=TRUE)
arrows.circular(mean.circular(p_vals[p_vals$dist==10,]$mean_wind_dir),col="black",zero=pi/2) 
arrows.circular(mean.circular(p_vals[p_vals$dist==10,]$mean_butt_dir),col="red",zero=pi/2)

# 25m
plot.circular(p_vals[p_vals$dist==25,]$mean_butt_dir,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "25m")
points.circular(p_vals[p_vals$dist==25,]$mean_wind_dir,zero = pi/2,rotation = 'clock',pch = 1,col="black",next.points = -0.1,cex=0.75,sep=0.06,stack=TRUE)
arrows.circular(mean.circular(p_vals[p_vals$dist==25,]$mean_wind_dir),col="black",zero=pi/2) 
arrows.circular(mean.circular(p_vals[p_vals$dist==25,]$mean_butt_dir),col="red",zero=pi/2)

# Wind normalized (save as 600/450 size)
windows(10, 5)
plot.circular(p_vals[p_vals$dist==0,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="black",
              pch=16,cex=0.75,sep=0.08,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n No resources")

plot.circular(p_vals[p_vals$dist==3,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="black",
              pch=16,cex=0.75,sep=0.08,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n 3m")

plot.circular(p_vals[p_vals$dist==10,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="black",
              pch=16,cex=0.75,sep=0.08,stack = TRUE,
              shrink = 1.4, main ="Angular difference\n 10m")

plot.circular(p_vals[p_vals$dist==25,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="black",
              pch=16,cex=0.75,sep=0.08,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n 25m")
# Plots of angular diff seem to point to more variability 
# From this seems like it would be helpful to determine turn angle


#### Step 8 ####
################################################################################
# Fisher's large-sample test for a common concentration of von Mises distributions
################################################################################

dValues <- function(cdat, ndat, g) {
  N <- length(cdat) ; ndatcsum <- cumsum(ndat) ; dval <- 0 
  for (k in 1:g) {
    sample <- circular(0) 
    if (k==1) {low <- 0} else
      if (k > 1) {low <- ndatcsum[k-1]}
    for (j in 1:ndat[k]) { sample[j] <- cdat[j+low] }
    tm1 <- trigonometric.moment(sample, p=1) ; tbar <- tm1$mu
    dvalk <- abs(sin(sample-tbar))
    dval <- c(dval, dvalk)
  }
  dval <- dval[-1]
  return(dval)
}

FgVal <- function(dvals, ndat, g) {
  N <- length(dvals) ; ndatcsum <- cumsum(ndat) 
  sum1 <- 0 ; sum2 <- 0 ; dk <- 0 ; dbar <- 0 ; gdbar <- 0 
  for (k in 1:g) {
    sample <- circular(0) 
    if (k==1) {low <- 0} else
      if (k > 1) {low <- ndatcsum[k-1]}
    for (j in 1:ndat[k]) { dk[j] <- dvals[j+low] }
    dbar[k] <- sum(dk)/ndat[k]
    sum2 <- sum2 + sum((dk-dbar[k])**2)
    gdbar <- gdbar+ndat[k]*dbar[k]
  }
  gdbar <- gdbar/N
  for (k in 1:g) { sum1 <- sum1 + ndat[k]*(dbar[k]-gdbar)**2 }
  Fg <- (N-g)*sum1/((g-1)*sum2)
  return(Fg)
}


#cdat <- c(cdat1, cdat2, cdat3) 
#n1 <- length(cdat1) ; n2 <- length(cdat2) ; n3 <- length(cdat3) ; N <- n1+n2+n3
#ndat <- c(n1, n2, n3) ; g <- 3

# Figure out how many individuals in each sample for further testing for
# cdat, n and g
p_vals%>%
  group_by(dist)%>%
  summarize(n = n())

# Testing concentration of butterfly orientation
dvals <- dValues(cdat = p_vals$mean_butt_dir, ndat = c(18, 28, 27, 20), g = 4)
FgObs <- FgVal(dvals, ndat= c(18, 28, 27, 20), g = 4)
# F= 3.388659
pf(FgObs, 4-1, 93-4, lower.tail=F)
# p= 0.02149692
# Suggests difference in mean_butterfly direction concentration


# Testing concentration of wind dir for each group
dvals <- dValues(cdat = p_vals$mean_wind_dir, ndat = c(18, 28, 27, 20), g = 4)
FgObs <- FgVal(dvals, ndat= c(18, 28, 27, 20), g = 4)
# F= 5.359939
pf(FgObs, 4-1, 93-4, lower.tail=F)
#p= 0.001937574
# Indicates a difference in the concentration of mean wind direction between groups

# This is testing difference in concentration for angular diff between wind and butterfly
dvals <- dValues(cdat = p_vals$mean_angular_diff_wind, ndat = c(18, 28, 27, 20), g = 4)
FgObs <- FgVal(dvals, ndat= c(18, 28, 27, 20), g = 4)
# F= 1.669304
pf(FgObs, 4-1, 93-4, lower.tail=F) # N = sum of individuals at each dist (18+28+20+27)
# g= number of groups = 4
# 0.179328
# We got a small F and a large p, suggesting no difference in concentration among 
# groups when normalizing for wind

#Wallraf alternative
# report WR instead of Fisher (same conclusions whether variance differs across distances)
# wallraffs at the group level show that WITHIN a distance the variation
# of orientation toward the resource is greater that the variation of orienting 
# toward the wind
# Change methods slightly
# Include wallraff for 0m
# Wallraff for angular diff
WallraffTest(cdat = p_vals$mean_angular_diff_wind, ndat = c(18, 28, 27, 20), g = 4)
# Kruskal-Wallis chi-squared = 3.5415, df = 3, p-value = 0.3154

# Wallraff for butterfly direction
WallraffTest(cdat = p_vals$mean_butt_dir, ndat = c(18, 28, 27, 20), g = 4)
# Kruskal-Wallis chi-squared = 6.8099, df = 3, p-value = 0.07821

# Wallraff for wind direction
WallraffTest(cdat = p_vals$mean_wind_dir, ndat = c(18, 28, 27, 20), g = 4)
# Kruskal-Wallis chi-squared = 7.006, df = 3, p-value = 0.07171

#### 20210324####
# This is testing difference in concentration between
# wind and butterfly direction WITHIN each distanct group
# asking if concentration differs between 
# raw butterfly direction and wind-normalized direction.


sub_0m<- p_vals[p_vals$dist==0,]
dvals <- dValues(cdat = c(sub_0m$mean_butt_dir, sub_0m$mean_angular_diff_wind), ndat = c(nrow(sub_0m),nrow(sub_0m)), g = 2)
FgObs <- FgVal(dvals, ndat= c(nrow(sub_0m), nrow(sub_0m)), g = 2)
#p=0.0181285, F= 6.16455
pf(FgObs, 2-1, nrow(sub_0m)* 2 -2, lower.tail=F)
WallraffTest(cdat = c(sub_0m$mean_butt_dir, sub_0m$mean_angular_diff_wind), ndat = c(nrow(sub_0m),nrow(sub_0m)), g = 2)
#Kruskal-Wallis chi-squared = 16.658, df = 1, p-value = 4.477e-05
# Rejection of common concentration between butterfly direction and
# normalized-by-wind directions

# 3m
sub_3m<- p_vals[p_vals$dist==3,]
dvals <- dValues(cdat = c(sub_3m$mean_butt_dir, sub_3m$mean_angular_diff_wind), ndat = c(nrow(sub_3m),nrow(sub_3m)), g = 2)
FgObs <- FgVal(dvals, ndat= c(nrow(sub_3m), nrow(sub_3m)), g = 2)
#p=0.4854502, F= 0.4933577
pf(FgObs, 2-1, nrow(sub_3m)* 2 -2, lower.tail=F)
WallraffTest(cdat = c(sub_3m$mean_butt_dir, sub_3m$mean_angular_diff_wind), ndat = c(nrow(sub_3m),nrow(sub_3m)), g = 2)
#Kruskal-Wallis chi-squared = 18.013, df = 1, p-value = 2.194e-05

#10m
sub_10m<- p_vals[p_vals$dist==10,]
dvals <- dValues(cdat = c(sub_10m$mean_butt_dir, sub_10m$mean_angular_diff_wind), ndat = c(nrow(sub_10m),nrow(sub_10m)), g = 2)
FgObs <- FgVal(dvals, ndat= c(nrow(sub_10m), nrow(sub_10m)), g = 2)
#p=0.7600747, F=  0.09424471
pf(FgObs, 2-1, nrow(sub_10m)* 2 -2, lower.tail=F)
WallraffTest(cdat = c(sub_10m$mean_butt_dir, sub_10m$mean_angular_diff_wind), ndat = c(nrow(sub_10m),nrow(sub_10m)), g = 2)
#Kruskal-Wallis chi-squared = 10.634, df = 1, p-value = 0.00111

#25m
sub_25m<- p_vals[p_vals$dist==25,]
dvals <- dValues(cdat = c(sub_25m$mean_butt_dir, sub_25m$mean_angular_diff_wind), ndat = c(nrow(sub_25m),nrow(sub_25m)), g = 2)
FgObs <- FgVal(dvals, ndat= c(nrow(sub_25m), nrow(sub_25m)), g = 2)
#p=0.8320175, F=  0.04561584
pf(FgObs, 2-1, nrow(sub_25m)* 2 -2, lower.tail=F)
WallraffTest(cdat = c(sub_25m$mean_butt_dir, sub_25m$mean_angular_diff_wind), ndat = c(nrow(sub_25m),nrow(sub_25m)), g = 2)
#Kruskal-Wallis chi-squared = 7.4641, df = 1, p-value = 0.006294

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

# Bootstrapping shouldnt be necessary here because we have n > 10
#B <- 9999 ; pval <- vMGoFBoot(cdat1, B) ; pval
#pval <- vMGoFBoot(cdat2, B) ; pval
#pval <- vMGoFBoot(cdat3, B) ; pval


#====================================================================================================

# vM2. Maximum likelihood estimation: with bias-correction for kappa

#====================================================================================================

# get estimates of k (concentration parameter) for each distance
# for the angular difference
tapply(p_vals$mean_angular_diff_wind, p_vals$dist, mle.vonmises,bias=T)

# for butterfly direction
tapply(p_vals$mean_butt_dir, p_vals$dist, mle.vonmises,bias=T)


# for wind direction
tapply(p_vals$mean_wind_dir, p_vals$dist, mle.vonmises,bias=T)


vMmle <- mle.vonmises(cdat, bias=TRUE)
muhat <- vMmle$mu ; semu <- vMmle$se.mu ; muhat ; semu
kaphat <- vMmle$kappa ; sekap <- vMmle$se.kappa ; kaphat ; sekap

plot(cdat, ylim=c(-1.2,1), pch=16, stack=TRUE, bins=720)
theta <- circular(seq(0, 2*pi, by=pi/3600))
y <- dvonmises(theta, muhat, kaphat) ; lines(theta, y, lwd=2)
lines(density.circular(cdat, bw=5), lwd=2, lty=2)



################################################################################
# Bootstrap version of Watson's test for a common mean direction
################################################################################

# Appears as though there is an ERROR  in the bootstrap version because the 
# test statistic and p-val dont make sense. Because our sample sizes are
# not much below 25, we used the normal test (not boostrapping) below.
# BUT found some code for the bootstrap test from another site.


YgTestBoot <- function(cdat, ndat, g, indsym, B) {
  N <- length(cdat) ; ndatcsum <- cumsum(ndat) 
  delhat <- 0 ; tbar <- 0 ; centdat <- circular(0)
  for (k in 1:g) {
    sample <- circular(0)  
    if (k==1) {low <- 0} else
      if (k > 1) {low <- ndatcsum[k-1]}
    for (j in 1:ndat[k]) { sample[j] <- cdat[j+low] }
    tm1 <- trigonometric.moment(sample, p=1)
    tm2 <- trigonometric.moment(sample, p=2)
    Rbar1 <- tm1$rho; Rbar2 <- tm2$rho ; tbar[k] <- tm1$mu
    delhat[k] <- (1-Rbar2)/(2*Rbar1*Rbar1)
    if (tbar[k] < 0) {tbar[k] <- tbar[k]+2*pi}
    centsamp <- sample-tbar[k]
    if (indsym == 1) {centsamp <- c(centsamp, -centsamp)}
    centdat <- c(centdat,centsamp)
  }
  centdat <- centdat[-1]
  dhatmax <- max(delhat) ; dhatmin <- min(delhat)
  if (dhatmax/dhatmin <= 4) {
    PorM <- 1 ; CP <- 0 ; SP <- 0 ; dhat0 <- 0
    for (k in 1:g) {
      CP <- CP + ndat[k]*cos(tbar[k])
      SP <- SP + ndat[k]*sin(tbar[k])
      dhat0 <- dhat0 + ndat[k]*delhat[k] 
    }
    dhat0 <- dhat0/N
    RP <- sqrt(CP*CP+SP*SP)
    Yg <- 2*(N-RP)/dhat0
  } else
    if (dhatmax/dhatmin > 4) {
      PorM <- 0 ; CM <- 0 ; SM <- 0 ; Yg <- 0
      for (k in 1:g) {
        CM <- CM + (ndat[k]*cos(tbar[k])/delhat[k])
        SM <- SM + (ndat[k]*sin(tbar[k])/delhat[k])
        Yg <- Yg + (ndat[k]/delhat[k]) 
      }
      RM <- sqrt(CM*CM+SM*SM)
      Yg <- 2*(Yg-RM)
    }
  YgObs <- Yg ; nxtrm <- 1
  
  if (indsym == 0) {
    for (b in 1:B) {
      centsamp <- circular(0) 
      for (k in 1:g) {
        if (k==1) {low <- 0} else
          if (k > 1) {low <- ndatcsum[k-1]}
        for (j in 1:ndat[k]) { centsamp[j] <- centdat[j+low] }
        bootsamp <- sample(centsamp, size=ndat[k], replace=TRUE)
        tm1 <- trigonometric.moment(bootsamp, p=1)
        tm2 <- trigonometric.moment(bootsamp, p=2)
        Rbar1 <- tm1$rho; Rbar2 <- tm2$rho ; tbar[k] <- tm1$mu
        delhat[k] <- (1-Rbar2)/(2*Rbar1*Rbar1)
      }
      if (PorM == 1) {
        CP <- 0 ; SP <- 0 ; dhat0 <- 0
        for (k in 1:g) {
          CP <- CP + ndat[k]*cos(tbar[k])
          SP <- SP + ndat[k]*sin(tbar[k])
          dhat0 <- dhat0 + ndat[k]*delhat[k] 
        }
        dhat0 <- dhat0/N
        RP <- sqrt(CP*CP+SP*SP)
        Yg <- 2*(N-RP)/dhat0
      } else
        if (PorM == 0) {
          CM <- 0 ; SM <- 0 ; Yg <- 0
          for (k in 1:g) {
            CM <- CM + (ndat[k]*cos(tbar[k])/delhat[k])
            SM <- SM + (ndat[k]*sin(tbar[k])/delhat[k])
            Yg <- Yg + (ndat[k]/delhat[k]) 
          }
          RM <- sqrt(CM*CM+SM*SM)
          Yg <- 2*(Yg-RM)
        }
      YgBoot <- Yg
      if (YgBoot >= YgObs) {nxtrm <- nxtrm+1}
    }
    pval <- nxtrm/(B+1)
    return(c(YgObs, pval))
  } else
    
    if (indsym == 1) {
      for (b in 1:B) {
        centsamp <- circular(0) 
        for (k in 1:g) {
          if (k==1) {low <- 0} else
            if (k > 1) {low <- 2*ndatcsum[k-1]}
          for (j in 1:(2*ndat[k])) { centsamp[j] <- centdat[j+low] }
          bootsamp <- sample(centsamp, size=ndat[k], replace=TRUE)
          tm1 <- trigonometric.moment(bootsamp, p=1)
          tm2 <- trigonometric.moment(bootsamp, p=2)
          Rbar1 <- tm1$rho; Rbar2 <- tm2$rho ; tbar[k] <- tm1$mu
          delhat[k] <- (1-Rbar2)/(2*Rbar1*Rbar1)
        }
        if (PorM == 1) {
          CP <- 0 ; SP <- 0 ; dhat0 <- 0
          for (k in 1:g) {
            CP <- CP + ndat[k]*cos(tbar[k])
            SP <- SP + ndat[k]*sin(tbar[k])
            dhat0 <- dhat0 + ndat[k]*delhat[k] 
          }
          dhat0 <- dhat0/N
          RP <- sqrt(CP*CP+SP*SP)
          Yg <- 2*(N-RP)/dhat0
        } else
          if (PorM == 0) {
            CM <- 0 ; SM <- 0 ; Yg <- 0
            for (k in 1:g) {
              CM <- CM + (ndat[k]*cos(tbar[k])/delhat[k])
              SM <- SM + (ndat[k]*sin(tbar[k])/delhat[k])
              Yg <- Yg + (ndat[k]/delhat[k]) 
            }
            RM <- sqrt(CM*CM+SM*SM)
            Yg <- 2*(Yg-RM)
          }
        YgBoot <- Yg
        if (YgBoot >= YgObs) {nxtrm <- nxtrm+1}
      }
      pval <- nxtrm/(B+1)
      return(c(YgObs, pval))
    }
}

indsym <- 1 ; B <- 9999  

YgTestBoot(cdat = p_vals$mean_butt_dir, ndat = c(18, 28, 27, 20), g = 4, indsym, B)
#Yg= 7.175934
#p= 0.231800
YgTestBoot(cdat = p_vals$mean_wind_dir, ndat = c(18, 28, 27, 20), g = 4, indsym, B)
YgTestBoot(cdat = p_vals$mean_angular_diff_wind, ndat = c(18, 28, 27, 20), g = 4, indsym, B)



#######non-bootstrap version
################################################################################
# Calculation of Yg test statistic for Watson's test for a common mean direction (pg 135)
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

# Test of common mean wind direction
YgObs <- YgVal(cdat = p_vals$mean_wind_dir, ndat = c(18, 28, 27, 20), g = 4)
pchisq(YgObs, 4-1, lower.tail=F)
#Yg= 15.7208, p= 0.001293664 # This is difference in mean wind direction across distances

# Test of common mean butterfly direction
YgObs <- YgVal(cdat = p_vals$mean_butt_dir, ndat = c(18, 28, 27, 20), g = 4)
pchisq(YgObs, 4-1, lower.tail=F)
# Yg= 7.175934, p= 0.06649661 #  weak non-signicicant difference in mean butt direction across distances


# Noticed that by chance, the 0m groups is completely difference than others,
# could this be driving the difference?
#Omitting 0 group to see if they drive any difference in butterfly direction
YgObs <- YgVal(cdat = p_vals[p_vals$dist!=0,]$mean_butt_dir, ndat = c( 28, 27, 20), g = 3)
pchisq(YgObs, 3-1, lower.tail=F)
#Yg= 3.765358, p= 0.1521819 
# Yes, After omitting the 0 group, there is no difference in common mean but direction direction


# The real test. Does the mean angular difference change?
YgObs <- YgVal(cdat = p_vals$mean_angular_diff_wind, ndat = c(18, 28, 27, 20), g = 4)
pchisq(YgObs, 4-1, lower.tail=F)
# Yg= 5.781041, p= 0.1227628 # No difference in mean  direction of butts correcting for wind




# is angular difference more variable when wind is from res or not? ####
# graphs below suggest it's not a big enough pattern to make
# any conclusions about
p_vals$wind_from_res <- ifelse(p_vals$mean_wind_dir >=-60 &
                                 p_vals$mean_wind_dir <=60,
                               1, 0)
plot.circular(p_vals[p_vals$dist==0 & p_vals$wind_from_res==1,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n No resources")
points.circular(p_vals[p_vals$dist==0 & p_vals$wind_from_res==0,]$mean_angular_diff_wind,
              zero=pi/2,rotation='clock',col="blue",
              pch=16,cex=0.75,sep=0.06,stack = TRUE)

plot.circular(p_vals[p_vals$dist==3 & p_vals$wind_from_res==1,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n 3m")
points.circular(p_vals[p_vals$dist==3 & p_vals$wind_from_res==0,]$mean_angular_diff_wind,
                zero=pi/2,rotation='clock',col="blue",
                pch=16,cex=0.75,sep=0.06,stack = TRUE)

plot.circular(p_vals[p_vals$dist==10 & p_vals$wind_from_res==1,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n 10m")
points.circular(p_vals[p_vals$dist==10 & p_vals$wind_from_res==0,]$mean_angular_diff_wind,
                zero=pi/2,rotation='clock',col="blue",
                pch=16,cex=0.75,sep=0.06,stack = TRUE)

plot.circular(p_vals[p_vals$dist==25 & p_vals$wind_from_res==1,]$mean_angular_diff_wind,units= "degrees",
              zero=pi/2,rotation='clock',col="red",
              pch=16,cex=0.75,sep=0.06,stack = TRUE,
              shrink = 1.4, main = "Angular difference\n 25m")
points.circular(p_vals[p_vals$dist==25 & p_vals$wind_from_res==0,]$mean_angular_diff_wind,
                zero=pi/2,rotation='clock',col="blue",
                pch=16,cex=0.75,sep=0.06,stack = TRUE)
