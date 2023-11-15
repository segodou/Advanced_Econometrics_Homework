####################  Midterm 
# Authors : 
# -Nelly AGOSSOU
# -Jean-Baptiste GOMEZ
# -Ulrich SEGODO

library(tidyverse)
library(broom)
library(ggplot2)
library(here)
library(modelr)
library(sf)
library(sp)
library(spdep)
library(lmtest)
library(car)
library(psych)
library(spatialreg)
library(readr)
library(dplyr)
library(magrittr)
library(mgcv)
library(GGally)
library(stargazer)
Paris= read.csv("C:/Users/Katinan AGOSSOU/Desktop/homework-M2-group6/homework-M2-group6-data.csv",sep=",")
Paris=tibble(Paris)

#descriptive statistics
colnames(Paris)
str(Paris)

#variables quanti
##realSum,person_capacity,cleanliness_rating,bedrooms,dist,metro_dist,lat,lng,attr_index,attr_index_norm,rest_index,rest_index_norm

#variables quali
##room_type,host_is_superhost,multi ,biz ,day ,city 
Paris=mutate(Paris,realSum=as.numeric(realSum),person_capacity=as.numeric(person_capacity),cleanliness_rating=as.numeric(cleanliness_rating),guest_satisfaction_overall=as.numeric(guest_satisfaction_overall),dist=as.numeric(dist),metro_dist=as.numeric(metro_dist),lng=as.numeric(lng),lat=as.numeric(lat),room_type=as.factor(room_type) ,room_shared=as.factor(room_shared) ,room_private=as.factor(room_private) ,host_is_superhost=as.factor(host_is_superhost) ,multi=as.factor(multi) ,multi=as.factor(multi) ,biz=as.factor(biz),day =as.factor(day),city =as.factor(city), rest_index=as.numeric(rest_index),attr_index=as.numeric(attr_index),rest_index_norm=as.numeric(rest_index_norm),attr_index_norm=as.numeric(attr_index_norm))
summary(Paris)
stats <- aggregate(realSum ~ city, data = Paris, FUN = function(x) c( mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
print(stats)
stats1 <- aggregate(cbind(person_capacity, cleanliness_rating, bedrooms, dist, metro_dist) ~ city, data = Paris, FUN = function(x) c( mean = mean(x), sd = sd(x)))
print(stats1)
prop.table(table(Paris$city, Paris$room_type), margin = 1)
prop.table(table(Paris$city, Paris$host_is_superhost), margin = 1)
prop.table(table(Paris$city, Paris$multi), margin = 1)
prop.table(table(Paris$city, Paris$biz), margin = 1)


###Correlation analysis
#correlation matrix
num_cols= sapply(Paris, is.numeric)
num_data= Paris[, num_cols]
mcorr=cor(num_data)
mcorr
cor.plot(mcorr,numbers=TRUE,diag=FALSE,stars=TRUE,main="Correlation plot",min.length=4)
ks.test(Paris$room_type,Paris$realSum)
ks.test(Paris$multi,Paris$realSum)
ks.test(Paris$biz,Paris$realSum)
ks.test(Paris$city,Paris$realSum)

##spatial representation
log=read_sf("C:/Users/Katinan AGOSSOU/Desktop/homework-M2-group6/logement-encadrement-des-loyers")
ggplot(log)+
  geom_sf()
mescoordparis=Paris%>%
  select(lng,lat)%>%
  as.matrix()%>%
  st_multipoint()%>%
  st_geometry()

st_crs(mescoordparis)=4326
mescoordparis=st_cast(mescoordparis,to="POINT")
st_geometry(Paris)=mescoordparis
map1=ggplot(log)+
  geom_sf(fill="white")+
  geom_sf(data = Paris,aes(color=realSum),pch=15)+
  scale_color_continuous(low="blue", high="green",limits = c(100, 500))
map1

map2=ggplot(log)+
  geom_sf(fill="white")+
  geom_sf(data = Paris,aes(color=rest_index_norm),pch=15)+
  scale_color_continuous(low="green", high="red",limits = c(10, 100))
map2

map3=ggplot(log)+
  geom_sf(fill="white")+
  geom_sf(data = Paris,aes(color=attr_index_norm),pch=15)+
  scale_color_continuous(low="green", high="red",limits = c(10, 100))
map3

######################################### Parametric Model #################################################

####Multicolinearity test
reg1=lm(log(Paris$realSum)~Paris$room_type+Paris$person_capacity+Paris$host_is_superhost+Paris$multi+Paris$biz+Paris$bedrooms+Paris$dist+Paris$metro_dist+Paris$attr_index_norm+Paris$rest_index_norm+Paris$day)
summary(reg1)
vif(reg1)

#### OLS
regParis=lm(log(Paris$realSum)~Paris$room_type+Paris$person_capacity+Paris$host_is_superhost+Paris$multi+Paris$biz+Paris$bedrooms+Paris$dist+Paris$metro_dist+Paris$day+Paris$attr_index_norm)
summary(regParis)
BIC(regParis)
AIC(regParis)


######################################### NonParametric Model #########################################

###################  GAM Model

Paris[[1]]<-log(Paris[,1])
# View(Paris)
attach(Paris)

ggpairs(data = Paris, columns = c("realSum", "person_capacity", "multi", "biz","dist")) #multi, biz, person_capacity and dist are linear relationship with realSum
ggpairs(data = Paris, columns = c("realSum","guest_satisfaction_overall", "metro_dist", "attr_index", "rest_index", "lng", "lat"))


gam_model <- gam(realSum~ s(guest_satisfaction_overall) + s(metro_dist) + s(attr_index) + s(rest_index) + s(lng) +s(lat), data = Paris)
summary(gam_model)

par(mfrow=c (2,3))
plot(gam_model, shade=T, shade.col="lightgreen" , ylim=c(-.5 , .7))

vis.gam(gam_model, view =c("rest_index", "metro_dist"), phi = 20)
vis.gam(gam_model, view =c("guest_satisfaction_overall", "attr_index"), phi = 20)
vis.gam(gam_model, view =c("lng", "lat"), phi = 20)


# interpretation: 
# We can examinate the effect of each realSum_i on y_i indiviually while holding all of others variables fixed.
# As we can see with the p_value of each explanatory variables, guest_satisfaction_overall, metro_dist, attr_index, lng and lat are highly nonlinear.
# So we can consider a more flexible model.



gam_model_flex<- gam(realSum~ s(rest_index) + s(metro_dist) + s(guest_satisfaction_overall,attr_index,lng,lat), data = Paris)
summary(gam_model_flex)

par(mfrow=c (1,3))
plot(gam_model_flex, shade=T, shade.col="lightgreen" , ylim=c(-.5 , .7))

vis.gam(gam_model_flex, view =c("rest_index", "metro_dist"), phi = 20)
vis.gam(gam_model_flex, view =c("guest_satisfaction_overall", "attr_index"), phi = 20)
vis.gam(gam_model_flex, view =c("lng", "lat"), phi = 20)

# Specify a smooth term using basic splines

gam_model_flex1_base<- gam(realSum~ s(rest_index) + s(guest_satisfaction_overall,metro_dist,attr_index,lng,lat), data = Paris,  method = "REML")
summary(gam_model_flex1_base)

vis.gam(gam_model_flex1_base, view =c("rest_index", "metro_dist"), phi = 20)
vis.gam(gam_model_flex1_base, view =c("guest_satisfaction_overall", "attr_index"), phi = 20)
vis.gam(gam_model_flex1_base, view =c("lng", "lat"), phi = 20)

#### MODEL SAR
###Spatial Model SAR
# test de Moran
dataT <- as.numeric(Paris$realSum)
coord <- data.frame(x = Paris$lng, y = Paris$lat)
W <- knn2nb(knearneigh(coord, k = 1))
W_listw <- nb2listw(W)
moran_result <- moran.test(x =dataT, listw = W_listw)
print(moran_result)## significatif 
##model sar
model_sar<- lagsarlm(log(Paris$realSum)~Paris$room_type+ Paris$person_capacity+Paris$host_is_superhost+Paris$multi+Paris$biz+Paris$bedrooms+Paris$dist+Paris$metro_dist+Paris$attr_index_norm+Paris$rest_index_norm+Paris$day, data = Paris, listw =W_listw)
summary(model_sar)
###################################################### END CODE ########################################################
