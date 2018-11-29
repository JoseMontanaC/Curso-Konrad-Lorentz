#Analysis of the data set scallops.....

scallops <- read.table("C:/RONNY/Spatial_Stat_RV_Sourse/datasets/scallops.txt", header=T)
scallops<-read.table("/Users/ronnyvallejos/Documents/scallops.txt",header=T)

hist(scallops$tcatch)
myscallops <- scallops
myscallops[,"lgcatch"] <- log(scallops$tcatch+1)
summary(myscallops$lgcatch)
hist(myscallops$lgcatch)
plot(myscallops$long, myscallops$lat)

library(akima)

#interpolating and transforming the data

int.scp<-interp.new(myscallops$long, myscallops$lat,myscallops$lgcatch)
image(int.scp, ylab="Latitude",xlab="Longitude",main="Interpolation of Scallops")
contour(int.scp,main="Countour Diagram for Scallops")

#transforming scallops to a geodata object to be process in GeoR.Computation of classical and robust variograms

library(geoR)
R <- 6371
lat <- myscallops$lat*pi/180
lon <- myscallops$long*pi/180
u <- cbind(R*cos(lat)*cos(lon), R*cos(lat)*sin(lon), R*sin(lat))
obj <- cbind(myscallops$long,myscallops$lat,myscallops$tcatch)
#obj <- cbind(u,myscallops$lgcatch)
scallops.geo <- as.geodata(obj,coords.col=1:2,data.col=3)
scallops.var <- variog(scallops.geo, estimator.type="classical")
scallops.var.robust <- variog(scallops.geo, estimator.type="modulus")

#plotting the variograms

par(mfrow=c(2,1))
plot(scallops.var, xlab="Variograma clasico")
plot(scallops.var.robust, xlab="Variograma Robusto")

#plotting the rheoretical and estimated models

scallops.var.fit <- variofit(scallops.var,ini.cov.pars=c( 3.2484, 16.4108 ),cov.model="exponential", fix.nugget=FALSE,nugget=1.7)
plot(scallops.var, xlab="Variograma Clasico y Exponencial")
lines.variomodel(cov.model = "exp", cov.pars = c( 3.2484, 16.4108), nug = 1.7, max.dist = 200) 

#fitting spherical variogram

scallops.var.fit <- variofit(scallops.var,ini.cov.pars=c( 3.2484, 16.4108 ),cov.model="spherical", fix.nugget=FALSE,nugget=1.7)
plot(scallops.var, xlab="Variograma Clasico y Esferico")
lines.variomodel(cov.model = "spherical", cov.pars = c( 3.2484, 16.4108), nug = 1.7, max.dist = 200) 

#fitting wave variogram

scallops.var.fit <- variofit(scallops.var,ini.cov.pars=c( 3.2484, 16.4108 ),cov.model="wave", fix.nugget=FALSE,nugget=1.7)
plot(scallops.var, xlab="Variograma Clasico y Wave")
lines.variomodel(cov.model = "wave", cov.pars = c( 3.2484, 16.4108), nug = 1.7, max.dist = 200) 




#predicting in new locations

#defininf the new locations

loci <- matrix(c( 0.71 ,0.72,  -1.24, -1.25), ncol = 2) 
kc <- krige.conv(scallops.geo, loc=loci, krige=krige.control(cov.pars=c( 1.5529, 18.0421 ),nugget=3.4047 ))

#performing universal kriging

#universal kriging with first order polinomyal in the coordinates
kcU <- krige.conv(scallops.geo, loc=loci, krige=krige.control(type.krige = "OK",cov.pars=c( 1.5529, 18.0421 ),nugget=3.4047,trend.d="1st", trend.l="1st" ))


######################################
# Kriging noonrectamgular areas
######################################
summ=summary(scallops.geo)

linf=seq(summ$coor[1,1],summ$coor[2,1],l=101)
lsup=seq(summ$coor[1,2],summ$coor[2,2],l=101)

pred.grid<-expand.grid(linf,lsup)

vertices=rbind(c(-73.5,38.5),c(-73,39),c(-72.5,39.5),c(-71.5,40.3),c(-71.8,40.9),c(-73,40),c(-73.5,39.2),c(-73.4,38.8))

polygon(vertices,angle=30,density=20)

krig=krige.control(trend.d = "1st", trend.l = "1st", cov.pars=c(3.2,16.4),nugget=1.7, cov.model="wave")
kcver<-krige.conv(scallops.geo, loc=pred.grid, krige=krig,borders=vertices)

#kriging estimates
image(kcver, loc = pred.grid, xlab="Coord X", ylab="Coord Y", main="Kriging estimates")

#stansard error
image(kcver, loc = pred.grid, xlab="Coord X", ylab="Coord Y", val=sqrt(kcver$krige.var), main="Kriging std. errors")







