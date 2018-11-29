# Geostatistical Analysis

# Box-Cox Transformation

min(s100$data) 
# the minimum value is -1.167695
aux=boxcoxfit(s100$data+1.17)  
#this computes the lambda values
plot(x)  
# shows the new histogram of the transform variable


# Computation of the Empirical Variogram

# binned variogram
vario.b <- variog(s100, max.dist=1)
# variogram cloud
vario.c <- variog(s100, max.dist=1, op="cloud")
#binned variogram and stores the cloud
vario.bc <- variog(s100, max.dist=1, bin.cloud=TRUE)
# smoothed variogram
vario.s <- variog(s100, max.dist=1, op="sm", band=0.2)
#
#
# plotting the variograms:
par(mfrow=c(2,2))
plot(vario.b, main="binned variogram") 
plot(vario.c, main="variogram cloud")
plot(vario.bc, bin.cloud=TRUE, main="clouds for binned variogram")  
plot(vario.s, main="smoothed variogram") 

#Fitting Variogram Models

vario100 <- variog(s100, max.dist=1)
ini.vals <- expand.grid(seq(0,1,l=5), seq(0,1,l=5))
ols <- variofit(vario100, ini=ini.vals, fix.nug=TRUE, wei="equal")
summary(ols)
wls <- variofit(vario100, ini=ini.vals, fix.nug=TRUE)
summary(wls)
plot(vario100)
lines(wls)
lines(ols, lty=2)


