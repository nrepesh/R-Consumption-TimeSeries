#------------------------------------------------------
# Part 1 Importing data & packages

install.packages("TTR")
library("TTR")
dset <- read.csv("energy.csv", header = TRUE); View(dset)
head(dset)

#--------------------------------------------------------------------------------------------

# Part 3 Creating Time Serries

# For electricity
electimeseries <- ts(dset$Elec, frequency = 12, start=c(2015,1))
plot.ts(electimeseries)

decompElec <- decompose(electimeseries)
decompElec$trend
decompElec$seasonal
decompElec$random
plot(decompElec)

#For Gas
gastimeseries <- ts(dset$Gas, frequency = 12, start=c(2015,1))
plot.ts(gastimeseries)

decompGas <- decompose(gastimeseries)
decompGas$trend
decompGas$seasonal
decompGas$random
plot(decompGas)

#--------------------------------------------------------------------------------------------

# Linear Regression

month<- factor(rep(c(1,2,3,4,5,6,7,8,9,10,11,12),4))
times <- c(1:48)

# For electricity
elecmodel <- lm(decompElec$trend~times); summary(elecmodel)
plot(times, decompElec$trend)


# For gas
gasmodel <- lm(decompGas$trend~times); summary(gasmodel)
plot(times, decompGas$trend)


# For prediction
elecpredict <- lm(dset$Elec~times+month); summary(elecpredict)

gaspredict <- lm(dset$Gas~times+month); summary(gaspredict)


#--------------------------------------------------------------------------------------------

# Part 4 Random components 

Relec <- elecpredict$residuals
hist(Relec)
plot(times,Relec,type = 'n')
lines(times,Relec,type='o')

Rgas <- gaspredict$residuals
hist(Rgas)
plot(times,Rgas,type = 'n')
lines(times,Rgas,type='o')

#--------------------------------------------------------------------------------------------
# White Noise Test 

elecresu <- decompElec$random[1:48]
elecfirst<- elecresu[1:47]
elecsecond<- elecresu[2:48]
elecresidsmodel<-lm(elecfirst~elecsecond);summary(elecresidsmodel)
plot(elecfirst,elecsecond)



gasresu <- decompGas$random[1:48]
gasfirst<- gasresu[1:47]
gassecond<- gasresu[2:48]
gasresidsmodel<-lm(gasfirst~gassecond); summary(gasresidsmodel)
plot(gasfirst,gassecond)

#--------------------------------------------------------------------------------------------























