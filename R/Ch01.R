#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slide 35
grades=read312("MidtermFinal.csv")
dim(grades)
head(grades)
(ybar=mean(grades$Final))
with(grades,plot(Midterm,Final))
abline(h=ybar)
axis(4,ybar,expression(bar(y)),las=2)


#Slide 36
with(grades,plot(Midterm,Final))
fgrades=lm(Final~Midterm,data=grades)
abline(fgrades,col="blue")


#Slide 42
fgrades
summary(fgrades)


#Slide 43
anova(fgrades)


#Slide 45
species=read312("SpeciesArea.csv")
dim(species)
head(species)
par(mfrow=c(1,2))
with(grades,plot(Midterm,Final))
abline(fgrades,col="blue")
with(species,plot(Area,Species))
fspecies=lm(Species~Area,data=species)
abline(fspecies,col="blue")


#Slide 46
par(mfrow=c(1,2))
plot(fitted(fgrades),resid(fgrades),xlab="Final Predicted",ylab="Final Residual")
abline(h=0,lty=2,col="gray")
plot(fitted(fspecies),resid(fspecies),xlab="Species Predicted",ylab="Species Residual")
abline(h=0,lty=2,col="gray")


#Slide 47
health=read312("MetroHealth83.csv")
dim(health)
head(health)
par(mfrow=c(1,2))
with(grades,plot(Midterm,Final))
abline(fgrades,col="blue")
with(health,plot(NumHospitals,NumMDs))
fhealth=lm(NumMDs~NumHospitals,data=health)
abline(fhealth,col="blue")


#Slide 48
par(mfrow=c(1,2))
plot(fitted(fgrades),resid(fgrades),xlab="Final Predicted",ylab="Final Residual")
abline(h=0,lty=2,col="gray")
plot(fitted(fhealth),resid(fhealth),xlab="NumMDs Predicted",ylab="NumMDs Residual")
abline(h=0,lty=2,col="gray")


#Slide 49
par(mfrow=c(1,2))
hist(resid(fgrades),xlab="Final Residual")
hist(resid(fhealth),xlab="NumMDs Residual")


#Slide 51
par(mfrow=c(1,2))
qqnorm(resid(fgrades),ylab="Final Residual")
qqnorm(resid(fhealth),ylab="NumMDs Residual")


#Slide 56
par(mfrow=c(1,2))
planets=read312("Planets.csv")
planets$LogDistance=log(planets$Distance,10)
planets$LogYear=log(planets$Year,10)
dim(planets)
head(planets)
pairs(planets[,c("Distance","Year","LogDistance","LogYear")], pch=16, col="grey")


#Slide 59
par(mfrow=c(1,1))
with(species,plot(logArea,logSpecies))
fspeciesll=lm(logSpecies~logArea,data=species)
abline(fspeciesll,col="blue")
coef(fspeciesll)
exp(coef(fspeciesll))


#Slide 64
par(mfrow=c(1,1))
gold=read312("LongJumpOlympics2016.csv")
dim(gold)
head(gold)
with(gold,plot(Year,Gold))
i=which.max(gold$Gold)
text(rbind(gold[i,]),"Bob Beamon",pos=1)


#Slide 67
fgold=lm(Gold~Year,data=gold)
plot(fitted(fgold),rstandard(fgold),ylim=c(-3,3),
 xlab="Predicted Gold",ylab="Studentized Resid Gold")
abline(h=c(-3,-2,2,3),lty=2,col="gray")
(bob=cbind(fitted(fgold)[i],rstandard(fgold)[i]))
text(bob,"Bob Beamon",pos=1)


#Slide 69
plot(fitted(fgold),rstudent(fgold),ylim=c(-4,4),
 xlab="Predicted Gold",ylab="Externally Studentized Resid Gold")
abline(h=c(-3,-2,2,3),lty=2,col="gray")
(bob=cbind(fitted(fgold)[i],rstudent(fgold)[i]))
text(bob,"Bob Beamon",pos=1)


#Slide 71
bush=read312("PalmBeach.csv")
dim(bush)
head(bush)
(i=which.max(bush$Buchanan)) #Obs num for Palm Beach
with(bush,plot(Bush,Buchanan))
text(rbind(bush[i,3:2]),"Palm Beach",pos=1)
abline(lm(Buchanan~Bush,data=bush),col="blue")
abline(lm(Buchanan~Bush,data=bush[-i,]),col="green",lty=2)
legend("topright", c("All 67 FL counties", "66 w/o Palm Beach"),
       lty=c(1,2), col=c("blue","green"))
