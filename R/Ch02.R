#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slides 82, 84, 88, 96
grades=read312("MidtermFinal.csv")
dim(grades)
head(grades)
fgrades=lm(Final~Midterm,data=grades)
summary(fgrades)


#Slide 84 CIs
confint(fgrades)


#Slide 89
species=read312("SpeciesArea.csv")
dim(species)
head(species)
with(species,plot(Area,Species,pch=16))
fspecies=lm(Species~Area,data=species)
abline(fspecies,col="red",lwd=2)
summary(fspecies)


#Slide 95
anova(fgrades)


#Slide 102
with(grades,plot(Midterm,Final,pch=16))
abline(fgrades,col="red",lwd=4)
(xr=range(grades$Midterm)) #x range
newdata=data.frame(Midterm=seq(xr[1],xr[2],length=1000))
pc=predict(fgrades,newdata,interval="confidence")
head(pc)
lines(newdata[,1],pc[,2],lty=2,col="red")
lines(newdata[,1],pc[,3],lty=2,col="red")


#Slide 104
with(grades,plot(Midterm,Final,pch=16,ylim=c(30,110)))
abline(fgrades,col="red",lwd=4)
pp=predict(fgrades,newdata,interval="prediction")
head(pp)
lines(newdata[,1],pc[,2],lty=2,col="blue")
lines(newdata[,1],pc[,3],lty=2,col="blue")
lines(newdata[,1],pp[,2],lty=2,col="green")
lines(newdata[,1],pp[,3],lty=2,col="green")
legend("topleft",
  legend=c("Upper PI", "Upper CI", expression(hat(y)),
           "Lower CI", "Lower PI"),
  lty=c(2, 2, 1, 2, 2),lwd=c(1, 1, 2, 1, 1),
  col=c("green", "blue", "red", "blue", "green", "red"))


#Slide 105
newdata=data.frame(Midterm=41) #Forecasts for Midterm=41%
predict(fgrades,newdata) #yhat
predict(fgrades,newdata,interval="confidence")[,2:3] #CI
predict(fgrades,newdata,interval="prediction")[,2:3] #PI
