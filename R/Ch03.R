#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slides 113, 115, 119, 122, 127, 129, 136
pulse=read312("Pulse.csv")
j=which(colnames(pulse)=="Sex")
colnames(pulse)[j]="Female"
dim(pulse)
head(pulse)
fpulse=lm(Active~Rest+Hgt+Female,data=pulse)
fpulse
summary(fpulse) #Gives R^2 and F stat for Slide 115


#Slide 115, 127, 129 (gives correct error and total rows)
anova(fpulse)


#Slide 116
cor(pulse[,c("Active","Rest","Female","Hgt")]) |> round(4)


#Slide 121
par(mfrow=c(1,3))
with(pulse,plot(Rest,Active,pch=16))
abline(lm(Active~Rest,data=pulse),col="red")
with(pulse,plot(Hgt,Active,pch=16))
abline(lm(Active~Hgt,data=pulse),col="red")
with(pulse,plot(Female,Active,pch=16))
abline(lm(Active~Female,data=pulse),col="red")
summary(lm(Active~Hgt,data=pulse))


#Slide 139
newdata=data.frame(Rest=70,Female=1,Hgt=66)
predict(fpulse,newdata) |> round(2) #yhat
predict(fpulse,newdata,interval="confidence")[,2:3] |> round(2) #CI
predict(fpulse,newdata,interval="prediction")[,2:3] |> round(2) #PI


#Slide 141
par(mfrow=c(1,1))
pulse$Sex=ifelse(pulse$Female==1,"Female","Male")
boxplot(Active~Sex,data=pulse)


#Slides 143, 145, 149, 
t.test(Active~Sex,data=pulse,var.equal=TRUE)
summary(lm(Active~Female,data=pulse))


#Slides 146, 147, 149
par(mfrow=c(1,1))
fpulse=lm(Active~Rest+Female,data=pulse)
summary(fpulse)
with(pulse,plot(Rest,Active,col=c("blue","red")[Female+1],pch=c("m","f")[Female+1]))
newdata=data.frame(Rest=c(40,110),Female=0)
lines(c(40,110),predict(fpulse,newdata),col="blue",lty=1)
newdata=data.frame(Rest=c(40,110),Female=1)
lines(c(40,110),predict(fpulse,newdata),col="red",lty=2)
legend("topleft",
       legend=c("Male", "Female", expression(Female~hat(y)), expression(Male~hat(y))),
       col=c("blue", "red", "red", "blue"),
       pch=c("m","f", NA, NA),lty=c(NA, NA, 2:1),lwd=c(NA, NA, 2, 2))
       

#Slides 150 and 151
par(mfrow=c(1,2))
plot(fitted(fpulse),resid(fpulse),xlab="Active Predicted",ylab="Active Residual")
abline(h=0,lty=3,col="gray")
qqnorm(resid(fpulse),ylab="Active Residual")
confint(lm(Active~Rest,data=pulse)) |> round(3)


#Slides 152 and 154
par(mfrow=c(1,1))
fpulse=lm(Active~Rest+I(Rest*Female),data=pulse)
with(pulse,plot(Rest,Active,col=c("blue","red")[Female+1],pch=c("m","f")[Female+1],
  xlim=c(0,110),ylim=c(0,160)))
newdata=data.frame(Rest=c(0,110),Female=0)
lines(c(0,110),predict(fpulse,newdata),col="blue",lty=1)
newdata=data.frame(Rest=c(0,110),Female=1)
lines(c(0,110),predict(fpulse,newdata),col="red",lty=2)
legend("topleft",
       legend=c("Male", "Female", expression(Female~hat(y)), expression(Male~hat(y))),
       col=c("blue", "red", "red", "blue"),
       pch=c("m","f", NA, NA),lty=c(NA, NA, 2:1),lwd=c(NA, NA, 2, 2))
summary(fpulse)


#Slides 155 and 157
par(mfrow=c(1,1))
fpulse=lm(Active~Rest*Female,pulse)
with(pulse,plot(Rest,Active,col=c("blue","red")[Female+1],pch=c("m","f")[Female+1]))
newdata=data.frame(Rest=c(40,110),Female=0)
lines(c(40,110),predict(fpulse,newdata),col="blue",lty=2)
newdata=data.frame(Rest=c(40,110),Female=1)
lines(c(40,110),predict(fpulse,newdata),col="red")
legend("topleft",
       legend=c("Male", "Female", expression(Female~hat(y)), expression(Male~hat(y))),
       col=c("blue", "red", "red", "blue"),
       pch=c("m","f", NA, NA),lty=c(NA, NA, 1, 2),lwd=c(NA, NA, 2, 2))
summary(fpulse)


#Slide 160
supekar=read312("Supekar.csv")
dim(supekar)
head(supekar)
fsupekar=lm(Cut.Rate~Day+KEL+I(KEL*Day),data=supekar)
with(supekar,plot(Day,Cut.Rate,col=c("blue","red")[KEL+1],pch=c("s","k")[KEL+1]))
x=c(0,70)
newdata=data.frame(Day=x,KEL=0)
lines(x,predict(fsupekar,newdata),col="blue",lty=2)
newdata=data.frame(Day=x,KEL=1)
lines(x,predict(fsupekar,newdata),col="red")
fsupekar2=lm(Cut.Rate~offset(Day*sum(coef(fsupekar)[c(2,4)]))+KEL,data=supekar)
newdata=data.frame(Day=x,KEL=0)
lines(x,predict(fsupekar2,newdata))
legend("topright",
       legend=c("KEL", "SSS", expression(KEL~hat(y)), expression(SSS~hat(y)), 
       expression(parallel~SSS~hat(y))),
       col=c("red", "blue", "red", "blue","black"),
       pch=c("k","s",NA,NA,NA),lty=c(NA,NA,1,2,1),lwd=c(NA,NA,2,2,2))
summary(fsupekar)


#Slide 165
perch=read312("Perch.csv")
dim(perch)
head(perch)
fperch=lm(Weight~Length+Width+I(Length*Width),data=perch)
summary(fperch)
plot(fitted(fperch),resid(fperch),xlab="Weight Predicted",ylab="Weight Residual")
abline(h=0,lty=3,col="gray",lwd=2)


#Slide 166
#install.packages("usmap") # Must to 1-time install
#install.packages("ggplot2")
library(usmap)
library(ggplot2)
sat=read312("StateSAT82.csv")
sat$state=tolower(state.name[rank(sat$State)])
sat[,c("State","state")] #Add spaces and lower case
dim(sat)
head(sat)
plot_usmap(data=sat,regions="states",values="SAT")+
scale_fill_continuous(low="blue",high="red",name = "SAT") + labs(title = "State colored by SAT")


#Slide 168
par(mfrow=c(1,2))
head(sat)
fsat=lm(SAT~Takers,data=sat)
with(sat,plot(Takers,SAT,pch=16))
abline(fsat,col="red",lwd=3)
plot(fitted(fsat),resid(fsat),pch=16,xlab="SAT Predicted",ylab="SAT Residual")
abline(h=0,lty=3,col="gray",lwd=2)


#Slide 171
(xbar=mean(sat$Takers))
fsat2=lm(SAT~Takers+I((Takers-xbar)^2),data=sat)
summary(fsat2)
with(sat,plot(Takers,SAT,pch=16))
newdata=data.frame(Takers=seq(0,70,length=1000))
lines(newdata[,1],predict(fsat2,newdata),col="red",lwd=3)
plot(fitted(fsat2),resid(fsat2),pch=16,xlab="SAT Predicted",ylab="SAT Residual")
abline(h=0,lty=3,col="gray",lwd=2)


#Slide 174 and 185
fsat3=lm(SAT~Takers+Expend+I(Takers^2)+I(Expend^2)+I(Takers*Expend),data=sat)
summary(fsat3)
par(mfrow=c(1,1))
plot(fitted(fsat3),resid(fsat3),pch=16,xlab="SAT Predicted",ylab="SAT Residual")
abline(h=0,lty=3,col="gray",lwd=2)


#Slide 179
grades=read312("MidtermFinalA.csv")
dim(grades)
head(grades)
cor(grades[,c("Final","Midterm","Quiz","Class")]) |> round(4)
pairs(grades[,c("Final","Midterm","Quiz","Class")],col="gray",pch=16)


#Slide 181
vif=function(fit) {
  mm=model.matrix(fit)[, -1, drop = FALSE]   # drop intercept
  vifs=numeric(ncol(mm))
  names(vifs)=colnames(mm)
  for (i in seq_along(vifs)) {
    xi=mm[, i]
    xrest=mm[, -i, drop = FALSE]
    r2=summary(lm(xi ~ xrest))$r.squared
    vifs[i]=1/(1-r2)
  }
  vifs
}
fgrades=lm(Final~Midterm+Quiz+Class,data=grades)
summary(fgrades)
vif(fgrades)


#Slides 191-193
fpulseMLR=lm(Active~Rest+Female+I(Rest*Female),data=pulse)
fpulseSLR=lm(Active~Rest,data=pulse)
anova(fpulseSLR,fpulseMLR)
