#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slide 198
nyc=read312("HousesNY.csv")
dim(nyc)
head(nyc)
cor(nyc[,c("Price","Beds","Size")]) |> round(4)
pairs(nyc[,c("Price","Beds","Size")],col="gray",pch=16)


#Slide 199
fnyc=lm(Price~Beds+Size,data=nyc)
fnyc
anova(fnyc)
summary(fnyc)


#Slides 201, 202
prb=resid(lm(Price~Beds,nyc))
srb=resid(lm(Size~Beds,nyc))
lm(prb~srb) #3 SLRs produce Size MLR slope from Slide 199
coef(fnyc)["Size"]


#Slide 203
pulse=read312("Pulse.csv")
j=which(colnames(pulse)=="Sex")
colnames(pulse)[j]="Female"
dim(pulse)
head(pulse)
cor(pulse[,c("Active","Rest","Female","Hgt")]) |> round(4)
pairs(pulse[,c("Active","Rest","Female","Hgt")],col="gray",pch=16)


#Slides 204, 205
arrf=resid(lm(Active~Rest+Female,pulse)) #MLR 2 predictors
hrrf=resid(lm(Hgt~Rest+Female,pulse)) #MLR 2 predictors
lm(arrf~hrrf) #SLR produces the same MLR 3 predictor slope for Hgt
coef(lm(Active~Rest+Female+Hgt,pulse))["Hgt"] #MLR 3 predictors


#Slide 211
#install.packages("leaps") #1-time install
library(leaps)
birds=read312("BlueJays2.csv")
j=which(colnames(birds)=="Sex")
colnames(birds)[j]="Male"
dim(birds)
head(birds)
fbirds=regsubsets(Mass~Depth+Width+Length+Skull+Male,birds,nbest=10,nvmax=5)
temp=summary(fbirds)
RMSE=sqrt(temp$rss/(nrow(birds)-rowSums(temp$which))) |> round(4)
cbind(temp$which,R2=round(temp$rsq,4),RMSE=RMSE)
fbirds=regsubsets(Mass~Depth+Width+Length+Skull+Male,birds,nbest=3,nvmax=5)
temp=summary(fbirds)
RMSE=sqrt(temp$rss/(nrow(birds)-rowSums(temp$which))) |> round(4)
cbind(temp$which,R2=round(temp$rsq,4),RMSE=RMSE)


#Slide 215
kids=read312("KidsTrainA.csv")
kids$lWt=log(kids$Wt)
dim(kids)
head(kids)
fkids=lm(lWt~Ht+Age+Sex,kids)
summary(fkids)


#Slide 216
kidsHO=read312("KidsHoldoutA.csv") #HO=Hold Out
kidsHO$lWt=log(kidsHO$Wt)
dim(kidsHO)
head(kidsHO)
yhat=predict(fkids,kidsHO) #Step 1
e=kidsHO$lWt-yhat #Step 2
mean(e) |> round(4) #Step 3
sd(e) |> round(4)
cor(kidsHO$lWt,yhat) |> round(4) #Step 4
cor(kidsHO$lWt,yhat)^2 |> round(4) #Step 5


#Slide 226
bush=read312("PalmBeach.csv")
dim(bush)
head(bush)
par(mfrow=c(1,2))
with(bush,plot(Bush,Buchanan,pch=16))
fbush=lm(Buchanan~Bush,bush)
abline(fbush,col="red",lwd=3)
i=which.max(bush$Buchanan)
text(rbind(bush[i,3:2]),"Palm Beach",pos=1)
i=which.max(bush$Bush)
text(rbind(bush[i,3:2]),"Dade",pos=2)
stripchart(round(hatvalues(fbush),3),method="stack",pch=16,xlab="h Buchanan",at=0)


#Slide 227
par(mfrow=c(1,1))
plot(fitted(fbush),rstudent(fbush),xlab="Predicted Buchanan",
 ylab="Externally Studentized Residual Buchanan",pch=16)
abline(h=c(-3,-2,2,3),lty=3,col="gray",lwd=2)
axis(4,c(-3,-2,2,3),las=2,cex.axis=.8)


#Slide 228
par(mfrow=c(1,1))
stripchart(round(cooks.distance(fbush),2),method="stack",pch=16,xlab="Cook's D Buchanan",at=0)


#Slide 229
par(mfrow=c(1,1))
with(bush,plot(Bush,Buchanan,pch=16))
abline(fbush,col="blue",lwd=3)
i=which.max(bush$Buchanan)
text(rbind(bush[i,3:2]),"Palm Beach",pos=1)
abline(lm(Buchanan~Bush,bush[-i,]),col="green",lwd=3,lty=2)
i=which.max(bush$Bush)
text(rbind(bush[i,3:2]),"Dade",pos=2)
abline(lm(Buchanan~Bush,bush[-i,]),col="gray",lwd=3,lty=3)


#Slide 230
sat=read312("StateSAT82.csv")
dim(sat)
head(sat)
fsat=lm(SAT~Takers+Expend,sat)
summary(fsat)


#Slide 231
par(mfrow=c(1,2))
with(sat,plot(Expend,Takers,pch=16))
i=which.max(sat$Expend)
sat[i,] #Check if this outlier is Alaska
text(rbind(sat[i,c("Expend","Takers")]),"Alaska",pos=2)
stripchart(round(hatvalues(fsat),2),method="stack",pch=16,xlab="h SAT",at=0)
text(hatvalues(fsat)[i],0,"Alaska",pos=2)


#Slide 232
par(mfrow=c(1,2))
plot(fitted(fsat),rstudent(fsat),xlab="Predicted SAT",
 ylab="Externally Studentized Residual SAT",pch=16,ylim=c(-3,3))
abline(h=c(-3,-2,2,3),lty=3,col="gray",lwd=2)
i=order(rstudent(fsat))
sat[i[1:2],]
text(fitted(fsat)[i[1]],rstudent(fsat)[i[1]],"AK",pos=3)
text(fitted(fsat)[i[2]],rstudent(fsat)[i[2]],"SC",pos=2)
stripchart(round(cooks.distance(fsat),2),method="stack",pch=16,xlab="Cook's D SAT",at=0)
text(cooks.distance(fsat)[i[1]],0,"AK",pos=2)
text(cooks.distance(fsat)[i[2]],0,"SC",pos=4)
sat$yhat=fitted(fsat) |> round(3)
sat$e=resid(fsat) |> round(3)
sat$ESR=rstudent(fsat) |> round(4)
sat$h=hatvalues(fsat) |> round(4)
sat$D=cooks.distance(fsat) |> round(2)
sat[i[1:2],]


#Slide 239
gpa=read312("GPAbyMajor.csv")
dim(gpa)
head(gpa)
par(mfrow=c(1,1))
i=apply(gpa[,c("HU","SS","NS")],1,function(i) sum(i*(1:3)))
cols=c("blue","green","red")[i]
pchs=(1:3)[i]
with(gpa,plot(SATM,GPA,col=cols,pch=pchs))
sr=c(300,900)
fgpa=lm(GPA~SATM+SS+NS,gpa)
newdata=data.frame(SATM=sr,SS=0,NS=0)
lines(sr,predict(fgpa,newdata),col="blue",lty=2,lwd=2)
newdata=data.frame(SATM=sr,SS=1,NS=0)
lines(sr,predict(fgpa,newdata),col="green",lty=2,lwd=2)
newdata=data.frame(SATM=sr,SS=0,NS=1)
lines(sr,predict(fgpa,newdata),col="red",lty=2,lwd=2)
legend("topleft",
       legend=c("HU", "SS", "NS", expression(HU~hat(y)), expression(SS~hat(y)), expression(NS~hat(y))),
       col=rep(c("blue", "green", "red"),2),
       pch=c(1:3,rep(NA,3)),lty=c(rep(NA,3), rep(2,3)),lwd=c(rep(NA,3),rep(2,3)))


#Slide 240
fgpaFULL=lm(GPA~SATM+SS+NS,gpa)
fgpaREDUCED=lm(GPA~SATM,gpa)
summary(fgpaFULL)
anova(fgpaREDUCED,fgpaFULL)


#Slide 242
fgpa=lm(GPA~SATM*(SS + NS),gpa)
par(mfrow=c(1,1))
with(gpa,plot(SATM,GPA,col=cols,pch=pchs))
sr=c(300,900)
newdata=data.frame(SATM=sr,SS=0,NS=0)
lines(sr,predict(fgpa,newdata),col="blue",lty=2,lwd=2)
newdata=data.frame(SATM=sr,SS=1,NS=0)
lines(sr,predict(fgpa,newdata),col="green",lty=2,lwd=2)
newdata=data.frame(SATM=sr,SS=0,NS=1)
lines(sr,predict(fgpa,newdata),col="red",lty=2,lwd=2)
legend("topleft",
       legend=c("HU", "SS", "NS", expression(HU~hat(y)), expression(SS~hat(y)), expression(NS~hat(y))),
       col=rep(c("blue", "green", "red"),2),
       pch=c(1:3,rep(NA,3)),lty=c(rep(NA,3), rep(2,3)),lwd=c(rep(NA,3),rep(2,3)))


#Slide 243
anova(lm(GPA~SATM+SS+NS,gpa),lm(GPA~SATM+SS+NS+I(SS*SATM)+I(NS*SATM),gpa))


#Slides 246-250
sat=read312("SATGPA.csv")
dim(sat)
head(sat) #Slide 246 Picture and Table
fsat=lm(GPA~VerbalSAT,sat)
with(sat,plot(VerbalSAT,GPA,pch=16))
abline(fsat,col="red",lwd=2)
summary(fsat)
(b1=coef(fsat)[2]) #Slope


#Slides 246-250 (cont)
b1s=sapply(1:10^3, function(i) coef(lm(sat$GPA[sample(1:nrow(sat))]~sat$VerbalSAT))[2])
hist(b1s,xlab=expression(hat(beta)[1]), #Slide 249
     main = expression("Permutation Distribution of " * hat(beta)[1]))
abline(v=b1,col="red",lwd=3)
mean(abs(b1s)>abs(b1)) #p-value (approx Slide 249)

#Slides 252-257
perch=read312("Perch.csv")
dim(perch)
head(perch) #Slide 252 Picture and Table
fperch=lm(Length~Width,perch)
with(perch,plot(Width,Length,pch=16))
abline(fperch,col="red",lwd=2)
summary(fperch)
(b1=coef(fperch)[2]) #Slope


#Slides 252-257 (cont)
b1s=sapply(1:10^3, function(i) coef(lm(Length~Width,perch[sample(1:nrow(perch),replace=TRUE),]))[2])
hist(b1s,xlab=expression(hat(beta)[1]), #Slide 252
     main = expression("Bootstrap Distribution of " * hat(beta)[1]))
quantile(b1s,probs=c(.025,.975)) |> round(2) #95% Bootstrap CI for true slope (approx Slide 257)
