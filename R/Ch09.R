#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slide 539
pulse=read312("Pulse.csv")
dim(pulse)
head(pulse)
colnames(pulse)[4]="Female"
(slr=lm(Female~Hgt,pulse)) #SLR is bad here


#Slide 540
jit=runif(nrow(pulse),-.1,.1)
with(pulse,plot(Hgt,Female+jit,ylab="Female"))
abline(h=0:1,lty=3,lwd=2)
abline(slr,col="blue",lwd=3)


#Slide 544
glm(Female~Hgt,binomial,data=pulse)


#Slides 545, 588
putt2=read312("Putts2.csv") #Distance-level data
dim(putt2)
head(putt2)
glm(cbind(Made,Missed)~Length,binomial,data=putt2)


#Slides 546, 588
putt1=read312("Putts1.csv") #Putt-level data
dim(putt1)
head(putt1)
(fputt=glm(Made~Length,binomial,data=putt1)) #Same as Putt2


#Slides 547, 548, 568
Length=putt2$Length
phat=putt2$Made/putt2$Trials
newdata=data.frame(Length=putt2$Length)
piehat=predict(fputt,newdata,type="response")
rbind(phat,piehat) |> round(3)
plot(Length,log(phat/(1-phat)))
abline(fputt,col="blue",lwd=3)


#Slide 549
plot(Length,phat,ylim=0:1)
abline(h=0:1,lty=3,lwd=2)
grid=seq(0,9,length=1000)
newdata=data.frame(Length=grid)
lines(grid,predict(fputt,newdata,type="response"),col="blue",lwd=3)


#Slides 552, 553
rbind(phat/(1-phat),piehat/(1-piehat)) |> round(2)
log(rbind(phat/(1-phat),piehat/(1-piehat))) |> round(3)


#Slide 557
(ache=read312("Migraines.csv"))
(odds=with(ache,Yes/No)) #Odds
odds[1]/odds[2] #Odds ratio


#Slide 570
groups=cut(pulse$Hgt,breaks=c(-Inf,62,65,68,72,Inf)+.5,labels=1:5,right=TRUE)
tab=cbind(
Group=1:5,
MinHgt=with(pulse,tapply(Hgt,groups,min)),
MeanHgt=with(pulse,tapply(Hgt,groups,mean)),
MaxHgt=with(pulse,tapply(Hgt,groups,max)),
N=with(pulse,tapply(Female,groups,length)),
Females=with(pulse,tapply(Female,groups,sum)))
tab=data.frame(tab)
tab$Males=with(tab,N-Females)
tab$AdjProp=with(tab,(Females+.5)/(N+1))
tab$logitAdjProp=log(tab$AdjProp/(1-tab$AdjProp))
tab
with(tab,plot(MeanHgt,logitAdjProp,ylim=c(-4,4)))
abline(glm(cbind(Females,Males)~MeanHgt,binomial,data=tab),lwd=3,col="blue")


#Slides 576, 582, 584
confint.default(fputt,"Length")|>round(3) #Slope CI
confint.default(fputt,"Length")|>exp()|>round(3) #Odd ratio CI
logLik(fputt) #Slide 582
anova(fputt, test="Chisq") #Slide 584
