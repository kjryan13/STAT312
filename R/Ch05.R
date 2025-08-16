#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slide 260
pulse=read312("Pulse.csv")
dim(pulse)
head(pulse)
colnames(pulse)[4]="Female" #Recode
pulse$Sex=ifelse(pulse$Female==1,"Female","Male")
head(pulse)
jit=runif(nrow(pulse),-.1,.1)
with(pulse,plot(1-Female+jit,Active,xaxt="n",xlab="Sex"))
axis(1,0:1,c("Female","Male"))
t.test(Active~Sex,pulse)


#Slide 261
rat=read312("FatRats.csv")
dim(rat)
head(rat)
par(mfrow=c(3,1))
f=function(i){rat$Gain[rat$Source==i & rat$Protein=="Hi"]}
g=function(i){hist(f(i),main=paste(i,"with hi protein"),xlab="Gain",xlim=range(rat$Gain))}
catch=sapply(c("Beef","Cereal","Pork"),g) #Plots
h=function(i){c(N=length(i),Mean=mean(i),StDev=sd(i)|>round(2))}
tab=with(rat[rat$Protein=="Hi",],tapply(Gain,Source,h))
do.call(rbind,tab)


#Slide 262
par(mfrow=c(1,1))
boxplot(Gain~Source,rat[rat$Protein=="Hi",])


#Slide 263
leaf=read312("Leafhoppers.csv")
dim(leaf)
head(leaf)
h=function(i){c(N=length(i),Mean=mean(i),StDev=sd(i)|>round(4))}
tab=with(leaf,tapply(Days,Diet,h))
do.call(rbind,tab)


#Slide 264
(dns=sort(unique(leaf$Diet)))
leaf$dn=match(leaf$Diet,dns) #Diet number
head(leaf)
with(leaf,plot(dn,Days,xlab="Diet",xaxt="n"))
axis(1,1:4,dns)


#Slide 266
teen=read312("TeenPregnancy.csv")
dim(teen)
head(teen)
h=function(i){c(N=length(i),Mean=mean(i)|>round(3),StDev=sd(i)|>round(3))}
tab=with(teen,tapply(Teen,CivilWar,h))
do.call(rbind,tab)


#Slide 267
#install.packages("usmap") # Must to 1-time install
#install.packages("ggplot2")
library(usmap)
library(ggplot2)
teen$state=teen$State #expects abbr for state abbreviations
plot_usmap(data=teen,regions="states",values="Teen")+
scale_fill_continuous(low="blue",high="red",name = "Teen")+ 
labs(title = "State colored by Teen")


#Slide 282, 285
exams=read312("FourExams.csv")
exams$Exam=factor(exams$Exam) #Treat Exam as qualitative (NOT SLR)
dim(exams)
head(exams)
fexam=lm(Grade~Exam,exams)
anova(fexam)


#Slide 292
par(mfrow=c(1,2))
plot(fitted(fexam),resid(fexam),xlab="Grade Predicted",ylab="Grade Residual")
abline(h=0,lty=2,col="gray")
(means=aggregate(Grade~Exam,mean,data=exams))
abline(v=means$Grade,col="gray")
axis(1,means$Grade,paste0("E",means$Exam))
qqnorm(resid(fexam),ylab="Exam Residual")


#Slides 299, 314-316
f=function(i){c(N=length(i),Mean=mean(i)|>round(2),StdDev=sd(i)|>round(2))}
(sumstat=do.call(rbind,with(exams,tapply(Grade,Student,f)))|>data.frame())
with(sumstat,plot(log(Mean),log(StdDev),pch=16))
fit=lm(log(StdDev)~log(Mean),sumstat)
abline(fit,col="red",lwd=3)
text(4,1.8,paste("Slope is",coef(fit)[2]|>round(2)))
do.call(rbind,with(exams,tapply(Grade^2,Student,f)))|>data.frame()


#Slides 300, 301, 318, 323
exams$Student=factor(exams$Student)
fexam=lm(Grade~Student,exams)
anova(fexam)
par(mfrow=c(1,2))
plot(fitted(fexam),resid(fexam),xlab="Grade Predicted",ylab="Grade Residual")
abline(h=0,lty=2,col="gray")
abline(v=sumstat$Mean,col="gray")
axis(3,sumstat$Mean,rownames(sumstat))
qqnorm(resid(fexam),ylab="Exam Residual")


#Slides 304, 324
#install.packages("emmeans") #1-time install
#install.packages("multcomp") #1-time install
library(emmeans)
library(multcomp)   
emm=emmeans(fexam, "Student")
summary(emm, infer = c(TRUE, TRUE))  
pairs(emm, adjust="none", infer=rep(TRUE,2))
g=glht(fexam,linfct=mcp(Student="Tukey"))
cld(g)


#Slides 306, 307
box=read312("Boxes.csv")
dim(box)
head(box)
(sumstat=do.call(rbind,with(box,tapply(Area,Width,f)))|>data.frame())
par(mfrow=c(1,1))
boxplot(Area~Width,box,at=sort(unique(box$Width)))


#Slide 309
fit=lm(log(StdDev)~log(Mean),sumstat)
with(sumstat,plot(log(Mean),log(StdDev),pch=16))
abline(fit,col="red",lwd=3)
text(3,3.5,paste("Slope is",coef(fit)[2]|>round(3)))


#Slides 310, 311
box$lnArea=log(box$Area) #log() means ln() in the STAT world
head(box)
(sumstat=do.call(rbind,with(box,tapply(lnArea,Width,f)))|>data.frame())
par(mfrow=c(1,1))
boxplot(lnArea~Width,box,at=sort(unique(box$Width)))


#Slides 327, 328
wwf=read312("WordsWithFriends.csv")
wwf$Blanks=factor(wwf$Blanks)
dim(wwf)
head(wwf)
(sumstat=do.call(rbind,with(wwf,tapply(Points,Blanks,f)))|>data.frame())
boxplot(Points~Blanks,wwf)


#Slides 329-331
fwwf=lm(Points~Blanks,wwf)
qqnorm(resid(fwwf))
anova(fwwf)
emm=emmeans(fwwf, "Blanks")
g=glht(fwwf,linfct=mcp(Blanks="Tukey"))
pairs(emm, adjust="none", infer=rep(TRUE,2))
summary(emm, infer = c(TRUE, TRUE))
cld(g) #Shared letter = no sig diff
