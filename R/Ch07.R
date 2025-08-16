#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}
IP=function(y, x, trace, yl, xl, tl, inset=c(.1,0), ...)
{
  k=nlevels(trace)
  interaction.plot(x, trace, y, lty=1:k, col=1, pch=NA, xlab=xl, ylab=yl, legend=FALSE, ...)
  legend("topright", legend=levels(trace), # draw the legend
         lty=1:k, col=1, pch=NA, title=tl,
         inset=inset, xpd=NA, bty="n")
}
f=function(x) c(N=length(x), Mean=mean(x), SD=sd(x))
SUM=function(df,y,f1,f2)
{
tab=tapply(df[,y], list(df[,f1], df[,f2]), f)
m=do.call(rbind, as.vector(tab))  # 6×3 numeric matrix
idx=expand.grid(f1=dimnames(tab)[[1]],f2=dimnames(tab)[[2]])
colnames(idx)=c(f1,f2)
sumstat=cbind(idx, as.data.frame(m))
sumstat$logMean=log(sumstat$Mean)
sumstat$logSD=log(sumstat$SD)
sumstat$GrandMean=mean(sumstat$Mean)
f1Mean=fitted(lm(sumstat$Mean~sumstat[[f1]]))
f2Mean=fitted(lm(sumstat$Mean~sumstat[[f2]]))
sumstat$Residual=with(sumstat,Mean-f1Mean-f2Mean+GrandMean)
sumstat$Comparison=with(sumstat,(f1Mean-GrandMean)*(f2Mean-GrandMean)/GrandMean)
sumstat
}


#Slide 390
(glue=read312("Glue.csv",colClasses=c("factor", "factor", "numeric")))
with(glue,tapply(Force,list(Thickness,Type),mean))


#Slides 392, 409
par(mfrow=c(1,2))
with(glue,IP(Force,Type,Thickness,"Mean Force","Type","Thickness"))
with(glue,IP(Force,Thickness,Type,"Mean Force","Thickness","Type"))


#Slides 393, 394, 411
(bird=read312("BirdCalcium.csv",colClasses=c(rep("factor",4), "numeric")))
with(bird,tapply(Ca,list(Sex,Hormone),mean))
par(mfrow=c(1,1))
with(bird,IP(Ca,Hormone,Sex,"Mean Ca","Hormone","Sex",inset=c(.4,0)))


#Slide 404
par(mfrow=c(1,1))
with(glue,IP(Force,Thickness,Type,"Mean Force","Thickness","Type",ylim=c(40,90)))
points(as.numeric(glue$Thickness),glue$Force,pch=substr(glue$Type,1,1))


#Slides 406, 407
sumglue=SUM(glue,"Force","Type","Thickness")
with(sumglue,plot(logMean,logSD,pch=16))
(slr=lm(logSD~logMean,sumglue))
abline(slr,col="red",lwd=2)


#Slide 433
fglue=lm(Force~Type*Thickness,glue)
anova(fglue)


#Slide 436
dino=read312("Dinosaurs.csv",colClasses=c(rep("factor",3),"numeric"))
dim(dino)
head(dino)
with(dino,tapply(Iridium,list(Source,Depth),mean))
fdino=lm(Iridium~Source*Depth,dino)
anova(fdino)
par(mfrow=c(1,1))
with(dino,IP(Iridium,Depth,Source,"Mean Iridium","Depth","Source"))


#Slide 438
par(mfrow=c(1,2))
plot(fitted(fdino),resid(fdino),xlab="Iridium Predicted",ylab="Iridium Residual")
abline(h=0,lty=3,col="gray",lwd=3)
qqnorm(resid(fdino),ylab="Iridium Residual")


#Slide 439
f=function(x) c(N=length(x), Mean=mean(x), SD=sd(x))
sumstat=SUM(dino,"Iridium","Source","Depth")
par(mfrow=c(1,2))
with(sumstat,plot(Comparison,Residual,asp=1))
(slr=lm(Residual~Comparison,sumstat))
abline(slr,col="red",lwd=3)
with(sumstat,plot(logMean,logSD,asp=1))
(slr=lm(logSD~logMean,sumstat))
abline(slr,col="red",lwd=3)


#Slide 440
dino$rIridium=sqrt(dino$Iridium)
fdino2=lm(rIridium~Source*Depth,dino)
par(mfrow=c(1,2))
plot(fitted(fdino2),resid(fdino2),xlab="root(Iridium) Predicted",ylab="root(Iridium) Residual")
abline(h=0,lty=3,col="gray",lwd=3)
qqnorm(resid(fdino2),ylab="root(Iridium) Residual")


#Slides 441, 442, 449
sumstat2=SUM(dino,"rIridium","Source","Depth")
par(mfrow=c(1,2))
with(sumstat2,plot(Comparison,Residual,asp=1))
(slr=lm(Residual~Comparison,sumstat2))
abline(slr,col="red",lwd=3)
with(sumstat2,plot(logMean,logSD,asp=1))
(slr=lm(logSD~logMean,sumstat2))
abline(slr,col="red",lwd=3)
anova(fdino2) #Y=root(Iridium)
anova(fdino) #Y=Iridium


#Slide 448
with(dino,tapply(rIridium,list(Source,Depth),mean)) |> round(3)


#Slide 451
tapply(dino$rIridium,dino$Source,mean) |> round(3)
tapply(dino$rIridium,dino$Depth,mean) |> round(3)


#Slides 454-457
anova(fglue)
qt(.975,6)
#install.packages("emmeans") #1-time install
#install.packages("multcomp") #1-time install
library(emmeans)
library(multcomp)   
fglue=lm(Force~Type*Thickness,glue)
emm=emmeans(fglue, ~Type*Thickness)
pairs(emm, adjust="none", infer=rep(TRUE,2))
