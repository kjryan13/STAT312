# NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slide 21
dat=read312("DistanceHome.csv")
dat$Male=(dat$Sex=="M")
dim(dat)
head(dat)
f=function(i){c(N=length(i),Mean=mean(i),StDev=sd(i))}
do.call(rbind,with(dat,tapply(Distance,Sex,f))) |> round(1) #Table

#Plot
ttl="Individual Value Plot of Distance vs Gender"
xlb="Gender"
xlm=c(-.25,1.25)
set.seed(312)
jit=runif(nrow(dat),-.1,.1)
(ybars=cbind(0:1,with(dat,tapply(Distance,Male,mean))))
with(dat,plot(Male+jit,Distance,xlab=xlb,xlim=xlm,main=ttl,xaxt="n"))
axis(1,0:1,c("F","M"))
points(ybars,pch=16,col="blue")
lines(ybars,col="blue")


#Slide 22
t.test(Distance~Sex,data=dat)


#Slide 23
polyunder=function(x,y,...){polygon(c(x, rev(x)), c(y*0, rev(y)),...)}
par(mfrow=c(1,3), mar=c(3,3,2,1), oma=c(0,0,4,0), mgp=c(1.4,.3,0))
x=seq(-4,4,length=1000)
i=300
bf=2
den=dnorm(x)
plot(x,den,type="l",xlab="t",ylab="Density",xaxt="n",yaxt="n")
axis(1, at=0, labels="0")
lines(c(-4,4),rep(0,2))
lines(rep(x[i],2),c(0,bf*den[i]))
text(x[i], bf*den[i], "t.s.", pos=3)
polyunder(x[1:i],den[1:i],col="skyblue")
title(expression("Shaded area is p-value if " ~ H[a] * ": parameter < " * "#"))

plot(x,den,type="l",xlab="t",ylab="Density",xaxt="n",yaxt="n")
axis(1, at=0, labels="0")
lines(c(-4,4),rep(0,2))
lines(rep(x[i],2),c(0,bf*den[i]))
text(x[i], bf*den[i], "-|t.s.|", pos=3)
lines(rep(-x[i],2),c(0,bf*den[i]))
text(-x[i], bf*den[i], "|t.s.|", pos=3)
polyunder(x[1:i],den[1:i],col="skyblue")
polyunder(-x[1:i],den[1:i],col="skyblue")
title(expression("Shaded area is p-value if " ~ H[a] * ": parameter \u2260 " * "#"))

plot(x,den,type="l",xlab="t",ylab="Density",xaxt="n",yaxt="n")
axis(1, at=0, labels="0")
lines(c(-4,4),rep(0,2))
lines(rep(-x[i],2),c(0,bf*den[i]))
text(-x[i], bf*den[i], "t.s.", pos=3)
polyunder(-x[1:i],den[1:i],col="skyblue")
title(expression("Shaded area is p-value if " ~ H[a] * ": parameter > " * "#"))
mtext("Anatomy of a t Test: Test Statistic Denoted by t.s.", 
 outer=TRUE, cex=1.5, font=2)

