#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}
blocksample=function(B,A) as.vector(sapply(1:A, function(i) sample(1:B)))


#Slide 461
exam=read312("FourExams.csv",colClasses=c(rep("factor",3),"numeric"))
e=abs(resid(lm(Grade~Student,exam)))
anova(lm(e~exam$Student)) #Levene's test


#Slides 476, 478
fexam=lm(Grade~Student,exam)
anova(fexam)
fexam
L=c(0,-.5,-.5,.5,0)
(est=sum(coef(fexam)*L)) #Estimate
(se=sqrt(t(L)%*%vcov(fexam)%*%L)|> as.vector()) #SE
(ts=est/se) #Test stat
(dfe=df.residual(fexam)) #Degrees of freedom error
2*pt(-abs(ts),dfe) #P-value
est+qt(c(.025,.975),15)*se #CI


#Slide 482
hawk=read312("HawkTail.csv",colClasses=c("factor","numeric"))
dim(hawk)
head(hawk)
wilcox.test(Tail~Species,hawk) #Will match the p-value


#Slides 484, 485
exam
fexam=lm(Grade~Exam,exam)
anova(fexam)
par(mfrow=c(1,1))
qqnorm(resid(fexam),ylab="Grade Residual")
kruskal.test(Grade~Exam,exam)


#Slide 492 (first step takes a min)
Fdsn=sapply(1:10^4, function(i) summary(lm(Grade~sample(Exam),exam))$fstatistic[1])
hist(Fdsn,xlab="F",main="Permutation Distribution of F")
(F=summary(fexam)$fstatistic[1]) #Test statistic
abline(v=F,col="red",lwd=3)
mean(Fdsn>F) #p-value


#Slide 505
Grade=exam$Grade
Student=exam$Student
(df=data.frame(Student,Grade,model.matrix(~Student-1)))
anova(lm(Grade~Student)) #ANOVA and MLR with dummies give same MSE
anova(lm(Grade~StudentAdam+StudentBrenda+StudentCathy+StudentDave,df))


#Slide 513
pulse=read312("Pulse.csv",colClasses=c(rep("numeric",4),"factor",rep("numeric",2)))
pulse=cbind(pulse,with(pulse,model.matrix(~Exercise-1)))
dim(pulse)
head(pulse)
colnames(pulse)[4]="Female"
fpulse=lm(Active~Female+Exercise2+Exercise3+Rest,pulse)
anova(fpulse)
summary(fpulse)


#Slides 529, 532 (sapply command takes a min)
exam
fexam=lm(Grade~Student+Exam,exam)
anova(fexam)
(F=anova(fexam)["Exam","F value"]) #Test statistic
Fdsn=sapply(1:10^4,function(i) anova(lm(Grade~Student+Exam[blocksample(4,5)],exam))["Exam","F value"])
hist(Fdsn,xlab="F",main="Permutation Distribution of F")
abline(v=F,col="red",lwd=3)
mean(Fdsn>F) #p-value
