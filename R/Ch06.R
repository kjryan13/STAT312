#NOTE: Run earlier code first. Later lines depend on it.
path="https://raw.githubusercontent.com/kjryan13/STAT312/main/CSV/"
read312=function(fname,pname=path,...) 
{
  full_path=paste0(pname,fname)
  read.csv(full_path,...)
}


#Slide 345
exam=read312("FourExams.csv")
exam=exam[order(exam$Student, exam$Exam), ]
dim(exam)
head(exam)
ewide=unstack(exam, Grade ~ Student)
dim(ewide)
head(ewide)
matplot(1:4,ewide,type="b",pch=substr(colnames(ewide),1,1),col=1:5,lty=1:5,
   xaxt="n",xlab="Exam",ylab="Grade",ylim=c(0,100))
axis(1,1:4)


#Slide 347
rec=read312("Rectangles.csv")
rec$Perimeter=2*(rec$Width+rec$Length)
rec=rec[order(rec$Width, rec$Length), ]
dim(rec)
head(rec)
rwide=unstack(rec, Perimeter ~ Width)
dim(rwide)
head(rwide)
matplot(c(1,4,10),rwide,type="b",pch=c("1","4","X"),col=1:3,lty=1:3,
   xlab="Width",ylab="Perimeter")


#Slide 348
rwide=unstack(rec, Area ~ Width)
dim(rwide)
head(rwide)
matplot(c(1,4,10),rwide,type="b",pch=c("1","4","X"),col=1:3,lty=1:3,
   xlab="Width",ylab="Area")


#Slide 351
exam=read312("FourExams.csv")
exam=exam[order(exam$Exam, exam$Student), ]
exam$Exam=paste0("Exam",exam$Exam)
ewide=unstack(exam, Grade ~ Exam)
rownames(ewide)=substr(sort(unique(exam$Student)),1,1)
dim(ewide)
head(ewide)
labs=rownames(ewide)
lower_fun=function(x, y, ...) {
  text(x, y, labels = labs)
  abline(0, 1, col = "gray70", lty = 3)
  fit=lm(y ~ x)
  abline(fit, col = "red", lwd = 2)
  s=coef(fit)[2]
  u=par("usr")
  text(u[1] + 0.2*(u[2]-u[1]), u[4] - 0.10*(u[4]-u[3]),
       paste0("slope=", round(s, 2)))
}
pairs(ewide,pch=LETTERS[1:5],upper.panel=NULL,lower.panel=lower_fun,
 main="Red=SLR and Gray=45\u00B0 reference line",asp=1)


#Slide 362
exam=read312("FourExams.csv")
exam$GrandMean=ybar=mean(exam$Grade)
(emeans=with(exam,tapply(Grade,Exam,mean)))
exam$ExamEffect=emeans[match(exam$Exam,names(emeans))]-ybar
(smeans=with(exam,tapply(Grade,Student,mean)))
exam$StudentEffect=smeans[match(exam$Student,names(smeans))]-ybar
exam$Residual=with(exam,Grade-(GrandMean+ExamEffect+StudentEffect))
exam


#Slide 366
exam=read312("FourExams.csv")
exam=transform(exam, Student=factor(Student), Exam=factor(Exam))
fexam=lm(Grade~Student+Exam,exam)
anova(fexam)


#Slide 371
par(mfrow=c(1,2))
plot(fitted(fexam),resid(fexam),pch=substr(exam$Student,1,1),
 xlab="Grade Predicted",ylab="Grade Residual")
abline(h=0,col="gray",lty=2,lwd=3)
qqnorm(resid(fexam),pch=substr(exam$Student,1,1),ylab="Grade Residual")


#Slide 374
(rec=read312("Rectangles.csv"))
rec=transform(rec, Length=factor(Length), Width=factor(Width))
COMP=function(data, response, fac1, fac2) 
{
  y=data[[response]]
  grand_mean=mean(y)
  fac1_eff=fitted(lm(y ~ data[[fac1]])) - grand_mean
  fac2_eff=fitted(lm(y ~ data[[fac2]])) - grand_mean
  comp=fac1_eff * fac2_eff/grand_mean
  return(comp)
}
rec$Comparison=COMP(rec,"Area","Length","Width")
rec$Residual=resid(lm(Area~Length+Width,rec))
rec
(slr=lm(Residual~Comparison,rec))
par(mfrow=c(1,1))
with(rec,plot(Comparison,Residual,pch=as.character(Width)))
abline(slr,col="red")


#Slide 376
(exam=read312("FourExams.csv"))
exam=transform(exam, Student=factor(Student), Exam=factor(Exam))
exam$Comparison=COMP(exam,"Grade","Student","Exam")
exam$Residual=resid(lm(Grade~Student+Exam,exam))
exam
(slr=lm(Residual~Comparison,exam))
par(mfrow=c(1,1))
with(exam,plot(Comparison,Residual,pch=substr(Student,1,1)))
abline(slr,col="red")


#Slide 381
finger=read312("FranticFingers.csv")
finger=transform(finger, Drug=factor(Drug), Subj=factor(Subj))
finger
ffinger=lm(Rate~Subj+Drug,finger)
anova(ffinger)


#Slides 382-384
#install.packages("emmeans") #1-time install
#install.packages("multcomp") #1-time install
library(emmeans)
library(multcomp)   
emm=emmeans(ffinger, "Drug")
summary(emm, infer = c(TRUE, TRUE))  
pairs(emm, adjust="none", infer=rep(TRUE,2))
g=glht(ffinger,linfct=mcp(Drug="Tukey")) #Need factors
cld(g)
