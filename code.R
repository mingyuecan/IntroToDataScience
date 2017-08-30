## load csv. file into R:
Arrowsmith<-read.csv('/Users/Constance/Desktop/Arrowsmith.csv',skip = 4,colClasses=c(rep(NA,15),rep("NULL",14)))

##transformation:
X1<-ifelse((Arrowsmith$nA>1|Arrowsmith$A.lit.size<1000) & (Arrowsmith$nC>1|Arrowsmith$C.lit.size<1000),1,0)
X2<-ifelse(Arrowsmith$nof.MeSH.in.common>0 &Arrowsmith$nof.MeSH.in.common<99999,1,ifelse(Arrowsmith$nof.MeSH.in.common==99999,0.5,0))
X3<-ifelse(Arrowsmith$nof.semantic.categories>0,1,0)
X4<-ifelse(Arrowsmith$cohesion.score<0.3,Arrowsmith$cohesion.score,0.3)
X5<--abs(log10(Arrowsmith$n.in.MEDLINE)-3)
X6<-pmax(pmin(Arrowsmith$X1st.year.in.MEDLINE,2005),1950)
X7<-pmin(8,-log10(Arrowsmith$pAC+0.000000001))
I1<-ifelse(Arrowsmith$Arrowsmith.search=='retinal detachment vs aortic aneurysm',1,0)
I2<-ifelse(Arrowsmith$Arrowsmith.search=='NO and mitochondria vs PSD',1,0)
I3<-ifelse(Arrowsmith$Arrowsmith.search=='mGluR5 vs lewy bodies',1,0)
I4<-ifelse(Arrowsmith$Arrowsmith.search=='magnesium vs migraine',1,0)
I5<-ifelse(Arrowsmith$Arrowsmith.search=='Calpain vs PSD',1,0)
I6<-ifelse(Arrowsmith$Arrowsmith.search=='APP vs reelin',1,0)
Y<-ifelse(Arrowsmith$target==0|Arrowsmith$target==2,1,0)

##summary statistics before transformation:
summary(Arrowsmith)

##histograms before transformation:
hist(as.numeric(Arrowsmith$Arrowsmith.search),main="Arrowsmith.search")
hist(Arrowsmith$A.lit.size)
hist(Arrowsmith$C.lit.size)
hist(as.numeric(Arrowsmith$B.term),main="B.term")
hist(Arrowsmith$target)
hist(Arrowsmith$nA)
hist(Arrowsmith$nC)
hist(Arrowsmith$nof.MeSH.in.common,main="nof.MeSH.in.common")
hist(Arrowsmith$nof.semantic.categories,main="semantic.categories")
hist(Arrowsmith$cohesion.score,main="cohesion.score")
hist(Arrowsmith$n.in.MEDLINE,main="n.in.MEDLINE")
hist(Arrowsmith$X1st.year.in.MEDLINE,main="X1st.year.in.MEDLINE")
hist(Arrowsmith$pAC)
hist(Arrowsmith$on.medium.stoplist.,main="on.medium.stoplist.")
hist(Arrowsmith$on.long.stoplist.,main="on.long.stoplist.")

##pairwise scatter plots before transformation:
plot(as.numeric(Arrowsmith$Arrowsmith.search),Arrowsmith$target,xlab="Arrowsmith.search")
plot(Arrowsmith$A.lit.size,Arrowsmith$target)
plot(Arrowsmith$C.lit.size,Arrowsmith$target)
plot(as.numeric(Arrowsmith$B.term),Arrowsmith$target,xlab="B.term")
plot(Arrowsmith$nA,Arrowsmith$target)
plot(Arrowsmith$nC,Arrowsmith$target)
plot(Arrowsmith$nof.MeSH.in.common,Arrowsmith$target)
plot(Arrowsmith$nof.semantic.categories,Arrowsmith$target)
plot(Arrowsmith$cohesion.score,Arrowsmith$target)
plot(Arrowsmith$n.in.MEDLINE,Arrowsmith$target)
plot(Arrowsmith$X1st.year.in.MEDLINE,Arrowsmith$target)
plot(Arrowsmith$pAC,Arrowsmith$target)

##looking for missing value before transformation:
which(Arrowsmith$nof.MeSH.in.common==99999)
which(Arrowsmith$cohesion.score==0.9999)
which(Arrowsmith$X1st.year.in.MEDLINE==9999)

##construct a data frame using X1,X2,X3,X4,X5,X6,X7,I1,I2,I3,I4,I5,I6,I7,I8 as variables:
feas<-data.frame(X1,X2,X3,X4,X5,X6,X7,I1,I2,I3,I4,I5,I6,Y)
##summary statistics after transformation:
summary(feas)

##histograms after transformation:
hist(X1)
hist(X2)
hist(X3)
hist(X4)
hist(X5)
hist(X6)
hist(X7)
hist(I1)
hist(I2)
hist(I3)
hist(I4)
hist(I5)
hist(I6)
hist(Y)

##pairwise scatter plots after transformation:
plot(X1,Y)
plot(X2,Y)
plot(X3,Y)
plot(X4,Y)
plot(X5,Y)
plot(X6,Y)
plot(X7,Y)

##looking for missing value after transformation:
which(X2==0.5)

##logistic regression model:
fml<-glm(Y~X1+X2+X3+X4+X5+X6+X7+I1+I2+I3+I4+I5+I6,family=binomial)
summary(fml)
