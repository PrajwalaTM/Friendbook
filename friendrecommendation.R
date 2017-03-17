setwd("E:/Computer Engg/Seminar/")
library(igraph)
library(ggplot2)
library("reshape2")
lifestyle<-matrix(NA, nrow=1000, ncol=10)
for (i in 1:1000) {
lifestyle[i,]<- runif(10)
}
head(lifestyle)
for(i in 1:1000) {
  sum=0
  for(j in 1:10)
  {
    sum=sum+lifestyle[i,j]
  }
  for(j in 1:10)
  {
    lifestyle[i,j]=lifestyle[i,j]/sum
  }
}
s=0
for(j in 1:10)
 s=s+lifestyle[1,j] 
S.c<-matrix(NA,nrow=1000,ncol=1000)
for(i in 1:1000)
{
  for(j in 1:1000)
    S.c[i,j]=crossprod(lifestyle[i,],lifestyle[j,])/sqrt(crossprod(lifestyle[i,])*crossprod(lifestyle[j,]))
}
lifestyle.sorted<-matrix(NA,nrow=1000,ncol=10)
for(i in 1:1000)
lifestyle.sorted[i,]=lifestyle[i,order(-lifestyle[i,])]
dominant.lifestyle<-matrix(NA,nrow=1000,ncol=10)
lambda=0.6;
for(i in 1:1000)
{
  sum=0;
  for(j in 1:10)
  {
    sum=sum+lifestyle.sorted[i,j];
    if(sum<lambda)
      dominant.lifestyle[i,j]=lifestyle.sorted[i,j];
  }
}
S.d<-matrix(NA,nrow=1000,ncol=1000);
for(i in 1:1000)
{
  for(j in 1:1000)
  {
  a=length(dominant.lifestyle[!is.na(dominant.lifestyle[i,])])
  b=length(dominant.lifestyle[!is.na(dominant.lifestyle[j,])])
  c=length(intersect(dominant.lifestyle[i,],dominant.lifestyle[j,]));
  S.d[i,j]=2*c/(a+b);
  }
}
S<-matrix(NA,nrow=1000,ncol=1000);
for(i in 1:1000)
{
  for(j in 1:1000)
    S[i,j]=S.c[i,j]*S.d[i,j];
}
true.friends1<-matrix(NA,nrow=1000,ncol=1000);
for(i in 1:1000)
  true.friends1[i,]=order(-S[i,])

true.friends2<-matrix(NA,nrow=1000,ncol=100);
for(i in 1:1000)
{
  for(j in 1:100)
    true.friends2[i,j]=true.friends1[i,j]
}
sim.thresh=0.0002;
fmg<-matrix(NA,nrow=1000,ncol=1000);
for (i in 1:1000)
{
  for(j in 1:1000)
  {
    if(i!=j & S[i,j]>sim.thresh)
      fmg[i,j]=S[i,j]
    else
      fmg[i,j]=0;
  }
}
g <- graph.adjacency(fmg, weighted=TRUE, mode="undirected")
r=page.rank (g, vids = V(g), directed = FALSE, damping = 0.85, 
           weights = NULL, options = igraph.arpack.default)$vector
k=100
beta=1
A = beta*S
A=A+(1-beta)*r*k
r.precision=0
r.recall=0

rec.prec<-matrix(0,nrow=1,ncol=10);
rec.recall<-matrix(0,nrow=1,ncol=10);
recommended.friends1<-matrix(NA,nrow=1000,ncol=1000);
for(i in 1:1000)
  recommended.friends1[i,]=order(-A[i,])
l=1
#rp=data.frame()
#rc=data.frame()
#rp=rbind(rp,c(0,0.3,0.6,1))
#rc=rbind(rc,c(0,0.3,0.6,1))
#k=1
#for (beta in c(0,0.3,0.6,1))
#{
for(p in c(100,200,300,400,500,600,700,800,900,1000))
{
  recommended.friends2<-matrix(NA,nrow=1000,ncol=p);
for(i in 1:1000)
{
  for(j in 1:p)
    recommended.friends2[i,j]=recommended.friends1[i,j]
}
for(i in 1:1000)
{
    a=length(true.friends2[i,])
    b=length(recommended.friends2[i,])
    c=length(intersect(true.friends2[i,],recommended.friends2[i,]));
    rec.prec[1,l]=rec.prec[1,l]+c/b;
    rec.recall[1,l]=rec.recall[1,l]+c/a;
    #r.precision=r.precision+c/b;
    #r.recall=r.recall+c/a;
  
}
rec.prec[1,l]=rec.prec[1,l]/10
rec.recall[1,l]=rec.recall[1,l]/10
l=l+1
}
r.precision=r.precision/1000
r.recall=r.recall/1000
    #rp=rbind(rp,rec.prec[k,])
    #rc=cbind(rc,rec.recall[k,])
  #k=k+1
#}
#mdf <- melt(rp, id.vars="Company", value.name="value", variable.name="Year")
number=c(100,200,300,400,500,600,700,800,900,1000)
plot(number,t(rec.prec),type = "o", col = "red", xlab = "No. of recommended friends", ylab ="Recommendation Precision" ,
     main = "Recommendation Precision")
plot(number,t(rec.recall),type = "o", col = "red", xlab = "No. of recommended friends", ylab ="Recommendation Recall" ,
     main = "Recommendation Recall")


#g=graph(fmg)
#M = get.adjacency(fmg, sparse = FALSE)
#M = t(g / rowSums(g))
#n = nrow(M)
#ranking.vector<-matrix(NA,nrow=1,ncol=1000);
#new.ranking.vector<-matrix(NA,nrow=1,ncol=1000);
#for(i in 1:1000)
 #ranking.vector[1,i]=0.001;

#delta=Inf;
#epsilon=exp(-10)
#damp=0.0005

#while(delta>epsilon)
#{
#  for(i in 1:1000)
  
#    x=0
#    y=0
 #   z=0
  #  for(j in 1:1000)
  #    {
  #    x=x+ranking.vector[1,j]*((1-damp)*0.001)
  #    y=y+fmg[i,j]*ranking.vector[1,j];
  #    z=z+fmg[i,j]
  #  }
  #  new.ranking.vector[1,i]=x+damp*(y/z);
  #}
#  delta=new.ranking.vector[1,1]-ranking.vector[1,1];
#  for(i in 2:1000)
#    delta=delta+abs(new.ranking.vector[1,i]-ranking.vector[1,i]);
#  for(i in 1:1000)
#    ranking.vector[1,i]=new.ranking.vector[1,i];
#


