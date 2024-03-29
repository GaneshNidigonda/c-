library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)
pr.out=prcomp(nci.data , scale = TRUE)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="z2")
plot(pr.out$x[,c(1,3)],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z3")
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(pve,type="o",xlab="Principal component",ylab="PVE",col="blue")
plot(cumsum(pve),type="o",xlab="Principal Component",ylab="cumulative 
PVE",col="brown")
sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs , main="Complete Linkage", xlab="", sub="", 
     ylab = "")
plot(hclust(data.dist,method="average"),labels=nci.labs,main="Average",xlab="",y
     lab = "",sub="")
plot(hclust(data.dist,method="single"),labels=nci.labs,main="single",xlab="",ylab 
     = "",sub="")
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters ,nci.labs)
par(mfrow=c(1,1))
plot(hc.out , labels=nci.labs)
abline(h=139,col="red")
hc.out
set.seed(2)
km.out=kmeans(sd.data,4,nstart = 20)
kmclusters=km.out$cluster
table(kmclusters,hc.clusters)
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels=nci.labs,main="hdsfko")
table(cutree(hc.out,4),nci.labs)

