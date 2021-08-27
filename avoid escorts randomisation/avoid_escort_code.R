
#ACTUAL RELATEDNESS BETWEEN BREEDING PAIRS

ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(avoiding_escort))
new_ped_post_exclusion_relatedness<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]

mean(new_ped_post_exclusion_relatedness$relatedness)   #0.1650775

#AVOID ESCORTS
pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(avoiding_escort))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

View(pedigree_pup_dam_2)

BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)

random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(avoiding_escort[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "randomisation_for_avoid_escorts.csv")

mean(random_mating)    #0.1814522    

p.value95<-(length(which(random_mating<0.1650775 )))/100 #7.86

hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.125,0.23),main="",col="grey16",border="white")


arrows(0.1650775,1300,0.1650775 ,900,col = "red", length = 0.17, angle = 30)
text(0.157,1480,labels = "Observed value", cex = 0.80)
text(0.160,1380,labels = "(0.1651)", cex = 0.80)

######################################################################################################################

#NULL randomisation

sire_list["BF671"]<- NULL 


random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(sire_list[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "null_avoid_escort code.csv")

mean(null_avoid_escort.code$x) #0.1789068

hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.125,0.235), ylim = c(0,2000), main="",col="grey16",border="white")


arrows(0.1650775,1650,0.1650775 ,1190,col = "red", length = 0.17, angle = 30)
text(0.160,1800,labels = "Observed value", cex = 0.80)
text(0.163,1700,labels = "(0.1651)", cex = 0.80)

########################################################################################################################################

# PUTTING TWO HISTOGRAMS TOGETHER 

#import null randomisation

null<-null_avoid_escort.code$x

#import escort excluion randomisation

avoid_escort<- randomisation_for_avoid_escorts$x

hist(null,xlab="Mean male-female relatedness",xlim=c(0.125,0.235), ylim = c(0,2000), main="",col= "gray70", border = "black") 
hist(avoid_escort,xlab="Mean male-female relatedness",xlim=c(0.125,0.235), ylim = c(0,2000), main="",col= rgb(1,1,0,0.4), border= "black",add=T)

legend(0.20,1900, c ("Random mating", "Avoid escorts"), col=c("gray70", "khaki1"), cex = 0.65, lwd=10)

arrows(0.1650775,1650,0.1650775 ,1190,col = "red", length = 0.17, angle = 30)
text(0.160,1800,labels = "Observed value", cex = 0.80)
text(0.163,1700,labels = "(0.165)", cex = 0.80)

############################################################################################################################################

#P.VALUE AND CI FOR AVOID ESCORT

dist<- randomisation_for_avoid_escorts$x
value<- 0.1650775


p.rand<-function(dist,value,type="two-tailed"){
  
  ci<-quantile(dist,probs=c(0.025,0.975),names=F)
  
  
  
  if (type=="is.lower") {
    
    p<-length(which(dist<=value))/length(dist)
    
  }
  
  else if (type=="is.higher") {
    
    p<-length(which(dist>=value))/length(dist)
    
  }
  
  else if (type=="two-tailed") {
    
    low<-length(which(dist<=value))
    
    high<-length(which(dist>=value))
    
    p<-2*min(c(low,high))/length(dist)
    
  }
  
  
  
  return(list("95% CI"=ci,"P-value"=p))
  
}

p.rand(dist,value)

#CI IS 0.1589773 0.2041420
#P.VALUE IS 0.1572

###################################################################################################################

#P.VALUE AND CI OF NULL 

dist<- null_avoid_escort.code$x
value<- 0.1650775


p.rand<-function(dist,value,type="two-tailed"){
  
  ci<-quantile(dist,probs=c(0.025,0.975),names=F)
  
  
  
  if (type=="is.lower") {
    
    p<-length(which(dist<=value))/length(dist)
    
  }
  
  else if (type=="is.higher") {
    
    p<-length(which(dist>=value))/length(dist)
    
  }
  
  else if (type=="two-tailed") {
    
    low<-length(which(dist<=value))
    
    high<-length(which(dist>=value))
    
    p<-2*min(c(low,high))/length(dist)
    
  }
  
  
  
  return(list("95% CI"=ci,"P-value"=p))
  
}

p.rand(dist,value)

#CI is 0.1563419 0.2026241
#p.value is 0.2306

####################################################################################################################

#check if the distributions overlap too much 

null<- null_avoid_escort.code$x
avoid<- randomisation_for_avoid_escorts$x

ks.test(avoid,null)

#Two-sample Kolmogorov-Smirnov test

#data:  avoid and null
#D = 0.0965, p-value < 2.2e-16
#alternative hypothesis: two-sided


