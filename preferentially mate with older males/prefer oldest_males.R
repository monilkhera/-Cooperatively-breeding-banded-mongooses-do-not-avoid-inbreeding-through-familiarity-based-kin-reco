#PREFER MATING WITH THE OLDEST MALES 


BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)

random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam)){
    simrel[j]<-sample(oldest_males[[pedigree_pup_dam$id[j]]],1)    
  }
  
  pedigree_pup_dam$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam)){
    rel[h]<- BMRP[pedigree_pup_dam$dam[h],pedigree_pup_dam$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}

mean(random_mating)    #0.1586694  

oldest_randomisation<- random_mating

write.csv(oldest_randomisation, "randomisation_oldest_males.csv")

#### null randomisation ####

#this would be the initial randomiation - see folder

###########################################################################################################################################################
null<-true_initial_randomisation$x

oldest_males<- randomisation_oldest_males$x

hist(null,xlab="Mean male-female relatedness",xlim=c(0.135,0.22), ylim = c(0,3000), main="",col= "gray70", border = "black") 
hist(oldest_males,xlab="Mean male-female relatedness",xlim=c(0.125,0.22), ylim = c(0,2700), main="",col= rgb(1,1,0,0.4), border= "black",add=T)

legend(0.195,3100, c ("Random mating", "Older males"), col=c("gray70", "khaki1"), cex = 0.65, lwd=10)

arrows(0.1438649,1470,0.1438649 ,950,col = "red", length = 0.17, angle = 30)
text(0.141,1690,labels = "Observed value", cex = 0.80)
text(0.1439,1550,labels = "(0.144)", cex = 0.80)

##############################################################################################################################################################################################

#p.value and ci values 

dist<- randomisation_oldest_males$x
value<- 0.1438649

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

#p.rand(dist,value)
#$`95% CI`
#[1] 0.1447327 0.1723572

#$`P-value`
#[1] 0.0384
#######################################################################################################


null<- true_initial_randomisation$x
avoid<- randomisation_oldest_males$x

ks.test(avoid,null)

#D = 0.7475, p-value < 2.2e-16
#alternative hypothesis: two-sided
