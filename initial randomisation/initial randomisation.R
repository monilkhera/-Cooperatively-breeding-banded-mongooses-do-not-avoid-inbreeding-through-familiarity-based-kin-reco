
#View(BMRP_relatedness_July_2016)
BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)

#in_pedigree is the name of initial sire list using post_exclusion_pedigree with 351 individuals
final_sires_1<- sire_list

#example shown in 1b simulation of successfull pairs
random_mating1<- c()
for (i in 1:10000){
   
     simrel<- c()
     for(j in 1:nrow(pedigree_pup_dam2)){
         simrel[j]<-sample(final_sires_1[[pedigree_pup_dam2$id[j]]],1)
     }
    
     pedigree_pup_dam2$potsire<- simrel
     
     
     rel<-c()
       for (h in 1:nrow(pedigree_pup_dam2)){
         rel[h]<- BMRP[pedigree_pup_dam2$dam[h],pedigree_pup_dam2$potsire[h]] 
       }
     
    random_mating1[i]<-mean(rel)
          
}

write.csv(random_mating1, "true_initial_randomisation.csv")

mean(random_mating1)    #0.1756008

# actual mean is 0.1438649 - calculated using pedigree_post_exclusion relatedness dataframe 
####################################################################################################################################
#P.VALUE + CI

dist<- true_initial_randomisation$x
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

#CI is 0.16011 0.19097

#p.value is 0

################################################################################################################################
#histogram of initial randomisaiton


hist(true_initial_randomisation$x,xlab="Mean male-female relatedness",xlim=c(0.13,0.20),main="",col="grey70",border="black")

arrows(0.1438649,600,0.1438649,30,col = "red", length = 0.17, angle = 30)
text(0.143,820,labels = "Observed value", cex = 0.80)
text(0.144,700,labels = "(0.144)", cex = 0.80)


