
#### relatedness between actual breeding pairs ####

#find out actual relatedness, (name for new_sire_list is "null_for_avoid_true_litter.RDATA")

ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(new_sire_list))
new_ped_post_exclusion_relatedness<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]

mean(new_ped_post_exclusion_relatedness$relatedness)#0.1442759


##########################################################################################################################
#AVOID MATERNAL LITTERMATES RANDOMISATION

pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(avoid_true_litter_final))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]


BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)


random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(avoid_true_litter_final[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "avoid_littermates_and_same_mother.csv")

mean(random_mating)     # 0.1723261

hist(random_mating)

#actual obserevd reladedness = #0.1442759

###############################################################################################



##########################################################################################################################
#NULL RANDOMISATION

pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(new_sire_list))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]


BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)


random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(new_sire_list[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "null_avoid_true_litter_mates.csv")

mean(random_mating)     # 0.1785325

hist(random_mating)





#######################################################################
dist<- avoid_littermates_and_same_mother$x
value<- 0.1442759

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

#$`95% CI`
#[1] 0.157866 0.187289

#$`P-value`
#[1] 0


#####################################################################################################

#NULL FOR P.VALUE AND CI VALUE 

dist<- null_avoid_true_litter_mates$x
value<- 0.1442759

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

#$`95% CI`
#[1] 0.1636848 0.1938235

#$`P-value`
#[1] 0
##################################################################################################################

#check if the distributions overlap too much 


ks.test(avoid_litter,null)

#Two-sample Kolmogorov-Smirnov test

#data:  avoid_litter and null
#D = 0.3193, p-value < 2.2e-16
#alternative hypothesis: two-sided

