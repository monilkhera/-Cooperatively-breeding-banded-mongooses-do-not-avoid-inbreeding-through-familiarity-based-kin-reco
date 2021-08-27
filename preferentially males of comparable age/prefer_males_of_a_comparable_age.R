
#### ACTUAL RELATEDNESS BETWEEN BREEDING PAIRS ####

#remove the individual with no birth date (SF035) from the initial sire list
sire_list["SF035"]<- NULL

#find out actual relatedness

ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(sire_list))
ped_post_exclusion_relatedness2<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]

mean(ped_post_exclusion_relatedness2$relatedness) #0.1442759

#### PREFERENTIALLY MATE WITH MALES OF A COMPARABLE AGE ####

#need to remove SF035 from birth list and relatedness list - as it has no birth date

relatedness_list["SF035"]<- NULL
age_list["SF035"]<- NULL


random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:length(relatedness_list)){
    simrel[j]<-sample(relatedness_list[[j]],1, prob = max(age_list[[j]]) - age_list[[j]])
  }
  
  random_mating[i]<-mean(simrel)
  
}

age_based_mating<- random_mating

write.csv(age_based_mating, "age_based_mating.csv")

mean(age_based_mating$x) #0.1749116


#### NULL RANDOMISATION ####

#run null using 350 datapoints - Same null sire list as for avoiding communal litter mates

pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(sire_list))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

null <- list()
for (i in 1:length(sire_list)){ 
  null[[i]]<-sire_list[[i]][! sire_list[[i]] %in% sire_list[[i]][is.na(match(sire_list[[i]],banded.mongoose.pedigree.from.David.2016$id))]]
}

names(null)<- names(sire_list)

BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)


random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(null[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}

mean(random_mating)    #  0.1756929  

########################################################################################################################################

#P.VALUE AND CI VALUE 

dist<- age_based_mating$x
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

p.rand(dist, value)

#$`95% CI`
#[1] 0.160234 0.189653

#$`P-value`
#[1] 0

########################################################################################


null<- null_for_avoid_litter_mates$x
avoid<- age_based_mating$x

ks.test(avoid,null)

#D = 0.0453, p-value = 2.449e-09
#alternative hypothesis: two-sided






