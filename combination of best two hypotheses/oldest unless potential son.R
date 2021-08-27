#combination of the two best - so the oldest 3 unless they are potential sons 

#find out actual relatedness

ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(oldest_unless_potential_son))
ped_post_exclusion_relatedness2<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]

mean(ped_post_exclusion_relatedness2$relatedness) #  0.1428985


#########################################################################################################################################
#NULL randomisation

#run null using 349 datapoints

#first upload initial sire list - then remove those dams without any sires left for combo of best two randomization
sire_list["DM171"]<- NULL
sire_list["DM124"]<- NULL

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


write.csv(random_mating, "null_for_oldest_unless_potential_son.csv")

mean(random_mating)    #  0.1746302



hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.215),main="",col="grey16",border="white")


arrows(0.1428985,400,0.1428985,100,col = "red", length = 0.17, angle = 30)
text(0.144,550,labels = "Observed value", cex = 0.80)
text(0.145,450,labels = "(0.143)", cex = 0.80)


#####################################################################################################################################################


#########################################################################################################################################
#OLDEST UNLESS POTENTIAL SON

pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(oldest_unless_potential_son))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

oldest_unless_pot_son <- list()
for (i in 1:length(oldest_unless_potential_son)){ 
  oldest_unless_pot_son[[i]]<-oldest_unless_potential_son[[i]][! oldest_unless_potential_son[[i]] %in% oldest_unless_potential_son[[i]][is.na(match(oldest_unless_potential_son[[i]],banded.mongoose.pedigree.from.David.2016$id))]]
}

names(oldest_unless_pot_son)<- names(oldest_unless_potential_son)



random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(oldest_unless_pot_son[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "oldest_unless_potential_son_randomisation.csv")

mean(random_mating)    # 0.1571263  




################################################################################################################
#histogram of avoid litter mates randomisaiton


hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.21),main="",col="grey16",border="white")


arrows(0.1428985,550,0.1428985,200,col = "red", length = 0.17, angle = 30)
text(0.143,700,labels = "Observed value", cex = 0.80)
text(0.143,600,labels = "(0.143)", cex = 0.80)

##################################################################################################################

# PUTTING TWO HISTOGRAMS TOGETHER 




#P.VALUE AND CI

randomisation<- oldest_unless_potential_son_randomisation
dist<- randomisation$x
value<- 0.1428985

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

# $`95% CI`
# [1] 0.1440496 0.1700209
# 
# $`P-value`
# [1] 0.0324

#####################################################################################################

#NULL FOR P.VALUE AND CI VALUE 

dist<- null_for_oldest_unless_potential_son$x
value<- 0.1428985

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

# $`95% CI`
# [1] 0.1596354 0.1904628
# 
# $`P-value`
# [1] 0

##################################################################################################################

#check if the distributions overlap too much 

null<- null_for_oldest_unless_potential_son$x

oldest_unless_potential_son_randomisation<- oldest_unless_potential_son_randomisation$x



ks.test(oldest_unless_potential_son_randomisation,null)

# Two-sample Kolmogorov-Smirnov test
# 
# data:  oldest_unless_potential_son_randomisation and null
# D = 0.7774, p-value < 2.2e-16
# alternative hypothesis: two-sided












