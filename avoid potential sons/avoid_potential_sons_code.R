

# actual relatedness between breeding pairs 

ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(avoid_potential_sons))
ped_post_exclusion_relatedness2<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]

mean(ped_post_exclusion_relatedness2$relatedness) # 0.1428985

#########################################################################################################################################################



#########################################################################################################################################################

#run null using 349 datapoints

#sire list called "null_for_potential_son"

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


write.csv(random_mating, "null_for_avoid_potential_sons.csv")

mean(random_mating)    #  0.1747511 



hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.215),main="",col="grey16",border="white")


arrows(0.1442759,400,0.1442759,100,col = "red", length = 0.17, angle = 30)
text(0.144,550,labels = "Observed value", cex = 0.80)
text(0.145,450,labels = "(0.144)", cex = 0.80)


#####################################################################################################################################################

# run the actual randomisation with 349



pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(avoid_potential_sons))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

avoiding_pot_sons <- list()
for (i in 1:length(avoid_potential_sons)){ 
  avoiding_pot_sons[[i]]<-avoid_potential_sons[[i]][! avoid_potential_sons[[i]] %in% avoid_potential_sons[[i]][is.na(match(avoid_potential_sons[[i]],banded.mongoose.pedigree.from.David.2016$id))]]
}

names(avoiding_pot_sons)<- names(avoid_potential_sons)



random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(avoiding_pot_sons[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "avoiding_potential_sons_randomisation.csv")

mean(random_mating)    #0.1583274    


################################################################################################################
#histogram of avoid litter mates randomisaiton


hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.21),main="",col="grey16",border="white")


arrows(0.1428985,550,0.1428985,200,col = "red", length = 0.17, angle = 30)
text(0.143,700,labels = "Observed value", cex = 0.80)
text(0.143,600,labels = "(0.144)", cex = 0.80)

##################################################################################################################



