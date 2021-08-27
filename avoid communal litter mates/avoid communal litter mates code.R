
#remove the individual with no litter code (SF035) from the initial sire list- as for avoid litter you only have 350
sire_list["SF035"]<- NULL

#find out actual relatedness
  
  ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(sire_list))
  ped_post_exclusion_relatedness2<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]
  
  mean(ped_post_exclusion_relatedness2$relatedness) #0.1442759
  
#########################################################################################################################################################
  
#run null using 350 datapoints
  
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
  
  
  write.csv(random_mating, "null_for_avoid_litter_mates.csv")
  
  mean(random_mating)    #  0.1756929  
  
  

  hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.215),main="",col="grey16",border="white")
  
  
  arrows(0.1442759,400,0.1442759,100,col = "red", length = 0.17, angle = 30)
  text(0.144,550,labels = "Observed value", cex = 0.80)
  text(0.145,450,labels = "(0.144)", cex = 0.80)
  
  
#####################################################################################################################################################

 # run the actual randomisation with 350
  
  avoid_litter_final["SF035"]<- NULL
  
  pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(avoid_litter_final))
  pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

  avoiding_litter <- list()
  for (i in 1:length(avoid_litter_final)){ 
    avoiding_litter[[i]]<-avoid_litter_final[[i]][! avoid_litter_final[[i]] %in% avoid_litter_final[[i]][is.na(match(avoid_litter_final[[i]],banded.mongoose.pedigree.from.David.2016$id))]]
  }

  names(avoiding_litter)<- names(avoid_litter_final)


  
random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(avoiding_litter[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}


write.csv(random_mating, "avoid_litter.csv")

mean(random_mating)    # 0.1728427     


################################################################################################################
#histogram of avoid litter mates randomisaiton


hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.21),main="",col="grey16",border="white")


arrows(0.1442759,550,0.1442759,200,col = "red", length = 0.17, angle = 30)
text(0.144,700,labels = "Observed value", cex = 0.80)
text(0.144,600,labels = "(0.144)", cex = 0.80)

##################################################################################################################

# PUTTING TWO HISTOGRAMS TOGETHER 

#import null randomisation

null<-null_for_avoid_litter_mates$x

#import litter mate excluion randomisation


avoid_litter<- avoid_litter$x

hist(null,xlab="Mean male-female relatedness",xlim=c(0.125,0.235), ylim = c(0,2700), main="",col= "gray70", border = "black") 
hist(avoid_litter,xlab="Mean male-female relatedness",xlim=c(0.125,0.235), ylim = c(0,2700), main="",col= rgb(1,1,0,0.4), border= "black",add=T)

legend(0.20,2400, c ("Random mating", "Avoid litter mates"), col=c("gray70", "khaki1"), cex = 0.65, lwd=10)

arrows(0.1442759,600,0.1442759 ,150,col = "red", length = 0.17, angle = 30)
text(0.140,850,labels = "Observed value", cex = 0.80)
text(0.143,720,labels = "(0.144)", cex = 0.80)

#######################################################################################################################################################

#P.VALUE AND CI

randomisation<- avoid_litter
dist<- randomisation$x
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

#CI IS 0.1572852 0.1880999
#P.VALUE IS 0

#####################################################################################################

#NULL FOR P.VALUE AND CI VALUE 

dist<- null_for_avoid_litter_mates$x
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
#[1] 0.1600811 0.1910666

#$`P-value`
#[1] 0

##################################################################################################################

#check if the distributions overlap too much 

null<- null_for_avoid_litter_mates$x

avoid_litter_mates<- avoid_litter

avoid<- avoid_litter_mates$x

ks.test(avoid,null)

#Two-sample Kolmogorov-Smirnov test

#data:  avoid and null
#D = 0.1459, p-value < 2.2e-16
#alternative hypothesis: two-sided



