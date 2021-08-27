
#ACTUAL RELATEDNESS BETWEEN BREEDING PAIRS 

ped_post_exclusion_relatedness$not<-match(ped_post_exclusion_relatedness$id,names(avoid_babysitters2))
ped_post_exclusion_relatedness2<- ped_post_exclusion_relatedness[which(ped_post_exclusion_relatedness$not!= "NA"),]

mean(ped_post_exclusion_relatedness2$relatedness) #0.159765


#### NULL RANDOMISATION ####

#NULL

#import initial sire list and remove the 93 but dont remove babysitters

sire_list<- sire_list[-c(1,2,3,4,5,6,7,8,10,11,12,13,14,20,30,31,32,33,34,35,36,37,39,40,43,45,46,47,48,50,51,54,55,57,59,60,62,63,64,65,66,74,75,76,80,81,83,84,86,87,88,89,90,94,98,99,105,110,111,112,120,123,124,127,130,138,139,141,149,168,170,173,178,180,181,186,187,193,194,247,258,264,265,272,275,290,291,297,309,330,331,332,333)]

#so now there are 258

#now I need to remove the other ones where no potential sires were left after removing babysitters from initial sire list

sire_list["BF353"]<- NULL
sire_list["BM607"]<- NULL
sire_list["BF608"]<- NULL
sire_list["BF671"]<- NULL
sire_list["BF674"]<- NULL
sire_list["BM662"]<- NULL
sire_list["BF712"]<- NULL
sire_list["BM707"]<- NULL
sire_list["BM715"]<- NULL

#went from 258 to 249


pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(sire_list))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

sire_list_new <- list()
for (i in 1:length(sire_list)){ 
  sire_list_new[[i]]<-sire_list[[i]][! sire_list[[i]] %in% sire_list[[i]][is.na(match(sire_list[[i]],banded.mongoose.pedigree.from.David.2016$id))]]
}

View(sire_list)

names(sire_list_new)<- names(sire_list)

BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)


random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(sire_list_new[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}

write.csv(random_mating, "null_randomisation_for_avoid_babysitter.csv")

mean(random_mating) #0.1842939


############################################################################################################################################################################################################################################################################################
#AVOID BABYSITTERS RANDOMISATION

pedigree_pup_dam$not<-match(pedigree_pup_dam$id,names(avoid_babysitters2))
pedigree_pup_dam_2<- pedigree_pup_dam[which(pedigree_pup_dam$not!= "NA"),]

avoidd_babysitters <- list()
for (i in 1:length(avoid_babysitters2)){ 
  avoidd_babysitters[[i]]<-avoid_babysitters2[[i]][! avoid_babysitters2[[i]] %in% avoid_babysitters2[[i]][is.na(match(avoid_babysitters2[[i]],banded.mongoose.pedigree.from.David.2016$id))]]
}

names(avoidd_babysitters)<- names(avoid_babysitters2)
View(avoidd_babysitters)

BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)

random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:nrow(pedigree_pup_dam_2)){
    simrel[j]<-sample(avoidd_babysitters[[pedigree_pup_dam_2$id[j]]],1)    
  }
  
  pedigree_pup_dam_2$potsire<- simrel
  
  
  rel<-c()
  for (h in 1:nrow(pedigree_pup_dam_2)){
    rel[h]<- BMRP[pedigree_pup_dam_2$dam[h],pedigree_pup_dam_2$potsire[h]] 
  }
  
  random_mating[i]<-mean(rel)
  
}

write.csv(random_mating, "randomisation_for_avoid_babysitter.csv")

mean(random_mating)    #0.2010869     


hist(random_mating,xlab="mean male-female relatedness",xlim=c(0.13,0.24),main="",col="grey16",border="white")


arrows(0.159765,400,0.159765 ,100,col = "red", length = 0.17, angle = 30)
text(0.159765,600,labels = "Observed value", cex = 0.80)
text(0.159765,500,labels = "(0.160)", cex = 0.80)

##################################################################################################################

# PUTTING TWO HISTOGRAMS TOGETHER 

#import null randomisation

null<-null_randomisation_for_avoid_babysitter$x

#import litter mate excluion randomisation

avoid_babysitter<- randomisation_for_avoid_babysitter$x

hist(null,xlab="Mean male-female relatedness",xlim=c(0.135,0.25), ylim = c(0,2700), main="",col= "gray70", border = "black") 
hist(avoid_babysitter,xlab="Mean male-female relatedness",xlim=c(0.125,0.25), ylim = c(0,2700), main="",col= rgb(1,1,0,0.4), border= "black",add=T)

legend(0.215,2500, c ("Random mating", "Avoid babysitters"), col=c("gray70", "khaki1"), cex = 0.65, lwd=10)

arrows(0.159765,700,0.159765 ,250,col = "red", length = 0.17, angle = 30)
text(0.155,950,labels = "Observed value", cex = 0.80)
text(0.156,820,labels = "(0.160)", cex = 0.80)

######################################################################################################################

#P.VALUE AND CI FOR AVOID BABYSITTERS 

dist<- randomisation_for_avoid_babysitter$x
value<- 0.159765


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

#CI IS 0.1832942 0.2193573
#P.VALUE IS 0

######################################################################################################################

#CI AND P.VALUE FOR NULL 

value<- 0.159765
dist<- null_randomisation_for_avoid_babysitter$x


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
#[1] 0.1662181 0.2028168

#$`P-value`
#[1] 0.0078

#################################################################
null<- null_randomisation_for_avoid_babysitter$x
avoid<- randomisation_for_avoid_babysitter$x

ks.test(avoid,null)

#Two-sample Kolmogorov-Smirnov test

#data:  avoid and null
#D = 0.6408, p-value < 2.2e-16
#alternative hypothesis: two-sided

