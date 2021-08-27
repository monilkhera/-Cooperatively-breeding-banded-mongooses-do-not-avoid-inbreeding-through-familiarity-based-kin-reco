#convert the BMRP into a 3 column matrix -  name one column dam and one column sire

BMRP<- as.matrix(BMRP_relatedness_July_2016)
View(BMRP)
colnames(BMRP)<-rownames(BMRP)

View(BMRP)

rel_dataframe<- as.data.frame(as.table(BMRP))

#create a new dataframe with the 351 pups names on one column and then add the corresponding dams
#then change the names of the sire list to the dams name instead of the pups 

dam_names<- data.frame(pups = names(sire_list))

pedigree<- pedigree_post_exclusion

dam_names$dam<- pedigree[match(dam_names$pups, pedigree$id),]$dam

names(sire_list)<- dam_names$dam

dam_list<- sire_list

##########################################################################################################################################################################################

relatedness_list <- list()
for (i in 1:length(sire_list)){
  relatedness_list[[i]]<- rel_dataframe$Freq[which(match(as.character(rel_dataframe$Var1), sire_list[[i]]) & match(as.character(rel_dataframe$Var2),names(sire_list)[[i]]))]
}

#rename sire list to dam_sire_list and then upload the initial sire list again so you have the pups name and then name the relatedness_list

dam_name_list<- sire_list

#now upload original sire list 

names(relatedness_list)<- names(sire_list)

save(relatedness_list, file = "relatedness_list.RData")

###############################################################################################################################################################################################

random_mating<- c()
for (i in 1:10000){
  
  simrel<- c()
  for(j in 1:length(relatedness_list)){
    simrel[j]<-sample(relatedness_list[[j]],1, prob = max(relatedness_list[[j]])+0.25 - relatedness_list[[j]])
  }
  
  random_mating[i]<-mean(simrel)
  
}

relatedness_mating<- random_mating

write.csv(relatedness_mating, "relatedness_randomisation.csv")

mean(relatedness_randomisation$x) #0.1360508

######################################################################################################################################################################

#HISTOGRAMS

relatedness<-relatedness_randomisation$x
null<- true_initial_randomisation$x

hist(null,xlab="Mean male-female relatedness",xlim=c(0.10,0.22), ylim = c(0,2900), main="",col= "gray70", border = "black") 
hist(relatedness,xlab="Mean male-female relatedness",xlim=c(0.10,0.22), ylim = c(0,2700), main="",col= rgb(1,1,0,0.4), border= "black",add=T)

legend(0.190,2850, c ("Random mating", "Less related males"), col=c("gray70", "khaki1"), cex = 0.65, lwd=10)

arrows(0.1438649,2250,0.1442759 ,1800,col = "red", length = 0.17, angle = 30)
text(0.152,2400,labels = "Observed value", cex = 0.80)
text(0.1475,2300,labels = "(0.144)", cex = 0.80)

##################################################################################################################################################

#p.value and CI value

dist<- relatedness_randomisation$x
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

p.rand(dist,value)

#$`95% CI`
#[1] 0.1232471 0.1495850

#$`P-value`
#[1] 0.2518


#> p.rand(dist,value,type = "is.higher")
#$`95% CI`
#[1] 0.1232471 0.1495850

#$`P-value`
#[1] 0.1259

#> p.rand(dist,value,type = "is.lower")
#$`95% CI`
#[1] 0.1232471 0.1495850

#$`P-value`
#[1] 0.8741

> 

