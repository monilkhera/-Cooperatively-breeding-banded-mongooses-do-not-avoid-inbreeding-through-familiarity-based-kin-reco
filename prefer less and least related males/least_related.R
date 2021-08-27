#upload relatedness_list

least_related <- list()
for (i in 1:length(relatedness_list)){
  least_related[[i]]<- relatedness_list[[i]][which(relatedness_list[[i]] == min(relatedness_list[[i]]))]
}

names(least_related)<- names(relatedness_list)

least_rel<- lapply(least_related, '[[',1)

dataframe_least_rel<- do.call(rbind.data.frame, least_rel)

dataframe_least_rel$relatednes<- dataframe_least_rel$c.0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..0..

mean(dataframe_least_rel$relatednes) #0.02114633

write.csv(dataframe_least_rel, "least_related.csv")

#######################################################################################################################################################################

#GRAPH

relatedness<-relatedness_randomisation$x
null<- true_initial_randomisation$x

hist(null,xlab="Mean male-female relatedness",xlim=c(0,0.22), ylim = c(0,2900), main="",col= "gray70", border = "black") 
hist(relatedness,xlab="Mean male-female relatedness",xlim=c(0.10,0.22), ylim = c(0,2700), main="",col= rgb(1,1,0,0.4), border= "black",add=T)

legend(0.187,2850, c ("Random mating", "Less related males"), col=c("gray70", "khaki1"), cex = 0.65, lwd=10)

arrows(0.1438649,2250,0.1442759 ,1800,col = "red", length = 0.17, angle = 30)
text(0.1538,2400,labels = "Observed value", cex = 0.80)
text(0.1475,2300,labels = "(0.144)", cex = 0.80)

arrows(0.02114633,450,0.02114633 ,0,col = "blue", length = 0.17, angle = 30)
text(0.02114633,600,labels = "Perfect mate choice", cex = 0.80)
text(0.02114633,510,labels = "(0.021)", cex = 0.80)

############################################################################################################################################################################


dist<- relatedness_randomisation$x
value<- 0.02114633

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
#[1] 0

