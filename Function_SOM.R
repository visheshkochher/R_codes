som.auto <- function(dimX, dimY){
  
  set.seed(101)
  som.export <- trainSOM(x.data=sombrero_data, dimension=c(dimX, dimY), scaling="none", radius.type="letremy")
  summary(som.export)
  save(som.export, file = "somexport.rda")
  pdf("som.pdf",width=15,height=9)
  plot(som.export, what="obs", type="radar", print.title=TRUE, key.loc=c(-0.5,3), mar=c(0,10,2,0))
  dev.off()
}


sombrero_data <- df
som.auto(3,2)
load("~/Desktop/R/CLV Development/somexport.rda")
som.classes <- as.data.frame(som.export$clustering)
som.classes <- plyr::rename(som.classes, c(`som.export$clustering` = "cluster"))

#Split customers into 5 groups for each broker
df <- cbind(df, som.classes)
