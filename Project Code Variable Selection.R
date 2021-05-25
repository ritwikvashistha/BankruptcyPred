mean_data=read.csv(file.choose())
knn_data=read.csv(file.choose())
mice_data=read.csv(file.choose())

library(perturb)

vardecomp<- function(data) {
  vardec<- colldiag(data,scale=FALSE,add.intercept = FALSE)
  pi<-vardec$pi
  cond<-vardec$condindx
  varmatrix<-data.frame(cond,pi)
  return(varmatrix)
}

mean_decomp<-vardecomp(mean_data)
knn_decomp<-vardecomp(knn_data)
mice_decomp<-vardecomp(mice_data)

write.csv(mean_decomp,file="C:/Users/rick7/Desktop/Mean_Decomp.csv")
write.csv(knn_decomp,file="C:/Users/rick7/Desktop/KNN_Decomp.csv")
write.csv(mice_decomp,file="C:/Users/rick7/Desktop/MICE_Decomp.csv")

var_selection<-function(data) {
  model<-glm(response ~.,family=binomial,data=data)
  model1<-glm(response ~ 1,family=binomial,data=data)
  bothways  = step(model1, list(lower=formula(model1),upper=formula(model)),direction="both",trace=0)
  return(bothways)
}

mean_model<-var_selection(mean_data)
knn_model<-var_selection(knn_data)
mice_model<-var_selection(mice_data)


