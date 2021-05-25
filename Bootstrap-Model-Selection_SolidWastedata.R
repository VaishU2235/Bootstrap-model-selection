rm(list=ls())
library(mvtnorm)
set.seed(12345)
data("Shao", package = "bestglm")

norm_bootstrap <-function(X,Y,Our_model_alpha,m=40,B=100){
  no_of_models=dim(Our_model_alpha)[1]
  LOSS = matrix(NA, nrow = 1, ncol = no_of_models)
  for (j in 1:no_of_models) {
    #Calculating loss for each models
    loss=c()
    for (boot in 1:B) {
      X_alpha=X[,which(Our_model_alpha[j,]==1)]
      i=sample(x=1:N,size = m, replace = TRUE)
      boot_x=X_alpha[i,]
      boot_y=Y[i]
      beeta_star=solve(t(boot_x)%*%boot_x)%*%t(boot_x)%*%boot_y
      loss[boot]=(t(Y-(X_alpha%*%beeta_star))%*%(Y-(X_alpha%*%beeta_star)))/N
    }
    LOSS[j]= mean(loss)
  }
  return(which.min(LOSS))#Here we returned the model having least loss
}

norm_bootstrap_m40 <-function(X,Y,Our_model_alpha,B=100){
  #We should calculate BIC when m=40 and our loss is different here.
  no_of_models=dim(Our_model_alpha)[1]
  LOSS = matrix(NA, nrow = 1, ncol = no_of_models)
  BICLOSS = matrix(NA, nrow = 1, ncol = no_of_models)
  for (j in 1:no_of_models) {
    loss=BICloss=c()
    X_alpha=X[,which(Our_model_alpha[j,]==1)]
    out <- lm(Y ~ X_alpha)
    BICLOSS[j] = BIC(out)
    Beta_alpha=solve(t(X_alpha)%*%X_alpha)%*%t(X_alpha)%*%Y
    model_loss=(t(Y-(X_alpha%*%Beta_alpha))%*%(Y-(X_alpha%*%Beta_alpha)))/N
    for (boot in 1:B) {
      i=sample(x=1:N,size = m, replace = TRUE)
      boot_x=X_alpha[i,]
      boot_y=Y[i]
      beeta_star=solve(t(boot_x)%*%boot_x)%*%t(boot_x)%*%boot_y
      boot_loss_1=(t(Y-(X_alpha%*%beeta_star))%*%(Y-(X_alpha%*%beeta_star)))/N
      boot_loss_2=(t(boot_y-(boot_x%*%beeta_star))%*%(boot_y-(boot_x%*%beeta_star)))/N
      loss[boot]=boot_loss_1-boot_loss_2
    }
    LOSS[j]= model_loss+mean(loss)
  }
  return(c(which.min(LOSS),which.min(BICLOSS)))#Here we returned the model having least loss and BIC Loss
}

#Initializing variables:
N=40;P=4;M=c(40,30,25,20,15)#Change the different bootstrap sample size we want to check here.
X=matrix(NA,N,P+1)
X[,1]=1

TrueBeta=matrix(c(2,0,0,4,0),P+1,1)#Change True Beta Here.

# Here, X is fixed. If X is random, it is better to move bottom two lines inside the MC loop.
X[,(2:(P+1))] = as.matrix.data.frame(Shao)
Mu=X%*%TrueBeta

Our_model_alpha=t(matrix(c(1,0,0,1,0,
                           1,1,0,1,0,
                           1,0,1,1,0,
                           1,0,0,1,1,
                           1,1,1,1,0,
                           1,1,0,1,1,
                           1,0,1,1,1,
                           1,1,1,1,1),P+1,8))#Here, 8 is no. of models. If you are changing it, put no. of models in place of 8.

MCTrials=1000#No. of trials for estimating probability of selection of a model.
B=100# No. of trials for estimating loss.
Result=matrix(0,dim(Our_model_alpha)[1],length(M)+1)
Winner=matrix(0,MCTrials,2)#one winner for loss one winner for BIC
proportion=BIC_Prop=c()

#MC Loop
for (m in M) {
  print(paste("Now calculating for M=",m))
  for (mc in 1:MCTrials) {
    eps=rnorm(N)
    Y=Mu+eps
    if(m!=40){
      Winner[mc,]=c(norm_bootstrap(X,Y,Our_model_alpha,m,B),0)
    }else{
      Winner[mc,]=norm_bootstrap_m40(X,Y,Our_model_alpha,B)
    }
  }
  if(m==40){
    BIC_Prop=table(Winner[,2])/MCTrials
  }
  proportion=table(Winner[,1])/MCTrials
  for(j in 1:dim(Our_model_alpha)[1]){
    Result[j,which(M==m)]=proportion[as.character(j)]#as char. bcz table() gives output indexed by charecters
    if(m==40){
      Result[j,length(M)+1]=BIC_Prop[as.character(j)]#Stores BIC in last position
    }
  }
}

df=as.data.frame(Result)
df[is.na(df)]=0
write.csv(df, "1st with 1000&BICNEW_SIM.csv", row.names=FALSE, quote=FALSE)
