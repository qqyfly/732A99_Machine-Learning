#1
df=read.csv("olive.csv")
data1=data.frame(scale(df[,3:10]))

res=prcomp(data1)
lambda=res$sdev^2
#eigenvalues
lambda
#proportion of variation
sprintf("%2.3f",cumsum(lambda)/sum(lambda)*100)
res$rotation[,1]#oleic has largest negatic

#2

df1=data.frame(res$x[,1:3], region=as.factor(df$Region))
n=dim(df1)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=df1[id,]
test=df1[-id,]

library(nnet)
m1=multinom(region~., data=train)

misclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

misclass(train$region, predict(m1))
misclass(test$region, predict(m1, newdata=test))
print(m1)
coef(m1)[1,]-coef(m1)[2,]


#3

n=dim(df)[1]
set.seed(12345)
df1=df
df1$Region=as.factor(ifelse(df$Region==1,"South","Otherwise"))
id=sample(1:n, floor(n*0.5))
train=df1[id,-2]
test=df1[-id,-2]

covariates=as.matrix(train[,2:9])
response=train[[1]]

library(glmnet)
set.seed(12345)
model=cv.glmnet(as.matrix(covariates), response, alpha=0,family="binomial")
model$lambda.min
plot(model)
coef(model, s="lambda.min")

m1=glmnet(as.matrix(covariates), response, alpha=0,family="binomial", lambda=model$lambda.min)


ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=rep(0,m)
  FPR=rep(0,m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    
    if(nrow(t)==1 && rownames(t)[1]=="TRUE"){
      TPR[i]=1
      FPR[i]=1
    } else if ( nrow(t)==1 && rownames(t)[1]=="FALSE"){
      TPR[i]=0
      FPR[i]=0
    } else{
      TPR[i]=t[2,2]/(t[2,2]+t[1,2])
      FPR[i]=t[2,1]/(t[2,1]+t[1,1])
    }
    
    
  }
  return (list(TPR=TPR,FPR=FPR))
}

ps=seq(0.05,0.95,0.05)

Yt1=predict(m1, newx=as.matrix(test[,2:9]), type="response")

res1=ROC(test$Region,Yt1,ps)
plot(res1$FPR,res1$TPR, type="l", col="red")

