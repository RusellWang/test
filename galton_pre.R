##Galton934对孩子以及父亲、母亲身高数据集预处理过程 chengwang
rm(list=ls()) ###初始化编程环境
set.seed(123) ##设置随机种子
library(caret)
library(HistData)  
data(GaltonFamilies)
galton0=GaltonFamilies  
galton1=galton0
galton1[,c(2,3,8)]=2.54*galton0[,c(2,3,8)] ##原始数据中的英寸转为厘米

##女性数据乘以1.08消除性别影响
galton1[,3]=1.08*galton1[,3];
galton1[galton1[,7]==c("female"),8]=1.08*galton1[galton1[,7]==c("female"),8] 
fa <-galton1[,2]
mo <-galton1[,3]
ch <-galton1[,8]


fun <- function(i) {
  x1<-galton1[-i,2]
  x2<-galton1[-i,3]
  y <-galton1[-i,8]
  lm_model <- lm(y ~ x1+x2+1)
  beta <- coefficients(lm_model)
  x <-cbind(1,galton1[i,2],galton1[i,3])
  y_hat <- x%*%beta
  error <- as.numeric(galton1[i,8]-y_hat)
  return(error)
}
eseq <- rep(0,934)
sum <-0
sum2 <-0
for (i in 1:934){
  eseq[i]<-fun(i)
  sum2 <- sum2+eseq[i]^2
}
print(sum2/934)

#加入交叉项对比
boost <- function(i) {
  x1<-galton1[-i,2]
  x2<-galton1[-i,3]
  x3<-galton1[-i,4]
  y <-galton1[-i,8]
  lm_model <- lm(y ~ x1+x2+1)
  beta <- coefficients(lm_model)
  x <-cbind(1,galton1[i,2],galton1[i,3])
  y_hat <- x%*%beta
  error <- as.numeric(galton1[i,8]-y_hat)
  return(error)
}
for (i in 1:934){
  sum <- sum+boost(i)^2
}
print(sum/934)#结果近似相同


#caret包
ctrl <- trainControl(method = "LOOCV")
data <-cbind(ch,fa,mo)
cv_results <- train(ch ~., data = data, method = "lm", trControl = ctrl)

# 输出交叉验证结果
print(cv_results)

