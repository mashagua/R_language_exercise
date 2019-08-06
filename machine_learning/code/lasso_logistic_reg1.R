library(xlsx)
library(glmnet)
library(rms)
library(Hmisc)
options(scipen=200)
#library(rmda)

#dsimple<- decision_curve(chdfate~scl,data = Data, family = binomial(link ='logit'),thresholds= seq(0,1, by = 0.01), confidence.intervals =0.95,study.design = 'case-control',population.prevalence = 0.3)
rawdata<- read.xlsx('F:\\993498039\\FileRecv\\MobileFile\\7excel-233.xlsx',sheetIndex = 1)
#给表格的列名重新赋值
header.true <- function(df) {
  #转成list再赋值
  colnames(df) <- as.character(unlist(df[1,]))
  #去掉第一行
  df[-1,]
}
dim_rawdata<- dim(rawdata)
data1<- rawdata[,1:6]
data2 <- rawdata[,c(1,7:dim_rawdata[2])]
data2 <- header.true(data2)
data1 <- header.true(data1)
cols_remove <- c('Teta1','Teta2','Teta3','Teta4','Sigma','GrMean','GrVariance','GrSkewness','GrKurtosis','GrNonZeros')
data3<- data2[, !(colnames(data2) %in% cols_remove)]
data3[,-1]  <- lapply(data3[,-1], as.numeric)

y <- as.factor(as.integer(data3$lable1))
#列名
x <- model.matrix(label1~., data3)[,-1]
# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.(标准化之后的)
glmmod <- glmnet(x, y, alpha=1, family="multinomial",standardize = TRUE)
#每一行代表了一个模型。列 Df 是自由度，代表了非零的线性模型拟合系数的个数。 列 %Dev 代表了由模型解释的残差的比例，对于线性模型来说就是模型拟合的 R^2（R-squred）。 它在 0 和 1 之间，越接近 1 说明模型的表现越好，如果是 0，说明模型的预测结果还不如直接把因变量的均值作为预测值来的有效。
#选取某一个
a1 <- coef(glmmod, s=c(glmmod$lambda[24],0.1))
#查看这些模型是如何变化的
plot(glmmod, xvar="lambda", label=TRUE)
###提取不为0的特征的系数
coef.df<- data.frame(coef.name=a1[[3]]@Dimnames[1][[1]][a1[[3]]@i+1],coefficient=a1[[3]]@x)
#因为交叉验证的时候有一些样本个数少于8，所以会报警告，因为类别4只有9个
cvfit = cv.glmnet(x, y, family = "multinomial", type.measure = "class")

tmp_coeffs <- coef(cvfit, s = "lambda.min")
cv_min_lambda<- data.frame(name = tmp_coeffs[[3]]@Dimnames[1][[1]][tmp_coeffs[[3]]@i+1], coefficient = tmp_coeffs[[3]]@x)

