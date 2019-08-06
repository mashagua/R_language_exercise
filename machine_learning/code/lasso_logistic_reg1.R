library(xlsx)
library(glmnet)
library(rms)
library(Hmisc)
options(scipen=200)
#library(rmda)

#dsimple<- decision_curve(chdfate~scl,data = Data, family = binomial(link ='logit'),thresholds= seq(0,1, by = 0.01), confidence.intervals =0.95,study.design = 'case-control',population.prevalence = 0.3)
rawdata<- read.xlsx('F:\\993498039\\FileRecv\\MobileFile\\7excel-233.xlsx',sheetIndex = 1)
#��������������¸�ֵ
header.true <- function(df) {
  #ת��list�ٸ�ֵ
  colnames(df) <- as.character(unlist(df[1,]))
  #ȥ����һ��
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
#����
x <- model.matrix(label1~., data3)[,-1]
# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.(��׼��֮���)
glmmod <- glmnet(x, y, alpha=1, family="multinomial",standardize = TRUE)
#ÿһ�д�����һ��ģ�͡��� Df �����ɶȣ������˷��������ģ�����ϵ���ĸ����� �� %Dev ��������ģ�ͽ��͵Ĳв�ı�������������ģ����˵����ģ����ϵ� R^2��R-squred���� ���� 0 �� 1 ֮�䣬Խ�ӽ� 1 ˵��ģ�͵ı���Խ�ã������ 0��˵��ģ�͵�Ԥ����������ֱ�Ӱ�������ľ�ֵ��ΪԤ��ֵ������Ч��
#ѡȡĳһ��
a1 <- coef(glmmod, s=c(glmmod$lambda[24],0.1))
#�鿴��Щģ������α仯��
plot(glmmod, xvar="lambda", label=TRUE)
###��ȡ��Ϊ0��������ϵ��
coef.df<- data.frame(coef.name=a1[[3]]@Dimnames[1][[1]][a1[[3]]@i+1],coefficient=a1[[3]]@x)
#��Ϊ������֤��ʱ����һЩ������������8�����Իᱨ���棬��Ϊ���4ֻ��9��
cvfit = cv.glmnet(x, y, family = "multinomial", type.measure = "class")

tmp_coeffs <- coef(cvfit, s = "lambda.min")
cv_min_lambda<- data.frame(name = tmp_coeffs[[3]]@Dimnames[1][[1]][tmp_coeffs[[3]]@i+1], coefficient = tmp_coeffs[[3]]@x)
