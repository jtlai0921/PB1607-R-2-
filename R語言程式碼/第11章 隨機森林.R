install.packages ( "randomForest" )
library(randomForest)

###importance函數範例
set.seed(4)
data(mtcars)
mtcars.rf=randomForest(mpg~.,data=mtcars,ntree=1000,importance=TRUE)
importance(mtcars.rf)	
importance(mtcars.rf, type=1)

###MDSplot函數範例
set.seed(1)	# 設定產生隨機數的初值
data(iris)	# 呼叫資料集iris
iris.rf=randomForest(Species ~ ., iris, proximity=TRUE)# 基於資料集iris建立隨機森林模型
MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))# 繪制圖形

###rfImpute函數範例
data(iris)	# 呼叫資料集iris
iris.na=iris	# 產生需要進行處理的資料集
iris.na[75,2]=NA;iris.na[125,3]=NA;	  # 在第75號樣本和第125號樣本中設定缺失值
set.seed(111)				# 設定隨機數產生器初值
iris.imputed=rfImpute(Species ~ .,data=iris.na)	# 對資料集iris.na進行插值

###treesize函數範例
iris.rf<- randomForest(Species ~ ., iris)	# 利用資料iris建構關聯隨機森林模型
hist(treesize(iris.rf))			# 繪制對應的柱狀圖

###模型可視化範例
data(airquality)		# 呼叫資料集airquality
set.seed(131)			# 設定隨機數產生器初值
ozone.rf=randomForest(Ozone~.,data=airquality,mtry=3,importance=TRUE,na.action=na.omit)# 建立隨機森林回歸模型
plot(ozone.rf)			# 繪制關聯圖形


###實際案例
wine=read.table("d:\\wine.txt") # 本文預設資料以記事本格式儲存於電腦D碟中
names(wine)=c("fixed acidity","volatile acidity","citric acid","residual sugar","chlorides","free sulfur dioxide","total sulfur dioxide","density","PH","sulphates","alcohol","quality")	# 為資料集wine各個變數命名
summary (wine)                # 取得wine資料集的概括訊息


cha=0	# 設定中間變數對處理後的向量進行臨時儲存
for(i in 1:4898) # 針對每一個樣本進行調整
{
	if(wine[i,12]>6)
	{
		cha[i]="good"	# 將品質大於6的樣本品質定義為“good”
	}
	else if(wine[i,12]>5)
	{
		cha[i]="mid"	# 將品質大於5卻不大於6的樣本品質定義為“mid”
	}
	else
	{
		cha[i]="bad"	# 將品質不大於5的樣本品質定義為“bad”
	}
}
wine[,12]=factor(cha)	# 將字元型變數轉化為含有因子的變數並複製給資料集wine
summary(wine$quality)
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")

set.seed(71)			# 設定隨機數產生器初值
samp=sample(1:4898,3000)	# 從全部資料集中抽取3000個樣本作為訓練集
set.seed(111)			# 設定隨機數產生器初值
wine.rf=randomForest(quality~.,data=wine,importance=TRUE,proximity=TRUE,ntree=500,subset=samp)	 # 建構決策樹為500棵的隨機森林模型

x=wine[-samp,1:11]		# 利用建構模型剩下的樣本作為測試集
pred=predict(wine.rf,x)		# 根據模型wine.rf對x資料進行預測
pred[sample(1:1898,8)]		# 隨機挑選8個預測結果進行展示

###尋找模型最佳節點變數數
n=ncol(wine)-1			# 計算資料集中自變數個數
rate=1				# 設定模型誤判率向量初值
for(i in 8:n)			# 依次一個一個增加節點所選變數個數
{
	set.seed(222)		# 設定隨機數產生器的初值
	model=randomForest(quality~.,data=wine,mtry=i,importance=TRUE,ntree=1000)	# 建立隨機森林模型
	rate[i]=mean(model$err.rate)				# 計算基於OOB資料的模型誤判率均值
	print(model)		# 展示模型簡要訊息
}
rate			

set.seed(222)		# 設定隨機數產生器初值
model=randomForest(quality~.,data=wine,mtry=1,importance=TRUE,ntree=1000) # 建構隨機森林模型
plot(model,col=1:1)
legend(800,0.215,"mid",cex=.9,bty="n")			#為圖形加入圖例
legend(800,0.28,"bad",cex=.9,bty="n")			#為圖形加入圖例
legend(800,0.37,"good",cex=.9,bty="n")			#為圖形加入圖例
legend(800,0.245,"total",cex=0.9,bty="n")		#為圖形加入圖例


set.seed(222)		# 設定隨機數產生器初值
model=randomForest(quality~.,data=wine,mtry=1,proximity=TRUE,importance=TRUE,ntree=400)# 建立隨機森林模型
hist(treesize(model))						 # 展示隨機森林模型中每顆決策樹的節點數
importance(model)			 # 展示在隨機森林模型中各個變數對模型預測能力的影響
MDSplot(model,wine$quality, palette=rep(1, 3), pch=as.numeric(wine$quality))		 # 展示資料集在二維的情況下個類別別的實際分佈情況