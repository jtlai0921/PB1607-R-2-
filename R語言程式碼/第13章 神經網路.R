###第一種建模格式
wine=read.table("d:\\wine.txt") 	# 本文預設資料以記事本格式儲存於電腦D碟中
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")															# 為每一個變數命名
set.seed(71)
samp=sample(1:4898,3000) 		# 從總樣本集中抽取3000個樣本作為訓練集
wine[samp,1:11]=scale01(wine[samp,])	# 對樣本進行預先處理
r=1/max(abs(wine[samp,1:11]))		# 確定參數rang的變化範圍
set.seed(101)
model1=nnet(quality~.,data=wine,subset=samp,size=4,rang=r,decay=5e-4,maxit=200)												# 建立神經網路模型
###第二種建模格式
x=subset(wine,select=-quality)		# 分析wine資料中除quality列以外的資料作為自變數
y=wine[,12]				# 分析wine資料中的quality列資料作為響應變數
y=class.ind(y)				# 對響應變數進行預先處理，將其變為類別指標矩陣
set.seed(101)
model2=nnet(x,y,decay=5e-4,maxit=200,size=4,rang=r)   	# 建立神經網路模型

###針對第一種格式進行預測
x=wine[,1:11]				# 確認需要進行預測的樣本特征矩陣
pred=predict(model1,x,type=”class”)	# 根據模型model1對xt資料進行預測
set.seed(110)
pred[sample(1:4898,8)]			# 隨機挑選8個預測結果進行展示

###針對第二種格式進行預測
xt=wine[,1:11]				# 確認需要進行預測的樣本特征矩陣
pred=predict(model2,xt)			# 根據模型model2對xt資料進行預測
dim(pred)				# 檢視預測結果的維度
pred[sample(1:4898,4),]			# 隨機挑選4個預測結果進行展示
name=c("bad","good","mid")		# 為三個類別別確定名稱
prednew=max.col(pred)			# 確定每行中最大值所在列
prednewn=name[prednew]			# 根據預測結果將其變為相對應的類別別名稱
set.seed(201)
prednewn[sample(1:4898,8)]		# 隨機挑選8個預測結果進行展示
true=max.col(y)				# 確定真實值的每行中最大值所在列
table(true,prednewn)			# 模型預測精度展示


###nnet函數使用過程中特別注意
model1=nnet(x,y,rang=1/max(abs(x)),size=4,maxit=500,decay=5e-4)   # 建立模型model1
model2=nnet(x,y,rang=1/max(abs(x)),size=4,maxit=500,decay=5e-4)   # 建立模型model2
name=c("setosa","versicolor","virginica")			# 為三個類別別確定名稱
 pred1=name[max.col(predict(model1,x))]
# 利用第二種模型的預測方法對模型model1進行預測
pred2=name[max.col(predict(model2,x))]
# 利用第二種模型的預測方法對模型model2進行預測
 table(Species,pred1)						# 模型model1預測精度展示


###實際建模動作
###確定隱藏層節點數
wine=read.table("d:\\wine.txt") # 本文預設資料以記事本格式儲存於電腦D碟中
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")															# 為每一個變數命名
set.seed(71)
wine=wine[sample(1:4898,3000),]
nrow.wine=dim(wine)[1]

###原始資料歸一化程式
scale01=function(x)
{
	ncol=dim(x)[2]-1
	nrow=dim(x)[1]
	new=matrix(0,nrow,ncol)
	for(i in 1:ncol)
	{
		max=max(x[,i])
		min=min(x[,i])
		for(j in 1:nrow)
		{
			new[j,i]=(x[j,i]-min)/(max-min)
		}
	}
	new
}
cha=0	# 設定中間變數對處理後的向量進行臨時儲存
for(i in 1: nrow.wine) # 針對每一個樣本進行調整
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
set.seed(444)
samp=sample(1:nrow.wine, nrow.wine*0.7) 	# 從總樣本集中抽取70%的樣本作為訓練集
wine[samp,1:11]=scale01(wine[samp,])		# 對訓練集樣本進行預先處理
wine[-samp,1:11]=scale01(wine[-samp,])		# 對測試集樣本進行預先處理
r=1/max(abs(wine[samp,1:11]))			# 確定參數rang的變化範圍
n=length(samp)
err1=0
err2=0
for(i in 1:17)
{	
	set.seed(111)
	model=nnet(quality~.,data=wine,maxit=400,rang=r,size=i,subset=samp,decay=5e-4)
	err1[i]=sum(predict(model,wine[samp,1:11],type='class')!=wine[samp,12])/n
	err2[i]=sum(predict(model,wine[-samp,1:11],type='class')!=wine[-samp,12])/(nrow.wine -n)
}
plot(1:17,err1,'l',col=1,lty=1,ylab="模型誤判率",xlab="隱藏層節點個數",ylim=c(min(min(err1),min(err2)),max(max(err1),max(err2))))
lines(1:17,err2,col=1,lty=3)
points(1:17,err1,col=1,pch="+")
points(1:17,err2,col=1,pch="o")
legend(1,0.53,"測試集誤判率",bty="n",cex=1.5)
legend(1,0.35,"訓練集誤判率",bty="n",cex=1.5)

###確定訓練周期
err11=0
err12=0
for(i in 1:500)
{
	set.seed(111)	
	model=nnet(quality~.,data=wine,maxit=i,rang=r,size=3,subset=samp)
	err11[i]=sum(predict(model,wine[samp,1:11],type='class')!=wine[samp,12])/n
	err12[i]=sum(predict(model,wine[-samp,1:11],type='class')!=wine[-samp,12])/(nrow.wine-n)
}

plot(1:length(err11),err11,'l',ylab="模型誤判率",xlab="訓練周期",col=1,ylim=c(min(min(err11),min(err12)),max(max(err11),max(err12))))
lines(1:length(err11),err12,col=1,lty=3)
legend(250,0.47,"測試集誤判率",bty="n",cex=1.2)
legend(250,0.425,"訓練集誤判率",bty="n",cex=1.2)

###最終模型
set.seed(111)
model=nnet(quality~.,data=wine,maxit=300,rang=r,size=3,subset=samp)
x=wine[-samp,1:11]				# 確認需要進行預測的樣本特征矩陣
pred=predict(model,x,type="class")		# 根據模型model1對xt資料進行預測
table(wine[-samp,12],pred)