install.packages("e1071")				# 下載安裝e1071軟體包
library(e1071)						# 載入e1071軟體包

###第一種格式建立模型
data(iris)						# 取得資料集iris
model=svm(Species~.,data=iris)				# 建立svm模型
###第二種格式建立模型
x=iris[,-5]						# 分析iris資料中除第5列以外的資料作為特征變數
y=iris[,5]						# 分析iris資料中的第5列資料作為結果變數
model=svm(x,y,kernel ="radial",gamma =if(is.vector(x)) 1 else 1/ncol(x))   # 建立svm模型

###對模型進行預測
x=iris[,1:4]									  # 確認需要進行預測的樣本特征矩陣
pred=predict(model,x)								# 根據模型model對x資料進行預測
pred[sample(1:150,8)]								 # 隨機挑選8個預測結果進行展示
table(pred,y)								# 模型預測精度展示

###實際建模過程中完整動作
attach(iris)					# 將資料iris按列單獨確認為向量
x=subset(iris,select=-Species)		# 確定特征變數為資料iris中除去Species的其他項
y=Species				# 確定結果變數為資料iris中的Species項
type=c("C-classification","nu-classification","one-classification")# 確定將要適用的分類別模式
kernel=c("linear","polynomial","radial","sigmoid")				#確定將要適用的核函數
pred=array(0,dim=c(150,3,4))		#起始化預測結果矩陣的3D長度分別為150，3，4
accuracy=matrix(0,3,4)						#起始化模型精准度矩陣的兩維分別為3，4
yy=as.integer(y)					#為方便模型精度計算，將結果變數數量化為1，2，3
for(i in 1:3)								#確認i影響的維度代表分類別模式
{
	for(j in 1:4)							#確認j影響的維度代表核函數
	{
		pred[,i,j]=predict(svm(x,y,type=type[i],kernel=kernel[j]),x)   #對每一模型進行預測
		if(i>2)
		{
			accuracy[i,j]=sum(pred[,i,j]!=1)
		}
		else
		{
			accuracy[i,j]=sum(pred[,i,j]!=yy)
		}
	}
}
dimnames(accuracy)=list(type,kernel)					#確定模型精度變數的列名和行名
table(pred[,1,3],y)							# 模型預測精度展示

###模型可視化
plot(cmdscale(dist(iris[,-5])),col=c("lightgray","black","gray")[as.integer(iris[,5])],pch= c("o","+")[1:150 %in% model$index + 1])                # 繪制模型分類別散點圖
legend(2,-0.8,c("setosa","versicolor","virginica"),col=c("lightgray","black","gray"),lty=1)		# 標示圖例

data(iris)										#讀入資料iris
model=svm(Species~., data = iris)							#利用公式格式建立模型
plot(model,iris,Petal.Width~Petal.Length,fill=FALSE,symbolPalette=c("lightgray","black","grey"),svSymbol="+")
									#繪制模型類別別關於花萼寬度和長度的分類別情況
legend(1,2.5,c("setosa","versicolor","virginica"),col=c("lightgray","black","gray"),lty=1)		#標示圖例

###模型進一步改善
wts=c(1,1,1)							# 確定模型各個類別別的比例為1：1：1
names(wts)=c("setosa","versicolor","virginica")			#確定各個比例對應的類別別
model1=svm(x,y,class.weights=wts)				#建立模型
wts=c(1,100,100)						# 確定模型各個類別別的比例為1：100：100
names(wts)=c("setosa","versicolor","virginica")			#確定各個比例對應的類別別
model2=svm(x,y,class.weights=wts)				#建立模型
pred2=predict(model2,x)						#根據模型進行預測
table(pred2,y)							#展示預測結果
wts=c(1,500,500)						# 確定模型各個類別別的比例為1：500：500
names(wts)=c("setosa","versicolor","virginica")			#確定各個比例對應的類別別
model3=svm(x,y,class.weights=wts)				#建立模型
pred3=predict(model3,x)						#根據模型進行預測
table(pred3,y)				 			#展示預測結果

