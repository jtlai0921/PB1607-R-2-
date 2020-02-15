x = c ( 1, 2, 3, 4 )                                
x                                                               
class ( x )                                             

x1 = as.integer ( x )                                   
class ( x1 )                                              # 顯示向量x1的資料型態

x = c ( 1, 2, 3, 4 )                                  # 建構元素依次為1,2,3,4的向量x
x==2                                               # 判斷向量x中等於2的元素
! ( x<2 )                                         # 判斷向量x中大於等於2的元素
which ( x<2 )                                         # 選取向量x中小於2的元素
is.logical ( x )                                      # 判斷向量x是否為邏輯性資料

y = c ( "I", "love", "R" )            # 建構元素依次為字串“I”，“love”，“R”的向量y
y                                                                # 輸出y的值
class ( y )                                                # 顯示向量y的資料型態
length ( y )                                       # 顯示向量y的維度，即元素個數
nchar ( y )                                     # 顯示向量y中每個元素的字元個數
y=="R"                                            # 判斷向量y中為“R”的元素

sex = factor ( c(1,1,0,0,1), levels=c(0,1), labels=c("male","female") )  # 設定因子型資料sex
sex                                                             # 輸出sex的值
class ( sex )                                                # 顯示sex的資料型態
sex1 = factor ( c(1,1,0,0,1), levels=c(0,1), labels=c("female","male") )      # 調換標簽（labels）的取值，得到因子型資料sex1
sex1                                                           # 輸出sex1的值
sex2 = factor (c(1,1,0,0,1), levels=c(1,0), labels=c("male","female") )  # 調換水平（levels）的取值，得到因子型資料sex2
sex2                                                          # 輸出sex2的值
num = factor ( c("a","b","c","d") )                             # 設定因子型變數num
as.numeric ( num )                            # 將因子型資料num轉為數值型資料
num1 = factor ( c("b","a","d","c") )        # 調換num中元素順序，建構因子型變數num1
as.numeric ( num1 )                          # 將因子型資料num1轉為數值型資料
num + 1                                          # 因子型資料不可進行數值運算
as.numeric ( num ) + 1                            # 轉為數值型資料後可參與運算

library ( MASS )                                  # 載入含有資料集的軟體包MASS
data ( Insurance )                                          # 取得資料集Insurance

dim ( Insurance )                                             # 取得資料集的維度
dim ( Insurance[1:10, ] )                            # 取得資料集前10條資料的維度
dim ( Insurance[ ,2:4] )                 # 取得資料集僅含第2、3、4個變數部分的維度
dim ( Insurance ) [1]                     # 取得資料集維度向量的第一個元素，即行數
dim ( Insurance ) [2]                     # 取得資料集維度向量的第二個元素，即列數

vars = c ( "District", "Age" )     # 建構含有“District”和“Age”兩個元素的字元向量vars
Insurance [ 20:25, vars ]                 # 篩選出District及Age變數的第20-25行資料
names ( Insurance )                                  # 輸出Insurance資料集變數名
head ( names(Insurance), n=2 )                                # 僅輸出前2個變數名
tail ( names(Insurance), n=2 )                                # 僅輸出後2個變數名
head ( Insurance$Age )                              # 僅輸出Age變數前許多條資料

class ( Insurance$District )                                # 顯示District的變數型態
class ( Insurance$Age )                                      # 顯示Age的變數型態
class ( Insurance$Holders )                                # 顯示Holders的變數型態

levels ( Insurance$Age )                                # 顯示Age變數的4個水平值
levels ( Insurance$Age) [1]                            # 顯示Age變數的第1個水平值
levels ( Insurance$Age ) [1] = "young"        # 將Age變數的第1個水平值修改為“young”
head ( Insurance$Age )                          # 回看修改後Age變數前許多個取值

is.character ( Insurance$Age )                           # 判斷Age是否為字元型變數
class ( Insurance$Claims )                                  # 顯示Claims的變數型態
class ( as.numeric (Insurance$Claims) )         # 將Claims的資料型態強制轉為數值型


# 抽樣技術 #
sub1=sample(nrow(Insurance),10,replace=T)
Insurance[sub1,]

sub2=sample(nrow(Insurance),10,replace=T,prob=c(rep(0,nrow(Insurance)-1),1))
Insurance[sub2,]

sub3=sample(nrow(Insurance),nrow(Insurance)+1)

sub4=strata(Insurance,stratanames="District",size=c(1,2,3,4),method="srswor")
sub4
getdata(Insurance,sub4)

sub5=strata(Insurance,stratanames="District",size=c(1,2,3,4),description=TRUE)
sub5
getdata(Insurance,sub5)

sub6=strata(Insurance,stratanames="District",size=c(1,2,3,4),method="systematic",pik=Insurance$Claims)
sub6
getdata(Insurance,sub6)

sub7=cluster(Insurance,clustername="District",size=2,method="srswor",description=TRUE)
sub7

sub8=mstage(Insurance, stage = c("stratified","stratified"), varnames=c("District","Group"), size=list(c(16,16,16,16),2),description=TRUE)
sub8
getdata(Insurance,sub8)

train_sub=sample(nrow(Insurance),3/4*nrow(Insurance))
train_data=Insurance[train_sub,]
test_data=Insurance[-train_sub,]
dim(train_data);dim(test_data)











