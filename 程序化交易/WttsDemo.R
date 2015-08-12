####################################
# 2015-05-11 15:08:15 CST
# zhibinzeng
# wind wtts 证券模拟组合构建示例代码
# 随机抽取100只股票买入
###################################
library(WindR)
library(lubridate)
w.start()
Today <- as.character(today())
TOTAL <- 10000000 #总资金量

StockCodeAll<-w.wset('SectorConstituent',paste('date=',gsub('-','',Today),';sector=全部A股(非ST)',sep=''))
StockCodeAll<-StockCodeAll$Data[,3]
RandCodes<-StockCodeAll[sample(1:length(StockCodeAll),100)] #随机选择出100只股票


logonID <-w.tlogon('0000','0','W529639901701','******','SHSZ')$Data$LogonID[1]

#限价委托 股票下单
lastPrice <-w.wsq(RandCodes,"rt_last") #获取随机股票的最新成交价
buyList <-lastPrice$Data[which(lastPrice$Data[,2]!=0),] #排除最新成交价格为0的股票，排除停牌股
code.buyList <- buyList[,1] #需要交易的股票列表
price.buyList <- buyList[,2] #需要交易的股票最新成交价
volume <-floor((TOTAL/sum(price.buyList))/100) #等权重买入，每只股票买入多少手
order.list <-w.torder(code.buyList,'Buy',price.buyList,volume,paste('OrderType=LMT;LogonID=',logonID,sep=''))$Data
cancel.list <- w.tquery('Order',paste('LogonID=',logonID,';OrderType=Withdrawable',sep=''))$Data #查询当前可撤单
w.tcancel(cancel.list$OrderNumber,'LogonID=1') #根据ordernumber撤单
w.torder(cancel.list$SecurityCode,'Buy',lastPrice$Data[,2],cancel.list$OrderVolume,'OrderType=LMT;LogonID=1')$Data
lastPrice <-w.wsq(canceldf$SecurityCode,"rt_last") #获取随机股票的最新成交价
w.tquery('Trade','LogonID=1')$Data #查询成交
w.tquery('Capital','LogonID=1')$Data #查询账户资金
w.tquery('Position','LogonID=1') #查询账户持仓

# 最优五档成交，剩余撤销
lastPrice <-w.wsq(RandCodes,"rt_last") #获取随机股票的最新成交价
buy.list <-lastPrice$Data[which(lastPrice$Data[,2]!=0),] #排除最新成交价格为0的股票，排除停牌股
code.buyList <- buyList[,1] #需要交易的股票列表
price.buyList <- buyList[,2] #需要交易的股票最新成交价
volume <-floor((TOTAL/sum(price.buyList))/100) #等权重买入，每只股票买入多少手

order.list <-w.torder(code.buyList,c('Buy','Buy'),volume,paste('OrderType=B5TC;LogonID=',logonID,sep=''))$Data
w.tquery(2,logonid=1) #查询委托

# 退出登录
list.LogonID <- w.tquery('LogonID')$Data$LogonID #查询当前登录了哪些账户
w.tlogout(list.LogonID)



library(WindR)
w.start()
w.menu()

