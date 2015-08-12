library(WindR)
library(RODBC)
library(plyr)
w.start(0,FALSE)
options(digits=4)
date <- Sys.Date()
#w.menu()
logfile<-file(paste("E:\\hy\\OptionData\\optionlog_ys",date,".data",sep=""), "w")
dat_re<- list()
arbi_re <- list()
o <-1

#############
# WTTS
#############
logonID <-w.tlogon('0000','0','W529639901703','******','SHO')$Data$LogonID[1]
TotalCapital <- 1000000 #总资产额度


myCallback2<-function(out)
{
    if(out$ErrorCode || is.null(out)){
        if (is.null(out)) {
            cat(paste("send null in ",Sys.time(),"for 'out'"),file=logfile)
        } else {
            cat(paste("Error in ",w.asDateTime(out$Time),"for",out$Code),file=logfile)
        }
        return ();
    }
    dat_re[[o]]<<-out
    newd=odata
    print(out)
    for (x in 1:length(out$Code)){
        newd[match(out$Code[x],newd$CODE),match(out$Field,colnames(newd))]=out$Data[x,dim(out$Data)[3]]
     }
    newd$time=w.asDateTime(out$Time)
    a1=paste(kpair[,1],kpair[,5],kpair[,6])
    a2=paste(kpair[,2],kpair[,5],kpair[,6])
    a3=paste(kpair[,3],kpair[,5],kpair[,6])
    a4=paste(kpair[,4],kpair[,5],kpair[,6])
    b=paste(newd$strike_price,newd$month,newd$call_put)
    orderdf <- data.frame()
    buy1 <- newd$RT_ASK1[match(a1,b)]
    sell1 <- newd$RT_BID1[match(a2,b)]
    sell2 <- newd$RT_BID1[match(a3,b)]
    buy2 <- newd$RT_ASK1[match(a4,b)]
    jiancang <- sell1+sell2-buy1-buy2
    
    sell1 <- newd$RT_BID1[match(a1,b)]
    buy1 <- newd$RT_ASK1[match(a2,b)]
    buy2 <- newd$RT_ASK1[match(a3,b)]
    sell2 <- newd$RT_BID1[match(a4,b)]
    pingcang <- sell1+sell2-buy1-buy2
    profit <- jiancang+pingcang
  
    port <- data.frame(kpair,jiancang,pingcang,profit)
    code1 <-paste(newd$CODE[match(a1,b)],newd$RT_BID1[match(a1,b)],newd$RT_ASK1[match(a1,b)],sep=',')
    code2 <-paste(newd$CODE[match(a2,b)],newd$RT_BID1[match(a2,b)],newd$RT_ASK1[match(a2,b)],sep=',')
    code3 <-paste(newd$CODE[match(a3,b)],newd$RT_BID1[match(a3,b)],newd$RT_ASK1[match(a3,b)],sep=',')
    code4 <-paste(newd$CODE[match(a4,b)],newd$RT_BID1[match(a4,b)],newd$RT_ASK1[match(a4,b)],sep=',')
  
    port$k <- code1
    port$mid1 <- code2
    port$mid2 <- code3
    port$rig <- code4
    odata<<-newd
    if( any(jiancang>0.005&profit>0.001) ){
      arbitrage <- port[jiancang>0.005&profit>0.001,]
      arbitrage <- arbitrage[order(arbitrage$profit, decreasing = T),]
      arbi_re[[o]] <<- arbitrage
        for ( rindex in c(1:nrow(arbitrage)) ) {
            codelist <- c()
            pricelist <- c()
            temp <- arbitrage[rindex,]
            l_ply(c(1:4),function(i){
                codelist <<- c(codelist,unlist(strsplit(temp[[i]], ","))[1])
            })
            ## 限价下单
            pricelist <- c(pricelist,unlist(strsplit(temp[[1]], ","))[3])  # 买入开仓
            pricelist <- c(pricelist,unlist(strsplit(temp[[2]], ","))[2])  # 卖出开仓
            pricelist <- c(pricelist,unlist(strsplit(temp[[3]], ","))[2])
            pricelist <- c(pricelist,unlist(strsplit(temp[[4]], ","))[3])
            orderinfo <- w.torder(codelist,c('Buy','Short','Short','Buy'),pricelist,1,paste('OrderType=LMT;LogonID=',logonID,sep=''))
            # 查询委托成交情况
            ordercode <- w.tquery("order",requestid=orderinfo$Data$RequestID)$Data
            if ( !all(ordercode$Remark=='已成') ){
                cancel.list <- ordercode[ordercode$Remark=='已报',] ## 如果不能同步成交，将未成交的单撤销，已成交的单平仓
                while( length(cancel.list$OrderNumber)>0 ){
                    for (cancelcode in cancel.list$OrderNumber){
                        w.tcancel(cancelcode,paste('LogonID=',logonID,sep='')) #根据ordernumber撤单
                    }
                  ordercode <- w.tquery("order",requestid=orderinfo$Data$RequestID)$Data
                  cancel.list <- ordercode[ordercode$Remark=='已报',]
                }
            }
            
          # 市价下单
          # orderinfo <- w.torder(codelist,c('1','2','2','1'),'0','1',paste('OrderType=MTL;LogonID=',logonID,sep=''))
        }
      
    }
    o<<-o+1
  
}


ObjectField <- c("rt_bid1","rt_ask1","rt_pre_settle","rt_settle") #获取WSQ字段
pairmat <- function(mon){
  k <-w.wset('OptionChain',paste('date=',date,";us_code=510050.SH;option_var=;month=",mon,";call_put=call;
                                 field=strike_price",sep=""))$Data[,-1]
  small <- min(k)
  big <- max(k)
  interval <- (big-small)/3-((big-small)/3)%%0.05
  s <- seq(0.05,interval,0.05)
  mid1 <- apply(expand.grid(k,s),1,sum)
  mid2 <- apply(expand.grid(k,2*s),1,sum)
  rig <- apply(expand.grid(k,3*s),1,sum)
  pair <- cbind(k,mid1,mid2,rig,mon)
  kmat <- pair[as.character(pair[,2]) %in% as.character(pair[,1]) & as.character(pair[,3]) %in% as.character(pair[,1]) 
               & as.character(pair[,4]) %in% as.character(pair[,1]),]
  type <- expand.grid(kmat[,1],c(1,2))[,2]
  kmat <- cbind(rbind(kmat,kmat),type)
  return(kmat)
}
mat1 <- pairmat(201508)
mat2 <- pairmat(201509)
mat3 <- pairmat(201512)
mat4 <- pairmat(201603)
kpair <- rbind(mat1,mat2,mat3,mat4)

Set<-w.wset('OptionChain',paste('date=',date,";us_code=510050.SH;option_var=;month=;call_put=;field=option_code,option_name,strike_price,month,call_put,expiredate",sep=""))$Data[,-1]
colnames(Set)[1] <- "CODE"
objcode=Set$CODE
Set$call_put[Set$call_put=="认购"]<-1
Set$call_put[Set$call_put=="认沽"]<-2
odata<-w.wsq(objcode,ObjectField)$Data
odata <- cbind(odata,Set[,-1])
data<-w.wsq(objcode,ObjectField,func=myCallback2)


# 取消实时
w.cancelRequest(data$RequestID)



#############
#  查询
#############
w.tquery('Capital',paste('LogonID=',logonID,sep=''))$Data #查询账户资金
w.tquery('Position',paste('LogonID=',logonID,sep='')) #查询账户持仓


#############
# 平仓
#############
posdata <- w.tquery('Position',paste('LogonID=',logonID,sep=''))
if ( posdata$ErrorCode==0 ) {
  posdata <- posdata$Data
  posdata <- posdata[posdata$EnableVolume>0,] #未平仓交易
  posdata <- posdata[,c(1,6,7)]
  pricedata <- w.wsq(posdata$SecurityCode,'RT_BID1,RT_ASK1')
  if ( pricedata$ErrorCode==0 ){
    pricedata <- pricedata$Data
    posdata$TradeSide <- gsub('Buy','Sell',posdata$TradeSide)
    posdata$TradeSide <- gsub('Short','Cover',posdata$TradeSide)
    price <- c()
    for (rindex in c(1:length(posdata$TradeSide))) {
      price <- c(price, ifelse(posdata$TradeSide[rindex]=='Sell',pricedata$RT_BID1[rindex],pricedata$RT_ASK1[rindex]))
    }
    w.torder(posdata$SecurityCode, posdata$TradeSide, price,posdata$EnableVolume)
    # orderinfo <- w.torder(posdata$SecurityCode,posdata$TradeSide,'0',posdata$EnableVolume,'OrderType=MTL')
    
  }
}

####################
# 委托撤单
####################
cancel.list <- w.tquery('Order',paste('LogonID=',logonID,';OrderType=Withdrawable',sep=''))$Data #查询当前可撤单
w.tcancel(cancel.list$OrderNumber,paste('LogonID=',logonID,sep='')) #根据ordernumber撤单
w.tlogout(LogonID = logonID)
