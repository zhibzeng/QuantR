library(WindR)
w.start()
today <- gsub('-','',Sys.Date())
months = c('201508','201509','201512','201603')

calloptiondata.Lists <- list()
putoptiondata.Lists <- list()
futuredata.Lists <- list()

    month <-  months[1]
    ### 期权部分
    # 认购期权
    temp <- w.wset('OptionChain',paste('date=',today,';us_code=510050.SH;option_var=;month=',month,';call_put=call;field=option_code,option_name,strike_price,month,call_put',sep=''))
    temp <- temp$Data
    # 期权数据实时回调函数
    CallBackFun <- function(outdata){
        ## 实时读取数据异常
        if(outdata$ErrorCode || is.null(outdata)){
            if (is.null(outdata)) {
                cat(paste("send null in ",Sys.time(),"for 'out'"),file=logfile)
            } else {
                cat(paste("Error in ",w.asDateTime(outdata$Time),"for",outdata$Code),file=logfile)
            }
            return ();
        }
        for (code in outdata$Code) {
            flag <- outdata$Data[seq(length(outdata$Field)),match(code,outdata$Code),1]!=0
            if (any(flag)){
                calloptiondata[match(code, calloptiondata$CODE),match(outdata$Field[flag], colnames(calloptiondata))] <<- outdata$Data[flag,match(code,outdata$Code),1]
            }
        }
        calloptiondata.Lists[[length(calloptiondata.Lists)+1]] <<- calloptiondata
    }
    calloptiondata <- w.wsq(temp$option_code,"rt_latest,rt_bid1,rt_ask1")$Data
   
    
    # 认沽期权
    temp.put <- w.wset('OptionChain',paste('date=',today,';us_code=510050.SH;option_var=;month=',month,';call_put=put;field=option_code,option_name,strike_price,month,call_put',sep=''))
    temp.put <- temp.put$Data
    PutBackFun <- function(outdata){
        ## 实时读取数据异常
        if(outdata$ErrorCode || is.null(outdata)){
            if (is.null(outdata)) {
                cat(paste("send null in ",Sys.time(),"for 'out'"),file=logfile)
            } else {
                cat(paste("Error in ",w.asDateTime(outdata$Time),"for",outdata$Code),file=logfile)
            }
            return ();
        }
        for (code in outdata$Code) {
            flag <- outdata$Data[seq(length(outdata$Field)),match(code,outdata$Code),1]!=0
            if (any(flag)){
                putoptiondata[match(code, putoptiondata$CODE),match(outdata$Field[flag], colnames(putoptiondata))] <<- outdata$Data[flag,match(code,outdata$Code),1]
            }
        }
        putoptiondata.Lists[[length(putoptiondata.Lists)+1]] <<- putoptiondata
    }
    putoptiondata <- w.wsq(temp.put$option_code,"rt_latest,rt_bid1,rt_ask1")$Data
  
    
    
    ### 期货部分
    # 期货实时回调函数
    futureCallBack <- function(outdata){
        ## 实时读取数据异常
        if(outdata$ErrorCode || is.null(outdata)){
            if (is.null(outdata)) {
                cat(paste("send null in ",Sys.time(),"for 'out'"),file=logfile)
            } else {
                cat(paste("Error in ",w.asDateTime(outdata$Time),"for",outdata$Code),file=logfile)
            }
            return ();
        }
        for (code in outdata$Code) {
            flag <- outdata$Data[seq(length(outdata$Field)),match(code,outdata$Code),1]!=0
            if (any(flag)){
                futuredata[match(code, futuredata$CODE),match(outdata$Field[flag], colnames(futuredata))] <<- outdata$Data[flag,match(code,outdata$Code),1]
            }
        }
        futuredata.Lists[[length(futuredata.Lists)+1]] <<- futuredata
    }
    futureCode <- paste('IH',substr(month,3,6),'.CFE',sep='') #对应当月期货合约 
    futuredata <- w.wsq(futureCode,"rt_latest,rt_bid1,rt_ask1")$Data
   
    
    data.wsq.option.call <- w.wsq(temp$option_code,"rt_latest,rt_bid1,rt_ask1",func=CallBackFun) # 实时订阅
    data.wsq.option.put <- w.wsq(temp.put$option_code,"rt_latest,rt_bid1,rt_ask1",func=PutBackFun) # 实时订阅
    data.wsq.future <- w.wsq(futureCode,"rt_latest,rt_bid1,rt_ask1",func=futureCallBack) # 实时订阅
    
    # 合成期货空头
    # 买入看跌期权，卖出看涨期权，买入股指期货
    while(1){ #循环输出
        combine <-  (temp$strike_price-putoptiondata$RT_ASK1+calloptiondata$RT_BID1)*1000 # 买入认沽，卖出认购，等于合成期货空头的指数
        total <- combine-futuredata$RT_ASK1
        
        # 程序化交易
        
        print(total)
        Sys.sleep(1)
    }
    
    
    # 合成期货多头
    # 卖出看跌期权，买入看涨期权，买入股指期货
#     while(1){ #循环输出
#         combine <-  (temp$strike_price+putoptiondata$RT_BID1-calloptiondata$RT_ASK1)*1000 # 卖出认沽，买入认购，等于合成期货多头的指数
#         total <- futuredata$RT_ASK1-combine
#         print(total)
#         Sys.sleep(5)
#     }



# 取消订阅
w.cancelRequest(data.wsq.future$RequestID)
w.cancelRequest(data.wsq.option.call$RequestID)
w.cancelRequest(data.wsq.option.put$RequestID)

