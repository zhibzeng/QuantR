library(WindR)
library(RODBC)
w.start(0,FALSE)
options(digits=4)
#w.menu()
logfile<-file("G:\\OptionData\\optionlog_ds0708.data", "w")
dat_re<- list()
arbi_re <- list()
o <-1
myCallback<-function(out)
{
  if(out$ErrorCode | is.null(out)){
    if(is.ull(out)) cat(paste("send null in ",Sys.time(),"for 'out'"),file=logfile) else
      cat(paste("Error in ",w.asDateTime(out$Time),"for",out$Code),file=logfile)
    return ();
  } else{
    # if(length(out$Code)>1 | length(out$Time)>1){
    dat_re[[o]]<<-out
    if(floor(as.numeric(Sys.time()))%%10==0)save(dat_re,file="G:\\OptionData\\DatLog_ds0708.RData")
    #       print(dat_re)
    #  }
    newd=odata
    for(x in 1:length(out$Code))
      newd[match(out$Code[x],newd$CODE),match(out$Field,colnames(newd))]=out$Data[,x,dim(out$Data)[3]]
    newd$time=w.asDateTime(out$Time)
    a1=paste(kpair[,1],kpair[,4],kpair[,5])
    a2=paste(kpair[,2],kpair[,4],kpair[,5])
    a3=paste(kpair[,3],kpair[,4],kpair[,5])
    b=paste(newd$strike_price,newd$month,newd$call_put)
    buy1 <- newd$RT_ASK1[match(a1,b)]
    sell <- newd$RT_BID1[match(a2,b)]
    buy2 <- newd$RT_ASK1[match(a3,b)]
    jiancang <- sell*2-buy1-buy2
    sell1 <- newd$RT_BID1[match(a1,b)]
    buy <- newd$RT_ASK1[match(a2,b)]
    sell2 <- newd$RT_BID1[match(a3,b)]
    pingcang <- sell1+sell2-buy*2
    profit <- jiancang+pingcang
  
    port <- data.frame(kpair,jiancang,pingcang,profit,t=newd$time[1])
    odata<<-newd
    if(any(jiancang>0.001&profit>0)){
      arbitrage <- port[jiancang>0.001&profit>0,]
      arbi_re[[o]] <<- arbitrage
      
      print(newd$time[1])
      print(arbitrage)
      cat('\n')
      
    }
    if(floor(as.numeric(Sys.time()))%%60==0) save(arbi_re,file="G:\\OptionData\\arbi_re_ds0708.RData", "w")
    o<<-o+1
    #insertdata=as.data.frame(matrix(NA,nrow=length(out$Code)*length(out$Time),ncol=length(objfield)+2))
    #colnames(insertdata)=c("time","code",colnames(odata)[c(-1,-ncol(odata))])
    #for(x in 1:length(out$Code)){
    #for(y in 1:length(out$Time)){
    #
    #}
    #  insertdata[x,1]=as.character(w.asDateTime(out$Time))
    #  insertdata[x,2]=out$Code[x]
    #  insertdata[x,-1:-2]=newd[which(insertdata[x,2]==newd$CODE),c(-1,-ncol(newd))]
    #}
    #inser <- sqlSave(myconn, insertdata, tablename = 'optionrealtimedata',rownames = FALSE, colnames = FALSE, append=TRUE,fast=TRUE)
  }
}
date <- Sys.Date()
objfield=c("rt_bid1","rt_ask1","rt_pre_settle","rt_settle")
pairmat <- function(mon){
  k <-w.wset('OptionChain',paste('date=',date,";us_code=510050.SH;option_var=;month=",mon,";call_put=call;
                                 field=strike_price",sep=""))$Data[,-1]
  small <- min(k)
  big <- max(k)
  interval <- (big-small)/2-((big-small)/2)%%0.05
  s <- seq(0.05,interval,0.05)
  mid <- apply(expand.grid(k,s),1,sum)
  rig <- apply(expand.grid(k,2*s),1,sum)
  pair <- cbind(k,mid,rig,mon)
  kmat <- pair[as.character(pair[,2]) %in% as.character(pair[,1]) & as.character(pair[,3]) %in% as.character(pair[,1]),]
  type <- expand.grid(kmat[,1],c(1,2))[,2]
  kmat <- cbind(rbind(kmat,kmat),type)
  return(kmat)
}
mat1 <- pairmat(201507)
mat2 <- pairmat(201508)
mat3 <- pairmat(201509)
mat4 <- pairmat(201512)
kpair <- rbind(mat1,mat2,mat3,mat4)
Set<-w.wset('OptionChain',paste('date=',date,";us_code=510050.SH;option_var=;month=;call_put=;field=option_code,option_name,strike_price,month,call_put,expiredate",sep=""))$Data[,-1]
colnames(Set)[1] <- "CODE"
objcode=Set$CODE
Set$call_put[Set$call_put=="认购"]<-1
Set$call_put[Set$call_put=="认沽"]<-2
odata<-w.wsq(objcode,objfield)$Data
odata <- cbind(odata,Set[,-1])
data<-w.wsq(objcode,objfield,func=myCallback)
w.cancelRequest(data$RequestID);
