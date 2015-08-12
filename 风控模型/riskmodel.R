Risk_found <- function(rstr_history,rm_history,rstr_now,rm_now,confidence=0.05){
#rstr_history=re[-(nrow(re)-1):-nrow(re),];rm_history=r_sec2[-(nrow(r_sec2)-1):-nrow(r_sec2),];rstr_now=re[(nrow(re)-1):nrow(re),];rm_now=r_sec2[(nrow(r_sec2)-1):nrow(r_sec2),]
  if(ncol(rstr_history)!=ncol(rm_history) | any(dim(rstr_now)-dim(rm_now)) |
      ncol(rstr_history)!=ncol(rstr_now) | ncol(rstr_now)!=ncol(rm_now)) {print("输入格式不对");stop}
  #model <- summary(lm(rstr_history~rm_history))
  model <- apply(matrix(1:ncol(rstr_history)),1,function(x)summary(lm(rstr_history[time(rm_history),x]~rm_history[,x])))
  #coef <- model$coef[,1]
  coef <- cbind(alpha=sapply(model,function(x)x$coef[1,1]),beta=sapply(model,function(x)x$coef[2,1]))
  #sigma <- model$sigma
  sigma <- matrix(sapply(model,function(x) x$sigma))
  alpha <- as.xts(matrix(coef[,1],ncol=ncol(rstr_now),nrow=nrow(rstr_now),byrow=T),time(rstr_now))
  beta <-as.xts(matrix(coef[,2],ncol=ncol(rstr_now),nrow=nrow(rstr_now),byrow=T),time(rstr_now))
  #res <- rstr_now - coef[1] - coef[2]*rm_now
  res <- rstr_now - alpha - beta*rm_now
  #p_alpha <- pnorm(res,0,max(c(abs(res)/1.96),sigma))
  p_alpha <- apply(matrix(1:ncol(res)),1,function(x)
    pnorm(res[,x],0,max(c(as.vector(abs(res[,x])/1.96),sigma[x,1])))
    )
  #p_epsilon <- pnorm(res,min(c(abs(res)-1.96*sigma,0)),sigma)
  p_epsilon <- apply(matrix(1:ncol(res)),1,function(x)
    pnorm(res[,x],min(c(as.vector(abs(res[,x])-1.96*sigma[x,1]),0)),sigma[x,1])
    ) 
  if(nrow(rstr_now)==1){
    risk <- p_alpha<confidence & p_epsilon<confidence
    list(p_alpha=p_alpha,p_epsilon=p_epsilon,risk=risk)
  } else {
    n <- nrow(rstr_now)
    k1 <- matrix(apply(p_alpha<confidence,2,sum,na.rm=T))
    k2 <- matrix(apply(p_epsilon<confidence,2,sum,na.rm=T))
    #if(k1>0) p1 <- choose(n,k1)*(1-confidence)^(n-k1)*confidence^k1 else p1 <- NA
    p1 <- apply(k1,1,function(x)if(x>0) choose(n,x)*(1-confidence)^(n-x)*confidence^x else NA)
    #if(k2>0) p2 <- choose(n,k2)*(1-confidence)^(n-k2)*confidence^k2 else p2 <- NA
    p2 <- apply(k2,1,function(x)if(x>0) choose(n,x)*(1-confidence)^(n-x)*confidence^x else NA)
    risk <- p1<confidence & p2<confidence
    risk <- ifelse(is.na(risk),0,risk)
    list(p_alpha=p1,p_epsilon=p2,risk=risk)
  }

}