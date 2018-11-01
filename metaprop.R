library(readxl)
y1 <- read_excel("r2/y1.xlsx",sheet = 2, col_types = c("text", 
																						 "text", "numeric", "numeric"))
View(y1)

library(meta)
options(digits=4)
##性“PRAW”(没有转换的原始率)
rate<-transform(y1, p= n/total)
a <- shapiro.test(rate$p)$p.value;a
##”PLN”(对数转换)
rate<-transform(y1, log=log(n/total))
b <- shapiro.test(rate$log)$p.value;b
##”PLOGIT“(logit转换)
rate<-transform(y1, logit=log((n/total)/(1-n/total)))
c <- shapiro.test(rate$logit)$p.value;c
##“PAS”(反正弦转换)
rate<-transform(y1, arcsin.size=asin(sqrt(n/(total+1))))
d <- shapiro.test(rate$arcsin)$p.value;d
##“PFT“(Freeman-Tukey双重反正弦转换)
rate<-transform(y1,darcsin=0.5*(asin(sqrt(n/(total+1)))+asin((sqrt(n+1)/(total+1)))))
e <- shapiro.test(rate$ darcsin)$p.value;e
p <- c(a,b,c,d,e);p
m <- c("PRAW","PLN","PLOGIT","PAS","PFT")
method <- data.frame(m,p);method
m <- method[which.max(method$p),1];m
rm(a,b,c,d,e)


##meta分析
meta1 <- metaprop(n,total, data=y1, studlab=paste(第一作者,sep="--",年份),sm=m,incr=0.5,allincr=TRUE,addincr=FALSE)
meta1
##森林图，随机效应
forest(meta1,comb.fixed = FALSE)
forest(meta1,comb.random = FALSE)
##漏斗图
funnel(meta1)
##发表偏倚检验
#egger
egger <- metabias(meta1,method.bias = "linreg",plotit = T,k.min=2)
egger <- egger$p.value
#begg
begg <- metabias(meta1,method.bias = "rank",plotit = T,k.min=2)
begg <- begg$p.value
bias <- data.frame(c("egger",egger),c("begg",begg));bias
##敏感性分析
meta2=metaprop(n,total, data=y1, studlab=paste(第一作者,sep="--",年份),sm=m,incr=0.5,allincr=TRUE,addincr=FALSE,comb.fixed=FALSE)
metainf(meta2)
forest(metainf(meta2),comb.fixed=FALSE)
forest(metainf(meta2),comb.random = FALSE)
rm(meta2)
