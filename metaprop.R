library(readxl)
y1 <- read_excel("y1.xlsx", sheet = "Sheet14", 
								 col_types = c("text", "text", "numeric", 
								 							"numeric"))
View(y1)

library(meta)
##性“PRAW”(没有转换的原始率)
rate<-transform(y1, p= n/total)
shapiro.test(rate$p)
##”PLN”(对数转换)
rate<-transform(y1, log=log(n/total))
shapiro.test(rate$log)
##”PLOGIT“(logit转换)
rate<-transform(y1, logit=log((n/total)/(1-n/total)))
shapiro.test(rate$logit)
##“PAS”(反正弦转换)
rate<-transform(y1, arcsin.size=asin(sqrt(n/(total+1))))
shapiro.test(rate$arcsin)
##“PFT“(Freeman-Tukey双重反正弦转换)
rate<-transform(y1,darcsin=0.5*(asin(sqrt(n/(total+1)))+asin((sqrt(n+1)/(total+1)))))
shapiro.test(rate$ darcsin)

##meta分析
meta1 <- metaprop(n,total, data=y1, studlab=paste(第一作者,sep="--",年份),sm="PFT",incr=0.5,allincr=TRUE,addincr=FALSE)
meta1
##森林图，随机效应
forest(meta1,comb.fixed = FALSE)
forest(meta1,comb.random = FALSE)
##漏斗图
funnel(meta1)
##发表偏倚检验
#egger
metabias(meta1,method.bias = "linreg",plotit = T,k.min=2)
#begg
metabias(meta1,method.bias = "rank",plotit = T,k.min=2)
##敏感性分析
meta2=metaprop(n,total, data=y1, studlab=paste(第一作者,sep="--",年份),sm="PFT",incr=0.5,allincr=TRUE,addincr=FALSE,comb.fixed=FALSE)
metainf(meta2)
forest(metainf(meta2),comb.fixed=FALSE)

rm(meta2)
