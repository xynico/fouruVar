library(readxl)
library(Hmisc)
Data <- read_excel("Pre.xlsx")
p = 0.001
listVar = c(2:8)
library(stringr)
t = 1
I=c();J=c();K=c();L=c();M1=c();M2=c();M3=c();M4=c()
for (i in listVar){
  for (j in listVar){
    for (k in listVar){
      for (l in listVar){
        if((l!=k)&(l!=j)&(l!=i)&(k!=j)&(i!=k)&(i!=j))
        {
          Key <-'Y=Data$A'
          KK =paste(Key,as.character(i))
          KK =str_replace_all(string=KK, pattern=" ", repl="")
          eval(parse(text=KK))
          Key <-'W=Data$A'
          KK =paste(Key,as.character(j))
          KK =str_replace_all(string=KK, pattern=" ", repl="")
          eval(parse(text=KK))
          Key <-'U=Data$A'
          KK =paste(Key,as.character(k))
          KK =str_replace_all(string=KK, pattern=" ", repl="")
          eval(parse(text=KK))
          Key <-'X=Data$A'
          KK =paste(Key,as.character(l))
          KK =str_replace_all(string=KK, pattern=" ", repl="")
          eval(parse(text=KK))
          x<-cbind(Y,X,U,W)
          pcor = rcorr(x, type="pearson")[['P']]<p
          checkP = mean(c(pcor[1,2:4],pcor[2,3:4],pcor[3,4]))
          if (checkP == 1)
          {
            b<-lm(Y~X+U)
            c<-lm(W~X+U)
            d<-lm(Y~X+U+W)
            e<-lm(Y~X+U+W+W*U)
            r1 = summary(b)
            r2 = summary(c)
            r3 = summary(d)
            r4 = summary(e)
            A<-r1[["coefficients"]][2,4]
            B<-r2[["coefficients"]][2,4]
            C<-r3[["coefficients"]][4,4]
            D<-r4[["coefficients"]][5,4]
            E<-mean(c(A,B,C,D)<p)
            b1<-lm(Y~X+U+U*X)
            c1<-lm(W~X+U+U*X)
            d1<-lm(Y~X+U+W+U*X)
            r5 = summary(b1)
            r6 = summary(c1)
            r7 = summary(d1)
            A1<-r5[["coefficients"]][4,4]
            B1<-r6[["coefficients"]][4,4]
            C1<-r7[["coefficients"]][4,4]
            H <-mean(c(A1,B1,C1)<p)
            b2<-lm(Y~X+U)
            c2<-lm(Y~X+W)
            d2<-lm(W~X+U)
            r51 = summary(b2)
            r61 = summary(c2)
            r71 = summary(d2)
            A2<-r51[["coefficients"]][2:3,4]
            B2<-r61[["coefficients"]][2:3,4]
            C2<-r71[["coefficients"]][2:3,4]
            G <-mean(c(A2,B2)<p)
            OO<-mean(c(A2,B2,C2)<p)
            if(E==1){M2[t] = 1}else{M2[t] = 0}
            if(H==1){M1[t] = 1}else{M1[t] = 0}
            if(G==1){M3[t] = 1}else{M3[t] = 0}
            if(OO==1){M4[t] = 1}else{M4[t] = 0}
          }else{M1[t] = 0;M2[t] = 0;M3[t] = 0;M4[t] = 0}
          I[t] = i
          J[t] = j
          K[t] = k
          L[t] = l
          t = t+1
        }
      }
    }
  }
}
O = data.frame(I,J,K,L,M1,M2,M3,M4)
names(O)[1]<-'Y'
names(O)[2]<-'W'
names(O)[3]<-'U'
names(O)[4]<-'X'
write.csv(O,'O.csv')

