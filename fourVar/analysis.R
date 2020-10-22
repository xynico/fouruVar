library(QuantPsyc)
library(readxl)
Iwanttotry = 28
Model = 3
BootNumber = 50
Data_I <- read.csv('O.csv')
Data <- read_xlsx('Pre.xlsx')
Iwt <- Data_I[Iwanttotry, ]
betas <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, d)
  Beta <- lm.beta(fit)
  return(Beta)
}

Key <- 'Y=Data$A'
KK = paste(Key, as.character(Iwt$Y))
KK = str_replace_all(string = KK,
                     pattern = " ",
                     repl = "")
eval(parse(text = KK))
Key <- 'W=Data$A'
KK = paste(Key, as.character(Iwt$W))
KK = str_replace_all(string = KK,
                     pattern = " ",
                     repl = "")
eval(parse(text = KK))
Key <- 'U=Data$A'
KK = paste(Key, as.character(Iwt$U))
KK = str_replace_all(string = KK,
                     pattern = " ",
                     repl = "")
eval(parse(text = KK))
Key <- 'X=Data$A'
KK = paste(Key, as.character(Iwt$X))
KK = str_replace_all(string = KK,
                     pattern = " ",
                     repl = "")
eval(parse(text = KK))
data <- data.frame(Y, W, U, X)
if (Model == 1)
{
  f1 <- 'Y~X+U'
  f2 <- 'W~X+U'
  f3 <- 'Y~X+U+W'
  f4 <- 'Y~X+U+W+W*U'
  result1 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f1)
  l1 = length(result1[['t']][1, ])
  result2 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f2)
  l2 = length(result2[['t']][1, ])
  result3 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f3)
  l3 = length(result3[['t']][1, ])
  result4 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f4)
  l4 = length(result4[['t']][1, ])
  t = 1
  RCIlower = c()
  RCIupper = c()
  for (i in 1:l1)
  {
    R <- t.test(result1[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l2)
  {
    R <- t.test(result2[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l3)
  {
    R <- t.test(result3[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l4)
  {
    R <- t.test(result4[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  
}
if(Model == 2)
{
  f1 <- 'Y~X+U+U*X'
  f2 <- 'W~X+U+U*X'
  f3 <- 'Y~X+U+W+U*X'
  result1 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f1)
  l1 = length(result1[['t']][1, ])
  result2 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f2)
  l2 = length(result2[['t']][1, ])
  result3 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f3)
  l3 = length(result3[['t']][1, ])
  t = 1
  RCIlower = c()
  RCIupper = c()
  for (i in 1:l1)
  {
    R <- t.test(result1[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l2)
  {
    R <- t.test(result2[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l3)
  {
    R <- t.test(result3[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  
}
if(Model == 3)
{
  f1 <- 'Y~X+U'
  f2 <- 'Y~X+W'
  result1 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f1)
  l1 = length(result1[['t']][1, ])
  result2 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f2)
  l2 = length(result2[['t']][1, ])
  t = 1
  RCIlower = c()
  RCIupper = c()
  for (i in 1:l1)
  {
    R <- t.test(result1[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l2)
  {
    R <- t.test(result2[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
}
if(Model == 4)
{
  f1 <- 'Y~X+U'
  f2 <- 'Y~X+W'
  f3 <- 'W~X+U'
  result1 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f1)
  l1 = length(result1[['t']][1, ])
  result2 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f2)
  l2 = length(result2[['t']][1, ])
  result3 <-
    boot(data,
         statistic = betas,
         R = BootNumber,
         formula = f3)
  l3 = length(result3[['t']][1, ])
  t = 1
  RCIlower = c()
  RCIupper = c()
  for (i in 1:l1)
  {
    R <- t.test(result1[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l2)
  {
    R <- t.test(result2[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
  for (i in 1:l3)
  {
    R <- t.test(result3[['t']][, i])
    RCIlower[t] <- R[["conf.int"]][1]
    RCIupper[t] <- R[["conf.int"]][2]
    t = t + 1
  }
}
RList = data.frame(RCIlower, RCIupper)
write.table(RList, 'RList.csv')