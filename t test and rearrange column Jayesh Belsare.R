install.packages("dplyr") 
library("dplyr") 
ttest <- function(df,src,tr)
{
  result <-list()
  for(i in 1:ncol(df))
  {
    if(is.numeric(df[,i])) ## check column in numeric or not
    {
      if (names(df[i])%in%src){
        a <- pull(df, i)   ## function to convert  dataframe column in vector
        print(names(df[i])) ## column name
        res <- t.test(a,tr, data = df, var.equal = TRUE)  ## t-test two sample
        print(res$p.value)  ## print p-value
        print(res)
        x<-c(names(df[i]),res$p.value)
        result <- c(result,x)  ## saving pvalue and column name in vector to return as result
        
      }
    }
  }
  return(result) # return result of two sample test
} 
df <- read.csv("/home/jayesh/Downloads/framingham.csv")
src<- c("totChol","BMI","heartRate")  # source vector
tr <- c(df$TenYearCHD)                 # target vector
View(resu)
resu<-ttest(df,src,tr)
str(resu)
#########################################################################################
install.packages("dplyr") ############ two sample chi square test
library("dplyr") 
ctest <- function(df,src,tr) ## chi square test function
{
  result<-c()
  for(i in 1:ncol(df))
  {
    if(is.numeric(df[,i])) ## if numeric skip the loop and move to next column
    {
      next
    }
      else if (names(df[i])%in%src){
        a <- pull(df, i)   ## function to convert  dataframe column in vector
        a[is.na(a)] <- 0
        tr[is.na(tr)] <- 0
        print(names(df[i])) ## column name
        res <- chisq.test(cbind(a,tr,simulate.p.value = TRUE))  ## chisq-test two sample 
        print(res$p.value)  ## print p-value
        x<-c(names(df[i]),res$p.value)
        result <- c(res,x)  ## saving pvalue and column name in vector to return as result
        
      }
    }
#return(result) # return result of two sample test
  }

 

df <- read.csv("/home/jayesh/Downloads/framingham.csv")
src<- c("totChol","BMI","heartRate")  # source vector
tr <- c(df$TenYearCHD)                 # target vector
View(df$totChol)
resu1<-ctest(df,src,tr)


#Version 6
#Rearrange dataframe column according to there data type
df <- read.csv("/home/jayesh/Downloads/framingham.csv")
dtype <- c ("integer","numeric","character","logical")  # datatypes in r
#my_data2<-
my_data2<-rarrange(df,dtype)
View(my_data2)
rarrange <- function(df,c)
{ col_order <- c()
for(i in 1:length(c))  # loop on datatype vector
{ k<-0
for(j in 1:ncol(df)) # loop on number of col in dataframe 
{  
  if(c[i]==class(df[[j]]))  # match datatype of col  with vector 
  {
    b<-names(df[j])
    col_order <- c(col_order,b) # if match stores the colname in vector
  }
}

}
#  View(col_order)
df <- df[ col_order] ## reorder columns
return (df) # return new rearrange data frame
}

