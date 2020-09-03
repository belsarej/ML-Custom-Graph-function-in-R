
#Version 1:
#Often we are not required the graphs for all the numeric variables. Try to improve the code
#by adding an additional parameter "variable" that can take a vector of variable index and 
#return the graphs for only those variables.
#
#Example: Graphs(Boston, var=c(1,3,4))
#Will generate the graphics for only the numerical variables among the variables 1,3 & 4
#in the data Boston
Graphs <- function(data,z )
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i])) 
      if(i %in% z){                     # check the column position present in vector
        
        
        {
          
          png(paste(names(data)[i], ".png", sep="")) #NOTE this step
          
          par(mfrow=c(2,1))
          boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
                  ylab = names(data)[i], col = "maroon", border = "grey5",
                  horizontal = T)
          
          hist(data[,i], main = paste("Histogram of", names(data)[i]), 
               xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
          
          
          
          dev.off()  #NOTE this step
          
        }
        
      }
  }
}
setwd("/home/jayesh/New Folder1")
z=c(1,2,3)  # vector contain position of column
Graphs(Boston,z)  #moving vector as parameter of function graphs






#Version 2:
#Improve the code in Version 1 such that if the argument variable is ignored then it will
#return the graphs of all the numeric variables in the data by default.
Graphs <- function(data,z=x)  #default value set here if variable ignored
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i])) 
      if(i %in% z){
        {
          
          png(paste(names(data)[i], ".png", sep="")) #NOTE this step
          
          par(mfrow=c(2,1))
          boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
                  ylab = names(data)[i], col = "maroon", border = "grey5",
                  horizontal = T)
          
          hist(data[,i], main = paste("Histogram of", names(data)[i]), 
               xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
          
          
          
          dev.off()  #NOTE this step
          
        }
        
      }
  }
}
z=c(1,2,3)
Graphs(Boston)

l <-ncol(Boston)  # move number of col to variable
x <- c()        # create empty vector
for (i in 1:l)  # 
{
  x[i] <- i    # moving number of column in default vector
}
x
#Version 3:
#We ignored the cateorical variables in our discussion. Make some improvement in your codes
#in Version 2 such that the function will take the argument "data" and "variable" and will
#return boxplots & histograms for the numerical variables and barplots and pie charts for
#the categorical variables.
#
#Example:
#Graphs(mtcars)
#will get the necessary graphics for all numeric variables and categorical variables in the
#data
Graphs <- function(data,z=x)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))  # check column numeric or not if not got to else part
    { if(i %in% z)
      
    {
      
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),  #boxplot code
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), # histogram code 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
      
    }
    else{   # code for non numerical data
      if(i %in% z)
      {
        png(paste(names(data)[i], ".png", sep="")) #NOTE this step
        
        par(mfrow=c(2,1))
        barplot(data[i,], main = paste("Barplot of", names(data)[i]),  # bar plot for categorical variable
                ylab = names(data)[i], col = "maroon", border = "grey5",
                horizontal = T)
        
        pie(data[i,], main = paste("pie char of", names(data)[i]),   # pie chart code
            labels = names(data)[i], col = "lightgreen", border=F)
        
        
        
        dev.off()  #NOTE this step
        
      }
    }
    
    
    
  }
}
z=c(1,2,3)
Graphs(mtcars)

l <-ncol(mtcars)  # move number of col to variable
x <- c()          # create empty vector
for (i in 1:l)     
{
  x[i] <- i
}
View(mtcars)  # moving number of column in default vector

#Version 4:
#Probably you need not want to mess up your working directory with so many image files...
#Create an additional argument for the function "dir" (directory), such that the function
#exports all the files to your specified folder (which need not necessaryly be your working
#directory).
#

#Example:
#Graphs(Boston, Variable = c(1,3,4), dir = ".../Praxis/LearntSometingNew/Graphs")
#will generate the necessary graphics for the variables 1, 3 and 4 in the specified location
#in your system i.e. ".../Praxis/LearntSometingNew/Graphs"

Graphs <- function(data,z=x,dr)
{
  setwd(dr)  # setting the location given in dr
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i])) 
    { if(i %in% z)
      
    {
      
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
      
    }
    else{
      if(i %in% z)
      {
        png(paste(names(data)[i], ".png", sep="")) #NOTE this step
        
        par(mfrow=c(2,1))
        barplot(data[i,], main = paste("Barplot of", names(data)[i]), 
                ylab = names(data)[i], col = "maroon", border = "grey5",
                horizontal = T)
        
        pie(data[i,], main = paste("pie char of", names(data)[i]), 
            labels = names(data)[i], col = "lightgreen", border=F)
        
        
        
        dev.off()  #NOTE this step
        
      }
    }
    
    
    
  }
}
z=c(1,2,3)
Graphs(mtcars,dr="/home/jayesh/New Folder1")  # location pass through dr

l <-ncol(mtcars)   # move number of col to variable
x <- c()
for (i in 1:l)  # create empty vector
{
  x[i] <- i   # moving number of column in default vector
}



