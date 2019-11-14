### converting all factor variable to numeric 

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))

### which variables contain how many missing values

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

### converting a list of variables to factors
names <- c(1:3,5)
mydata[,names] <- lapply(mydata[,names] , factor)
str(mydata)
