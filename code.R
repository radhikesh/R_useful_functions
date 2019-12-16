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

### generate freq table for list of cols
lapply(data[,c("race", "gender","education")],table)

### replace values in multiple columns using stringR
replace <- function(x){str_replace_all(x,"\\[\\{'name':\\s|,\\s'id'.*|^'name':\\s|'","")}
prodcompany1<- mutate_all(prodcompany, funs(replace))

### round multiple numeric columns
df <- df %>%mutate_if(is.numeric, round,3)
