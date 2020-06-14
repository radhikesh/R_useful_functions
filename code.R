### converting all factor variable to numeric 

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
### or another way to convert all numeric to character 
pro_df <- pro_df %>%mutate_if(is.numeric, as.character)


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

### plot matrix of independent variable vs depended variable using gridExtra using for loop:
library(gridExtra)
#independent vars
nC <- c("Facebook","GooglePlus", "LinkedIn")
#depended vars
y <- "SentimentHeadline"
plotlist <- list()
n=0
for (j in nC)
  local({ 
        j <- j
        n <- length(plotlist)+1
        xx <- j
        yy <- y 
        xl <- j
        yl <- y
        temp <- dataset[,c(xx,yy,"Topic")]
        temp <- temp[complete.cases(temp),]
        p1 <-  ggplot(data=temp, mapping = aes_string(x=xx,y=yy, color="Topic")) + 
          geom_point() + xlab(xl) + ylab(yl) 
        
        plotlist[[n]] <<- p1
    
        temp <-  NULL
  })
do.call(grid.arrange, plotlist)

#individually loading datasets with pre-processing
preprocess <- function(dataname){
  data <- read_csv(file=dataname)
  year <- str_extract(dataname,pattern = "\\d+") #extracting year from filename
  data <- data[-1,] #removing first row
  data <- data %>%  #adding new col
            mutate(medYear=paste0("20",year))
}

list2env(
  lapply(setNames(myfiles, make.names(gsub("dirname.|.csv$", "", myfiles))), 
         function(x) {preprocess(dataname = x)}), envir = .GlobalEnv)

# loading datasets simultaneouly and merging them together:
combinedata <- do.call(rbind, lapply(myfiles, function(x){preprocess(dataname = x)}))
