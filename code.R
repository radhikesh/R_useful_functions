###
# remove rows where all column values are NA
# ref: https://www.r-bloggers.com/2021/06/remove-rows-that-contain-all-na-or-certain-columns-in-r/
df[rowSums(is.na(df)) != ncol(df), ]

### Applying a same function across multiple columns
table_1 <- table_1 %>% mutate(across(.cols = c(x,y,z,w), ~ifelse(.x==1,"x","")))
table_1 <- table_1 %>% mutate(across(.cols = starts_with("width"), ~ifelse(.x==1,"x","")))

### converting all factor variable to numeric 

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
### or another way to convert all numeric to character 
df %>% mutate(across(where(is.numeric), round, 3))

## Apply a same function to multiple columns, here I'm converting strings as blank in character column to Missing using zap_empty
library(haven)
i <- sapply(bob, is.character)
bob[i] <- lapply(bob[i], zap_empty)

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

# for creating one-way freq tab using flextable
library(flextable)
library(dplyr)
freqtab <- function(data, colname, newcolname) {
df <- data %>% 
      select({{colname}}) %>% 
      dplyr::group_by({{colname}}) %>% 
      summarise(Freq = n(), Percentage = round(n()/nrow({{data}})*100,2)) 

df1 <- data.frame(colname="Total","Freq"=sum(df$Freq),"Percentage"=round(sum(df$Percentage,na.rm = T)))
colnames(df)[1] <- newcolname
colnames(df1)[1] <- newcolname
df <- df %>% bind_rows(df1)
ft <- flextable(df)
ft <- colformat_char(x=ft, na_str = "Missing")
autofit(ft) 
}

# for creating cross-tab/contingency table using flextable:
crosstabfunc <- function(data, col1, col2, newcol1name) {
  
  pchisq <- chisq.test(table(data[[col1]], data[[col2]]))
  
  if (any(pchisq$expected<5)) {
    pfisher <- fisher.test(table(data[[col1]], data[[col2]]))
    if (pfisher$p.value < 0.001) {
      pvalue <- paste("P-value= <0.001","(Fisher's Exact Test)")
    } else{
    pvalue <- paste("P-value=",round(pfisher$p.value,3),"(Fisher's Exact Test)")}
    
  } else{
    
    if (pchisq$p.value < 0.001) { 
      pvalue <- paste("P-value= <0.001","(Chi-Square Test)")
    } else {
      pvalue <- paste("P-value=",round(pchisq$p.value,3),"(Chi-Square Test)")}
  }
  
  #ctab for getting both proportions and frequencies
  output <- ctable(data[[col1]], data[[col2]], prop = "c")
  colnames(output$cross_table)[colnames(output$cross_table)=="<NA>"] <- "Missing"
  colnames(output$proportions)[colnames(output$proportions)=="<NA>"] <- "Missing"

  
  ct_df <- as.data.frame(output$cross_table)
  ct_df <- spread(ct_df, key = 2, value = Freq)
  pro_df <- as.data.frame(output$proportions)
  pro_df <- pro_df %>% mutate_if(is.numeric, ~.x*100)  %>%mutate_if(is.numeric, round,2)
  pro_df <- pro_df %>%mutate_if(is.numeric, as.character)
  ct_df <-  ct_df %>%mutate_if(is.numeric, as.character)
  pro_df <- pro_df %>%mutate_if(is.character, ~paste0(.x, "%"))
  # 
  col_name_pro <- colnames(pro_df)
  ct_pro_df <- map(col_name_pro, ~ paste0(ct_df[[.x]], " (",pro_df[[.x]], ")"))
  ct_pro_df <- as.data.frame(do.call("cbind", ct_pro_df))
  colnames(ct_pro_df) <- col_name_pro
  ct_pro_df <- cbind(ct_df[[1]],ct_pro_df)
  colnames(ct_pro_df) <- colnames(ct_df)
  colnames(ct_pro_df)[1] <- newcol1name
  ct_pro_df[[newcol1name]]  <- as.character( ct_pro_df[[newcol1name]])
  ct_pro_df[[newcol1name]] <- ifelse(ct_pro_df[[newcol1name]]=="<NA>", "Missing",ct_pro_df[[newcol1name]])
  
  
  ### removing percentage for missings in rows
  missing_rownum <- ct_pro_df[[newcol1name]]
  
  index <- which(missing_rownum=="Missing")
  if (!is.null(index)) {
  for ( i in 2:ncol(ct_pro_df))
  {
    ct_pro_df[index,i] <- str_replace_all(ct_pro_df[index,i],"\\s.*","")
  }
  }
  
  ### removing percentage for missings in col
  missing_colnum <- colnames(ct_pro_df)
  index <- which(missing_colnum=="Missing")
  if (!is.null(index)) {
    for ( i in 1:nrow(ct_pro_df))
    {
      ct_pro_df[i,index] <- str_replace_all(ct_pro_df[i,index],"\\s.*","")
    }
  }
  
  if (any(is.na(data[[col2]])) & is.factor(data[[col2]]))
  {
    col_keys <- c(newcol1name,as.character(levels(data[[col2]][!is.na(data[[col2]])])),"Missing","Total")
  } else if (any(is.na(data[[col2]])) & is.character(data[[col2]])) {
    
    col_keys <- c(newcol1name,as.character(unique(data[[col2]][!is.na(data[[col2]])])),"Missing","Total")
    
  } else if (is.factor(data[[col2]])) { 
    col_keys <- c(newcol1name,as.character(levels(data[[col2]])),"Total")
  } else {
    
    col_keys <- c(newcol1name,as.character(unique(data[[col2]])),"Total")
    
  }
  
  ft <- flextable(ct_pro_df,col_keys=col_keys) %>% 
        add_footer_lines(values = pvalue) %>% 
        colformat_char(na_str="Missing") %>% autofit() %>%  theme_box()
  ft
  
}

## convert a number to date format
data_temp$year.start <- as.Date(data_temp$year.start, origin = "1900-01-01")

## Rename multiple columns at once by removing pattern
## https://dplyr.tidyverse.org/reference/rename.html
data <- data %>%
        rename_with(.fn = ~str_remove_all(string = .x, pattern = "pattern"))

## unzip or extract files from .tar extension
untar(tarfile = "C:/Users/Downloads/bundle-54.tar.gz", exdir = "C:/Users/Downloads/test2/")

## calculate rowwise sum using dplyr:
df <- df %>%  rowwise() %>% mutate(s = sum(dplyr::c_across(a:e)))

## Add row to a data frame with total sum for each column
df <- df %>% bind_rows(summarise(., across(where(is.numeric), sum), across(where(is.character), ~"Total")))


