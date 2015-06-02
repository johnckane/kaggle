library(XML)
setwd("/home/john/Kaggle/Machine Learning Mania 2015/my data")
base_url <- "http://espn.go.com/mens-college-basketball/bpi/_/season/"


g <- function(year){
  web_page <- readHTMLTable(
    
    paste(base_url,
          year,
          sep=""),
    header = TRUE,
  #colClasses = c("character","character","character",rep("numeric",10)),
  as.data.frame=TRUE,
  stringsAsFactors = FALSE
  )
    
  return(web_page)
}
#format 2012 data first
data_2012 <- data.frame(g(2012))
data_2012[1,]
data_2012 <- data_2012[-1,]
colnames(data_2012) <- c("rank","team","conference","bpi","pva","conf_rank",
                         "w_l","raw","raw_rank","sos","sos_rank","var","var_rank")

data_2012 <- data_2012[-c(21,42,63,84,105,126,147,168,189,210,231,252,273,
                          294,315,336,357),-1]

data_2012$bpi <- as.numeric(data_2012$bpi)
data_2012$pva <- as.numeric(data_2012$pva)
data_2012$conf_rank <- as.numeric(data_2012$conf_rank)
data_2012$raw <- as.numeric(data_2012$raw)
data_2012$raw_rank <- as.numeric(data_2012$raw_rank)
data_2012$sos <- as.numeric(data_2012$sos)
data_2012$sos_rank <- as.numeric(data_2012$sos_rank)
data_2012$var <- as.numeric(data_2012$var)
data_2012$var_rank <- as.numeric(data_2012$var_rank)
write.csv(data_2012,"data_2012.csv")

### Now repeat all this for 2013 and 2014
data_2013 <- data.frame(g(2013))
data_2013[1,]
data_2013 <- data_2013[-1,]
colnames(data_2013) <- c("rank","team","conference","bpi","pva","conf_rank",
                         "w_l","raw","raw_rank","sos","sos_rank","var","var_rank")

data_2013 <- data_2013[-c(21,42,63,84,105,126,147,168,189,210,231,252,273,
                          294,315,336,357),-1]

data_2013$bpi <- as.numeric(data_2013$bpi)
data_2013$pva <- as.numeric(data_2013$pva)
data_2013$conf_rank <- as.numeric(data_2013$conf_rank)
data_2013$raw <- as.numeric(data_2013$raw)
data_2013$raw_rank <- as.numeric(data_2013$raw_rank)
data_2013$sos <- as.numeric(data_2013$sos)
data_2013$sos_rank <- as.numeric(data_2013$sos_rank)
data_2013$var <- as.numeric(data_2013$var)
data_2013$var_rank <- as.numeric(data_2013$var_rank)
write.csv(data_2013,"data_2013.csv")

## Finally for 2014
data_2014 <- data.frame(g(2014))
data_2014[1,]
data_2014 <- data_2014[-1,]
colnames(data_2014) <- c("rank","team","conference","bpi","pva","conf_rank",
                         "w_l","raw","raw_rank","sos","sos_rank","var","var_rank")

data_2014 <- data_2014[-c(21,42,63,84,105,126,147,168,189,210,231,252,273,
                          294,315,336,357),-1]

data_2014$bpi <- as.numeric(data_2014$bpi)
data_2014$pva <- as.numeric(data_2014$pva)
data_2014$conf_rank <- as.numeric(data_2014$conf_rank)
data_2014$raw <- as.numeric(data_2014$raw)
data_2014$raw_rank <- as.numeric(data_2014$raw_rank)
data_2014$sos <- as.numeric(data_2014$sos)
data_2014$sos_rank <- as.numeric(data_2014$sos_rank)
data_2014$var <- as.numeric(data_2014$var)
data_2014$var_rank <- as.numeric(data_2014$var_rank)
write.csv(data_2014,"data_2014.csv")
