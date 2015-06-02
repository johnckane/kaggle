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
#pull 2015 data, this was done on March 12, just in case I can't pull it later.
data_2015 <- data.frame(g(2015))
data_2015[1,]
data_2015 <- data_2015[-1,]
colnames(data_2015) <- c("rank","team","conference","bpi","pva","conf_rank",
                         "w_l","raw","raw_rank","sos","sos_rank","var","var_rank")

data_2015 <- data_2015[-c(21,42,63,84,105,126,147,168,189,210,231,252,273,
                          294,315,336,357),-1]

data_2015$bpi <- as.numeric(data_2015$bpi)
data_2015$pva <- as.numeric(data_2015$pva)
data_2015$conf_rank <- as.numeric(data_2015$conf_rank)
data_2015$raw <- as.numeric(data_2015$raw)
data_2015$raw_rank <- as.numeric(data_2015$raw_rank)
data_2015$sos <- as.numeric(data_2015$sos)
data_2015$sos_rank <- as.numeric(data_2015$sos_rank)
data_2015$var <- as.numeric(data_2015$var)
data_2015$var_rank <- as.numeric(data_2015$var_rank)
write.csv(data_2015,"data_2015_16MAR2015.csv")

