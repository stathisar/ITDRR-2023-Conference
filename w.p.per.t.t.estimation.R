rm(list = ls())
setwd("/media/")
require("textmineR")
require("qdapRegex")
require("stringi")
require("stringr")

#w1
#load("/media/stathis/MAXTOR/data.ref/lda/models/models.final/final/702.3.gram.2k.iter")
#70
load("/media/stathis/MAXTOR/data.ref/lda/models/models.final/final/12.3.gram.2k.iter")


model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
s1 = as.data.frame(model$phi)

grams = as.data.frame(matrix(nrow = NCOL(model$top_terms), ncol = 22))

names(grams) = c("t_","t_t", "p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","w1","w2","w3","w4",
                 "w5","w6","w7","w8","w9","w10")


data <- read.csv("/media/stathis/MAXTOR/data.ref/lda/final/data.merged.14.7.23.1021.csv")
m.list <- list.files("/media/stathis/MAXTOR/data.ref/lda/models/models.final/final/")
m.list <- as.data.frame(m.list)
m.list$week <- NA
m.list$week <- gsub("2.3.gram.2k.iter", "", m.list$m.list)
m.list$week <- as.numeric(m.list$week)
m.list$week[m.list$week > 18] <- paste("2015 week ", 52:1)

m.list$week[!grepl("week", m.list$week)][11:18] <- paste("2016 week ", 69:62)
m.list$week[!grepl("week", m.list$week)][3] <- paste("2016 week ",70)
m.list$week[!grepl("week", m.list$week)] <- paste("2016 week ", 61:53)
m.list$week <- gsub("2015 week  ", "week", m.list$week)
m.list$week <- gsub("2016 week  ", "", m.list$week)
x =  m.list$week[!grepl("week", m.list$week)]
m.list$week[!grepl("week", m.list$week)] = paste0("week",as.numeric(x) - 52,"/16")
write.csv(m.list, "/media/stathis/MAXTOR/data.ref/lda/models/models.final/final.list.15.7.23.csv")  

#i = 1

#c = s[[1]][i]
#c <- str_trim(c)
#s1["t_7" ,c]
#j = 1

grams.total = as.data.frame(matrix(nrow = 0, ncol = 23))
names(grams.total)[1:22] <- c("t_","t_t", "p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","w1","w2","w3","w4",
                              "w5","w6","w7","w8","w9","w10")
names(grams.total)[23] <- c("week")

values <- unique(data$week)
data <- read.csv("/media/stathis/MAXTOR/data.ref/lda/final/data.merged.14.7.23.1021.csv")
k = 2
for(k in 1:length(values)){
 #import model
  path = paste0("/media/stathis/MAXTOR/data.ref/lda/models/models.final/final/",m.list$m.list[m.list$week == values[k]])
  load(path)
  
  #grams df
  model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
  s1 = as.data.frame(model$phi)
  grams = as.data.frame(matrix(nrow = NCOL(model$top_terms), ncol = 22))
  names(grams) = c("t_","t_t", "p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","w1","w2","w3","w4",
                   "w5","w6","w7","w8","w9","w10")

  #grams f 1,2
  grams$t_ = data$topic[data$week == c(values[k])]
  grams$t_t = data$top_terms[data$week == c(values[k])]

for(j in 1:NROW(grams)){
  s = str_split(grams$t_t[j], ",")
  for(i in 1:10){
    c = s[[1]][i]
    c <- str_trim(c)
    #grams$t_[i]
    grams[j,2+i] <- s1[grams$t_[j] ,c]
    rm(c)
  }
  
}
  grams$week <- values[k]
  grams.total <- rbind(grams.total, grams)  
  rm(grams)
}


for(i in 1:NROW(grams.total)){
  sum.v <- sum(grams.total[i, 3:12])
  grams.total[i, 13] <- as.numeric(grams.total[1, 3])/sum.v
  grams.total[i, 14] <- as.numeric(grams.total[1, 4])/sum.v
  grams.total[i, 15] <- as.numeric(grams.total[1, 5])/sum.v
  grams.total[i, 16] <- as.numeric(grams.total[1, 6])/sum.v
  grams.total[i, 17] <- as.numeric(grams.total[1, 7])/sum.v
  grams.total[i, 18] <- as.numeric(grams.total[1, 8])/sum.v
  grams.total[i, 19] <- as.numeric(grams.total[1, 9])/sum.v
  grams.total[i, 20] <- as.numeric(grams.total[1, 10])/sum.v
  grams.total[i, 21] <- as.numeric(grams.total[1, 11])/sum.v
  grams.total[i, 22] <- as.numeric(grams.total[1, 12])/sum.v
  
}
data$id <- rownames(data)
grams.total$id <- rownames(grams.total)
names(data)
final.output <- merge(data[ ,c(1,4,5,6,7,8)], grams.total, by = "id")

write.csv(final.output, "/media/stathis/MAXTOR/data.ref/lda/final/w.p.estimated.merge.15.july.2023.csv")
