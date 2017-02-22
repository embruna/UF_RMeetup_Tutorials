install.packages("pdftools")
library(pdftools)
library(stringdist)
library(tidyr)
library(dplyr)
txt <- pdf_text("2013-Marine_Ecology.pdf")
txt <- pdf_text("AmNat Jan 2006.pdf")


str(txt)

df <- data.frame(x =  pdf_text("2013-Marine_Ecology.pdf"))

df <- data.frame(x =  pdf_text("AmNat Jan 2006.pdf"))
df %>% separate(x, c("A", "B"),extra = "merge", fill = "left")
df$x<-as.character(df$x)
s <- strsplit(df$x, split = " ")
s<-as.data.frame(s)
summary(s)
colnames(s) <- c("x")
which(s$x=="")
s<- as_data_frame(s[-which(s$x==""), ])



s <- na.omit(s)
txt2<-separate(df, name, c("1", "2"), sep = " ", remove = FALSE, convert = FALSE, extra = "merge", fill = "right")


