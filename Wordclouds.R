if (!require("pacman")) install.packages("pacman")
pacman::p_load(RMySQL, dplyr, data.table, googlesheets, readr, tidyr, digest, readr, caret, ggplot2, jsonlite, tidyjson, knn, kohonen, car, sqldf, RSQLite,RXKCD,tm, wordcloud, RColorBrewer)



mydbgrizzly = dbConnect(MySQL(), user='username', password='password',
                        dbname='dbname', host='hostURL')
comments <- dbGetQuery(mydbgrizzly, "
                       
                       select
                       
                       t.id, t.description
                       
                       from Table t
                       where t.description is not null
                       limit 1000
                       ")


path <- system.file("xkcd", package = "RXKCD")
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
xkcd.corpus <-Corpus(DataframeSource(data.frame(comments[, 2])))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, content_transformer(tolower))
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("german")))
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordclouddescription.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()
