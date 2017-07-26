if (!require("pacman")) install.packages("pacman")
##Load Packages
pacman::p_load(pdftools, tesseract, stringr, tm, dplyr, tidytext, hunspell, parallel, stringdist, tidyr, sqldf, doSNOW, doParallel, foreach)

##Create relevant functions
#Assign 10 leading words
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
#Find Datapoint key per word and answer per Datapoint key
findKeyNamedAll <- function(Keyword, KeyAnswerx){
  df <- text_words
  df$Key <- NA
  df$Key <-  sapply(text_words$word, function(x){agrep(Keyword, x, max.distance = 1, ignore.case = TRUE)})
  df$Key <- sapply(df$Key, function(x){ifelse(x==1, Keyword, x)})
  df$Key <- ifelse(grepl('logical', df$Key), '', Keyword)
  
  text_words <- df
  assign('text_words',text_words,envir=.GlobalEnv)
  
  
  #Search next 10 words for answers for the relevant datapoint
  text_wordsGather1 <- text_words %>% filter(Key != '') %>% gather(WordPosition, WordValue, word1:word10)
  if(length(text_wordsGather1$Key) > 0) {
    df <- text_wordsGather1
    df$KeyAnswer <- NA
    df$KeyAnswer <-  sapply(df$WordValue, function(x){agrep(KeyAnswerx, x, max.distance = 1, ignore.case = TRUE) & grepl(Keyword, df$Key)})
    df$KeyAnswer <- ifelse(grepl('logical', df$KeyAnswer), '', df$WordValue)
    x <- df %>% filter(KeyAnswer != '')
    x1 <- data.frame()
    x1 <- if(length(x$KeyAnswer) == 0) {
    x1 <- sqldf("select line, word, Key, 'No Match' as WordPosition, group_concat(WordValue) as WordValue, 'No Match' as KeyAnswer from df group by line, word, Key")
    } else {
      x1 <- x
    }
    x1$KeyLookup <- KeyAnswerx
    text_wordsGather <- rbind(text_wordsGather, x1)
    assign('text_wordsGather',text_wordsGather,envir=.GlobalEnv)
  }
}


##Keyword List
keydf <- data.frame(Key = c('beruf', 'beruf', 'tätigkeit', 'tatigkeit', 'familienstand', 'familienstand', 'wohneigentum', 'stellplatz'), KeyAnswer = c('Angestellt', 'Rentner', 'Rentner', 'Rentner', 'single', 'verheiratet', 'selbst', 'privat'))
text_wordsGather <- data.frame(line = NA, word = NA, Key = NA, WordPosition = NA, WordValue = NA, KeyAnswer = NA, KeyLookup = NA)



##OCR Config
tesseract_download("deu", datapath = NULL, progress = TRUE)
german <- tesseract("deu")

#Scan & Process PDF
ScanFileParallel <- function(filename){
  bitmapPDF <- list()
  textPDF <- list()
  registerDoParallel()  
  bitmapPDF <-  foreach(i = 1:pdf_info(filename)$pages) %dopar% (pdf_render_page(filename, page = i, dpi = 500))
  foreach(i = 1:pdf_info(filename)$pages) %dopar% (png::writePNG(bitmapPDF[[i]], paste0("scanPage",i,".png"), dpi = 500))
  textPDF <- foreach(i = 1:pdf_info(filename)$pages) %dopar% (ocr(paste0("scanPage",i,".png"), engine = german))
  
  ##Write text into txt file to be used later for Corpus
  for(i in 1:length(textPDF)) {
    write.table(textPDF[i], paste0('textPDF', i, '.txt'), row.names=F,col.names=F)
  }
  
  ##Convert text into corpus
  corpusList <- list()
  
  for(i in 1:length(textPDF)) {
    corpusList[[i]] <- paste(readLines(paste0("textPDF",i,".txt")))
  }
  
  ##Convert Corpus to dataframe
  text_df <- data.frame()
  for(i in 1:length(corpusList)) {
    if(i == 1){
      text_df <- data_frame(line = length(corpusList[[i]]), text = corpusList[[i]])
    } else {
      text_df <- rbind(text_df, data_frame(line = length(corpusList[[i]]), text = corpusList[[i]]))
    }
  }
  text_df$line = filename
  
  ##Split into single words
  text_words <- text_df %>%
    unnest_tokens(word, text)
  
  #Assign 10 leading words
  #shift.vec <- function (vec, shift)
  text_words$word1 <- shift.vec(text_words$word, -1)
  text_words$word2 <- shift.vec(text_words$word, -2)
  text_words$word3 <- shift.vec(text_words$word, -3)
  text_words$word4 <- shift.vec(text_words$word, -4)
  text_words$word5 <- shift.vec(text_words$word, -5)
  text_words$word6 <- shift.vec(text_words$word, -6)
  text_words$word7 <- shift.vec(text_words$word, -7)
  text_words$word8 <- shift.vec(text_words$word, -8)
  text_words$word9 <- shift.vec(text_words$word, -9)
  text_words$word10 <- shift.vec(text_words$word, -10)
  assign('text_words',text_words,envir=.GlobalEnv)
  #text_wordsGather <- data.frame(line = NA, word = NA, Key = NA, WordPosition = NA, WordValue = NA, KeyAnswer = NA, KeyLookup = NA)
  
  ######
  #findKeyNamedAll <- function(Keyword, KeyAnswerx)
  
  for (i in 1:length(keydf$Key)) {
    findKeyNamedAll(as.character(keydf[i,1]), as.character(keydf[i,2]))
  }
}


#Run OCR function on PDFs
for (i in 1:length(pdfVec)){
  ScanFileParallel(pdfVec[i])
}

#View Final Results
View(text_wordsGather)


