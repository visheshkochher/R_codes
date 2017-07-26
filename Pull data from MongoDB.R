


library(mongolite)
library(tidyr)
library(dplyr)
library(sqldf)

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 
#Get Keys and value type per question
#####

#Keys
mongo_AccountProperties <- mongo(collection= "AccountProperties", 
                                 db = "offer_process", 
                                 url = "mongodb://wilfried.dalvai:7EkkYWoTzEDHMrvjxpXd@mongodb001.dc001.knip.ag:27017/offer_process", 
                                 verbose = TRUE)

question_key <- mongo_AccountProperties$find('{"properties.key" : {"$ne":""}}', fields = '{"properties.question":1, "properties.key":1, "properties.answer":1, "_id":0, "accountId":1, "createdAt":1}')
question_key <- unnest(question_key)
question_key_grouped <- sqldf("select question, key, group_concat(distinct answer) as all_answers from question_key group by question, key")


#value type
mongo_NeedsAnalysis <- mongo(collection= "CollectionName", 
                             db = "dbname", 
                             url = "mongodb://URL", 
                             verbose = TRUE)

question_type <- mongo_NeedsAnalysis$find('{"questions.type" : {"$ne":""}}', fields = '{"questions.question":1, "questions.type":1, "_id":0}')
question_type <- unnest(question_type)
question_type_grouped <- sqldf("select question, type from question_type group by question, type")


# Get data with nested values
nested_selectable_answers <- mongo_offer$find('{"values.selected":true}', fields = '{"question":1, "_id":0, "accountId":1, "values.selected":1, "values.label":1, "answerTime":1, "key" :1, "type":1}')
selectable_answers <- unnest(nested_selectable_answers)
selectable_answers <- selectable_answers %>% filter(selected=="TRUE")
selectable_answers$answer <- selectable_answers$label
selectable_answers <- selectable_answers[c("accountId", "question", "answer", "answerTime", "key", "type")]%>%mutate(answerType = type)
selectable_answers <- plyr::rename(selectable_answers, c(accountId="account_id"))


# Get data with non-nested answers for query
numeric_answers <- mongo_offer$find('{"answer.type":"numeric"}', fields = '{"question":1, "_id":0, "accountId":1, "answer.numeric":1, "values.label":1, "answerTime":1, "key" :1, "type":1, "answer.type":1}')
df <- as.data.frame(numeric_answers$answer)
df <- plyr::rename(df, c(type="answerType"))
numeric_answers <- cbind(numeric_answers, df)
numeric_answers <- numeric_answers[c("accountId", "question", "numeric", "answerTime", "key", "type", "answerType")]
numeric_answers <- plyr::rename(numeric_answers, c(numeric="answer", accountId="account_id"))
