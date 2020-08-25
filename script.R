if (!file.exists("data")) {
   dir.create("data")
}
urlfile <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
filename <- "./data/swiftkey_db.zip"



if (!file.exists(filename)) {
   download.file(urlfile, filename, method = "curl")
}


dataset.name <- "final"
if (!file.exists(dataset.name)) {
   unzip(filename)
}


con <- file("./final/en_US/en_US.twitter.txt", "r") 
length(readLines(con))




max(sapply(twittext, nchar))

which(nchar(twittext)==213)
twittext[1484357]


summarydf <- data.frame(
   Object = c("Blog dataset", "News dataset", "Twitter dataset"),
   Nr.of.elements = sapply(list(blogtext, newstext, twittext), length),
   length.longest.element = c(
      max(sapply(blogtext, nchar)),
      max(sapply(newstext, nchar)),
      max(sapply(twittext, nchar))
   ),
   dataset.size = sapply(list(blogtext, newstext, twittext), object.size)
)

grep("biostats", twittext)
twittext[556872]
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twittext))



temp <- c("run running cat")
temp <- tokens_wordstem(tokens(temp), language = "english")
temp[[1]]

profanities <- read.csv("https://www.frontgatemedia.com/new/wp-content/uploads/2014/03/Terms-to-Block.csv", header = F, sep = ",", skip = 4)
profanities[, 2] <- gsub(pattern = ",", replacement = "", profanities[, 2])
profanities <- profanities[, 2]


temp <- dfm_trim(traindfm, min_termfreq = 10, min_docfreq = 2)
plot(temp)


sum(frequencylist$frequency >=100)
sum(frequencylist$frequency >=100) / nrow(frequencylist)

sum(frequencylist$frequency[1:991]) / sum(frequencylist$frequency)

sum(frequencylist$frequency >=40)
sum(frequencylist$frequency >=40) / nrow(frequencylist)

sum(frequencylist$frequency[1:2115]) / sum(frequencylist$frequency)





temp <- tokens_ngrams(train_tokens, n = 2, concatenator = " ")
rm(blogtext)
rm(twittext)
rm(newstext)

rm(train_tokens_2G)
rm(train_tokens_3G)
rm(traindfm)
rm(traindfm_2G)
rm(traindfm_3G)



model_tokens <- sample(model_tokens, 5000)
model_tokens <- paste(model_tokens)
set.seed(1234)
markov_fit_1 <- markovchainFit(model_tokens)



model_tokens_2G <- sample(model_tokens_2G, 5000)
model_tokens_2G <- paste(model_tokens_2G)
set.seed(1234)
markov_fit_2 <- markovchainFit(model_tokens_2G)


set.seed(1234)
model_tokens_3G <- sample(model_tokens_3G, 1000)
model_tokens_3G <- paste(model_tokens_3G)
set.seed(1234)
markov_fit_3 <- markovchainFit(model_tokens_3G)


markov_fit_3$estimate["who is my",]

markov_fit_2$estimate["i am"]


predictive_text <- function(text){
   text <- tolower(text)
   text <- tokens(text)
   text <- tokens_select(text, keeplist, selection = "keep")
   text <- paste(text)
   
   suggest <- markov_fit_1$estimate[ text, ] %>%
      sort(decreasing = T) %>% 
      head(3) 
   
   suggest <- suggest[suggest > 0] %>% 
      names()
   
   return(suggest)
}

predictive_text(predt)


predt <- "Very early observations on the Bills game: Offense still struggling but the"


head(markov_fit_2$estimate[predt], 3)
