library(shiny)


shinyServer(function(input, output) {
    
    Sys.setenv(LANG = "en")
    
    library(quanteda)
    library(data.table)
    library(stringi)
    library(stringr)
    
    load(url("https://github.com/Anvil-Late/Next-Word-Predictor/raw/master/data/bifreq.RData"))
    load(url("https://github.com/Anvil-Late/Next-Word-Predictor/raw/master/data/trifreq.RData"))
    load(url("https://github.com/Anvil-Late/Next-Word-Predictor/raw/master/data/unifreq.RData"))
    load(url("https://github.com/Anvil-Late/Next-Word-Predictor/raw/master/data/profanities.RData"))
    
    ngram_inlist_getter <- function(wordinput, ngram_freq){
        process_word <- sprintf("%s%s%s", "^", wordinput, "_")
        # this transforms "word_input" into "^word_input_" so we can use it in grep
        ngram_freq[grep(pattern = process_word,
                        ngram_freq[, word],
                        perl = T,
                        useBytes = T)]
    }
    
    ngram_out_unigetter <- function(inlist_ngrams, N){
        unigrams_in_inlist_ngrams <- str_split_fixed(inlist_ngrams[, word], "_", N)[, N]
        # This gets all the unigrams that are the last words in our ngram list
        # If we remove them from our unigram list, we only end up with unigrams that
        # would constitute new ngrams
        return(data.table(
            word = unifreq[!unigrams_in_inlist_ngrams, word, on = "word"]
        ))
    }
    
    inlist_prob <- function(ngram_inlist, n_minus_gram, wordinput){
        # ngram_inlist : the list given by ngram_inlist_getter()
        # n_minus_gram : the N-1 Grams list
        # wordinput : the provided n-1 gram
        pattern_count <- n_minus_gram[wordinput, count, on=.(word)]
        ngram_inlist[, prob := ngram_inlist[, adj_count] / pattern_count]
    }
    
    alpha_numerator <- function(ngram_inlist, n_minus_gram, wordinput){
        if (dim(ngram_inlist)[1] == 0){
            return(1)
        } else {
            return(
                sum(ngram_inlist[, count - adj_count] / n_minus_gram[wordinput, count, on = .(word)]))
        }
    }
    
    nextword_getter <- function(userinput, noresult = 3){
        userinput <- gsub(" ", "_", userinput)
        
        if (length(which(bifreq$word == userinput)) > 0){ # if bigram found in our list
            # retrieve trigrams in our list that match the input :
            
            trigram_inlist <- ngram_inlist_getter(userinput, trifreq)
            userinput_word2 <- str_split_fixed(userinput, "_", 2)[, 2]
            # retrieve bigrams in our list that match the last word of the input
            bigram_inlist <- ngram_inlist_getter(userinput_word2, bifreq)
            # retrieve all unigrams that would constitute unobserved bigrams
            big_out_uni <- ngram_out_unigetter(bigram_inlist, 2)
            # Exclude bigrams in list that are in trigrams in list (no redudancy)
            bigram_inlist <- bigram_inlist[
                !str_split_fixed(trigram_inlist[, word], "_", 2)[, 2], on = "word"
            ]
            # calculate probabilities of trigrams in list
            trigram_inlist <- inlist_prob(trigram_inlist, bifreq, userinput)
            # Calculate Katz alpha numerator for the bigram input
            input_alpha <- alpha_numerator(trigram_inlist, bifreq, userinput)
            # Calculate probabilities of bigrams in list
            bigram_inlist <- inlist_prob(bigram_inlist, unifreq, userinput_word2)
            # Calculate Katz alpha numerator for last word of input
            input_word2_alpha <- alpha_numerator(bigram_inlist, unifreq, userinput_word2)
            # Calculate maximum likelihood for unigrams in bigrams not in list 
            big_out_uni[, prob := unifreq[big_out_uni, count, on = .(word)] / 
                            unifreq[big_out_uni, sum(count), on = .(word)]
            ]
            big_out_uni[, prob := input_alpha * input_word2_alpha * prob]
            
            # only keep "word" and "prob" columns in trigram_inlist and bigram_inlist
            # and process the words in a readable manner
            
            trigram_inlist[, c("count", "adj_count") := NULL]
            trigram_inlist[, word := str_remove(trigram_inlist[, word], "([^_]+_)+")]
            
            bigram_inlist[, c("count", "adj_count") := NULL]
            bigram_inlist[, word := str_remove(bigram_inlist[, word], "([^_]+_)+")]
            
            # multiply bigram_inlist probabilities by alpha 
            bigram_inlist[, prob := input_alpha * prob]
            
            # Combine all probabilities in order into a single data table
            all_probs <- setorder(rbind(trigram_inlist, bigram_inlist, big_out_uni), -prob)
            
            # return list of results, only the top ones according to the noresult input,
            # 3 by default. 
            # If less than the demanded amount results, print as many results as there are
            # (dim(all_probs)[1])
            return(all_probs[prob != 0][1:min(dim(all_probs[prob != 0])[1], noresult)])
        } else { # if bigram input not in list
            userinput_word2 <- str_split_fixed(userinput, "_", 2)[2]
            
            if (length(which(unifreq$word == userinput_word2)) >0){# if last word found
                # in unigram list
                
                # retrieve all bigrams in list beginning with last word of input
                bigram_inlist <- ngram_inlist_getter(userinput_word2, bifreq)
                # Calculate probabilities of bigrams in list
                bigram_inlist <- inlist_prob(bigram_inlist, unifreq, userinput_word2)
                # Calculate alpha
                input_word2_alpha <- alpha_numerator(bigram_inlist, unifreq, userinput_word2)
                
                # retrieve all unigrams that would constitute new bigrams :
                big_out_uni <- ngram_out_unigetter(bigram_inlist, 2)
                # calculate maximum likelihood for unigrams in bigrams not in list :
                big_out_uni[, prob := unifreq[big_out_uni, count, on = .(word)] / 
                                unifreq[big_out_uni, sum(count), on = .(word)]
                ]
                big_out_uni[, prob := input_word2_alpha * prob]
                
                # only keep "word" and "prob" columns in bigram_inlist
                # and process the words in a readable manner
                bigram_inlist[, c("count", "adj_count") := NULL]
                bigram_inlist[, word := str_remove(bigram_inlist[, word], "([^_]+_)+")]
                
                all_probs <- setorder(rbind(bigram_inlist, big_out_uni), -prob)
                return(all_probs[prob != 0][1:noresult])
                
                
            } else { # if last word not found in unigram list
                # We call the maximum likelihood
                return(setorder(unifreq, -adj_count)[1:noresult, .(word, 
                                                                   prob = adj_count / 
                                                                       unifreq[, sum(count)])])
            }
        }
    }
    
    nextword_preproc <- function(wordinput){
        names(wordinput) <- NULL
        processed_input <- tokens(wordinput, remove_numbers = T,
                                  remove_punct = T, remove_symbols = T, split_hyphens = T,
                                  remove_url = T, remove_twitter = T)
        processed_input <- tokens_select(processed_input, stopwords(), selection = "remove")
        processed_input <- tokens_select(processed_input, profanities, selection = "remove")
        processed_input <- tokens_tolower(processed_input)
        
        return(paste(tail(processed_input[[1]], 2), collapse = "_"))
        
    }
    
    nextword <- function(userinput, noresult=5){
        userinput_bigram <- nextword_preproc(userinput)
        answer <- nextword_getter(userinput_bigram, noresult = noresult)
        if (dim(answer)[1] == 0){
            return("no prediction found")
        }
        return(answer)
    }
    
    user_text <- reactive({as.character(input$usertext)})
    

    predictions <- reactive({nextword(user_text())})
    
    dimpred <- reactive({dim(predictions())[1]})

    prediction1 <- reactive({
        if(user_text() == ""){
            ""
        }else{
            as.character(predictions()[1,1])
        }
        })
    prediction2 <- reactive({
        if(user_text() == ""){
            ""
        }else if(dimpred() < 2){
            NULL
        }else{
            as.character(predictions()[2,1])
        }
    })
    prediction3 <- reactive({
        if(user_text() == ""){
            ""
        }else if(dimpred() < 3){
            NULL
        }else{
            as.character(predictions()[3,1])
        }
    })
    prediction4 <- reactive({
        if(user_text() == ""){
            ""
        }else if(dimpred() < 4){
            NULL
        }else{
            as.character(predictions()[4,1])
        }
    })
    prediction5 <- reactive({
        if(user_text() == ""){
            ""
        }else if(dimpred() < 5){
            NULL
        }else{
            as.character(predictions()[5,1])
        }
    })
    
    output$predictedword1 <- renderText({prediction1()})
    output$predictedword2 <- renderText({prediction2()})
    output$predictedword3 <- renderText({prediction3()})
    output$predictedword4 <- renderText({prediction4()})
    output$predictedword5 <- renderText({prediction5()})
    
    uicodeurl <- a("Github link to UI code", 
                   href="https://github.com/Anvil-Late/Next-Word-Predictor/blob/master/NextWordPredictor/ui.R")
    servercodeurl <- a("Github link to Server and computation code",
                       href="https://github.com/Anvil-Late/Next-Word-Predictor/blob/master/NextWordPredictor/server.R")
    output$uicodelink <- renderUI({
        tagList("", uicodeurl)
    })
    output$servercodelink <- renderUI({
        tagList("", servercodeurl)
    })
    

})

