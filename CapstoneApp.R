#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tm)
library(RWeka)
library(stringr)

corpus <- VCorpus(DirSource(directory="final/en_US", encoding = "UTF-8"), 
                  readerControl = list(language = "en"))

set.seed (2020)

n_samples = 1000

corpus[[1]]$content <- sample(corpus[[1]]$content, n_samples, replace = TRUE)
corpus[[2]]$content <- sample(corpus[[2]]$content, n_samples, replace = TRUE)
corpus[[3]]$content <- sample(corpus[[3]]$content, n_samples, replace = TRUE)

clean_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

clean_data <- function(corpus_data) {
    #corpus_data <- tm_map(corpus_data, tolower)
    corpus_data <- tm_map(corpus_data, removeWords, stopwords("english"))
    corpus_data <- tm_map(corpus_data, removePunctuation)
    corpus_data <- tm_map(corpus_data, removeNumbers)
    corpus_data <- tm_map(corpus_data, clean_space, "(f|ht)tp(s?)://(.*)[.][a-z]+")
    corpus_data <- tm_map(corpus_data, clean_space, "@[^\\s]+")
    corpus_data <- tm_map(corpus_data, stripWhitespace)
    return(corpus_data)
}

corpus <- clean_data(corpus)

merged_corpus <- c(corpus[[1]]$content, 
                   corpus[[2]]$content, 
                   corpus[[3]]$content)

data_dtm <- DocumentTermMatrix(VCorpus(VectorSource(merged_corpus)))
data_dtms <- removeSparseTerms(data_dtm , 0.99)
data_freq <- sort(colSums(as.matrix(data_dtms)), decreasing=TRUE)

n_gram <- function(data, n) {
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    n_gram_data <- DocumentTermMatrix(data, control=list(tokenize=tokenizer))
    return(n_gram_data)
}

ng1 <- n_gram(VCorpus(VectorSource(merged_corpus)), 1)
ng2 <- n_gram(VCorpus(VectorSource(merged_corpus)), 2)
ng3 <- n_gram(VCorpus(VectorSource(merged_corpus)), 3)

predict_word <- function(words) {
    words <- tolower(words)
    words <- str_replace_all(words, "[^a-zA-Z\\s]", " ")
    words <- str_replace_all(words,"[\\s]+", " ")
    words <- str_split(words, " ")[[1]]
    indexes <- which(words == "")
    if(length(indexes) > 0){
        words <- words[-indexes]
    } 
    if (length(words) > 1) {
        text = words[length(words)]
        res <- findAssocs(ng1, text, corlimit = 0.1)
        if (length(res[[1]])>1) {
            result <- sort(res[[1]], decreasing = TRUE)[1:min(3, length(res[[1]]))]
            return(paste(names(result), sep=" "))
        }  else {
            return("")
        }
        
    } else {
        return("")
    }
    return(words)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Capstone Project by Roshan"),

    sidebarLayout(
        sidebarPanel(
            textInput("userInput",
                        "Enter a text Phrase:
                      For example: How are you"),
            h6("Click to predict the next words:"),
            actionButton("predict", "Predict Text")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Text phrase entered by user:"),    
           textOutput("userText"),
           h3("Predicted words:"), 
           textOutput("predictedWord")
        )
    )
)

server <- function(input, output, session) {
    
    ntext <- eventReactive(input$predict, {
        predict_word(input$userInput)
    })

    output$userText <- renderText({
        input$userInput
    })
    
    output$predictedWord <- renderText({
        ntext()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
