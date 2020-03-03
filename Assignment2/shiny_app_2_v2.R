library(tm)
library(wordcloud)
library(shiny)
library(memoise)
library(dplyr)
library(ggplot2)

# load data

data <- read.csv(file.choose())


airlines <- c("Southwest",
               "United",
               "Delta",
               "US Airways",
               "American",
              "Virgin America")

va <- data %>%
  filter(airline == "Virgin America")

un <- data %>%
  filter(airline == "United")

sw <- data %>%
  filter(airline == "Southwest")

dl <- data %>%
  filter(airline == "Delta")

us <- data %>%
  filter(airline == "US Airways")

am <- data %>%
  filter(airline == "American")


getTermMatrix <- memoise(function(airline) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(airline %in% airlines))
    stop("Unknown airline")
  
  if (airline  == "Virgin America") {
    text <- va$text
  }
  
  if (airline  == "United") {
    text <- un$text
  }
  
  if (airline  == "Southwest") {
    text <- sw$text
  }
  
  if (airline  == "Delta") {
    text <- dl$text
  }
  
  if (airline  == "US Airways") {
    text <- us$text
  }
  
  if (airline  == "American") {
    text <- am$text
  }

  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "united", "flight", 
                      "virginamerica", "southwestair", "american", "jetblue", "usairways", "americanair"))
  
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

#word frequency dataframe function  
getConditionedDataFrame <- function(terms) {
  #create the term matrix
  # calculate the frequency of words and sort it by frequency
  word.freq <- subset(terms, terms >= 25)
  df <- data.frame(term = names(word.freq), freq = word.freq)
  return(df)
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Assignment 2"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose an Airline:",
                  choices = airlines),
      actionButton("update", "Go"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 5),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 50)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot"),
      plotOutput("freqPlot"),
      plotOutput("corrPlot")
    )
  )
)



server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  
  terms_2 <- reactive({
    input$update
    isolate({
      getConditionedDataFrame(terms())
    })
  })
  
  #TDM/DTM for topic modeling
  myCorpus <- Corpus(VectorSource(data$text))
  myCorpus <-  tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "united", "flight", 
                      "virginamerica", "southwestair", "american", "jetblue", "usairways", "americanair"))
  tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
  dtm <- as.DocumentTermMatrix(tdm)
  
  
  #word cloud plot
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(3.5,0.25),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  #word frequency barplot
  hues <- c(60:330)
  output$freqPlot <- renderPlot({
    v <- terms_2()
    ggplot(v, aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) +
      geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + ylab("Count") + 
      scale_fill_hue(c = sample(hues, 1)) + 
      theme(axis.text.x = element_text(angle = 90)) +
      guides(fill=FALSE)
  })
  
  #install.packages("BiocManager")
  #BiocManager::install("Rgraphviz")
  #library(Rgraphviz)
  #word correlation plot
  #output$corrPlot <- renderPlot({
    #plot((tdm),
         #terms=findFreqTerms(tdm, lowfreq=input$freq),
         #corThreshold=0.15,
         #weighting = T)
  #})
  
  
  
}

shinyApp(ui, server)

