library(shiny)
library(tidytext)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(shinythemes)
library(rsconnect)


ui <- fluidPage(
  # Application title
  titlePanel("FosterCare USer's Tweets"), 
  
  # task6: add in shinythemes function
  theme = shinytheme("united"),
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    # SidebarPanel with a slider input
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
      fileInput(inputId = "file_1",
                label = "Select file:"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("maxwords","Max # of Words:",min = 10, max = 200, value = 100, step = 10),
      sliderInput("largestwords","Size of largest words:",min = 1, max = 8, value = 4),
      sliderInput("smallestwords","Size of smallest words:",min = 0.1, max = 4, value = 0.5),
    ),
    
    # task1: within the mainPanel, create two tabs (Word Cloud and Word Counts)
    mainPanel(
      tabsetPanel(
        # task3: add in the outputs in the sidebarPanel
        # task6: and modify your figure heights
        tabPanel("Word Cloud",plotOutput("cloud", height = "600px"))
      )
    )
  )
  
)

server <- function(input, output) {
  
  input_tweet_file <- reactive({
    if(is.null(input$file_1)){
      return("!! No data loaded !!")
    }
    readLines(input$file_1$datapath)
  })

  
  output$cloud<-renderPlot({
    
    text<-tibble(text=input_tweet_file())
    #text <-  tibble(text = readLines(sprintf("./data/%s.txt", Inpfile), encoding="UTF-8"))
    
    # could also pass column of text/character instead
    text <- text %>%
      unnest_tokens(word, text) %>%
      count(word, sort = TRUE) 
    
    
    text <- text %>%
      anti_join(stop_words)
    
    
    v <- text
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largestwords,input$smallestwords),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })
  
  
}

shinyApp(ui = ui, server = server)
