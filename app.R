library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(vroom)
library(viridis)
library(plotly)
library(sf)
library(sunburstR)

library(gganimate)

library(rvest)

library(gifski)
library(png)

source("preprocessing.R", local = TRUE)
state_df = state_data()
nation_df = nation_data()
kids_df = kids_data()
state_nation_df = state_nation_data()


ui <- tagList(
  navbarPage(
    "FosterCare and Adoption",
    theme = shinytheme("cerulean"),
    tabPanel(
      "Every Kid needs a Family",
      tags$div(
        tags$img(src = "Child_Fostercare_adoption.png",height="300px"),style = "text-align:center"),
      h3("Project Objective", style = "text-align:left"),
      tags$div(
        tags$p(
          "This is a research project for DSBA-5122 at UNC Charlotte.For the project we wanted to explore data related to Foster Care and Adoption System across US.
      Foster care is intended to provide temporary, safe living arrangements and therapeutic services for children who cannot remain safely at home because of risk for maltreatment or inadequate care.Foster care arrangements include non-relative foster homes, relative foster homes (also known as “kinship care”), group homes, institutions, and pre-adoptive homes."
        ),
        tags$p(
          "Our domain problem is one for a socialworker wanting to provide support and services to the foster kids.But a social worker can only access existing resources in a community. If no resources exist,
      there is nothing that a social worker can really do except provide a continuing, stable presence in the child’s life. This is where we need to do better as a community to help foster kids.
      We need to give them access to resources so that if an adoption isn’t available, they will still have tools, a supportive environment, and people who care about their dreams.
      There is great potential for data and technology to be used to help tackle the complex problems faced by the sector, ensure the well-being of foster youth, and even reduce the number of children who end up in foster care in the first place."
        ),
        tags$p(
          "The report and data visualization presented in the project is the national estimates related to children who experience time in foster care and who are adopted from the foster care system,
      relative to each Federal Fiscal Year."
        ),
        tags$p(
          "This data would allow the social worker to see the distribution of different fostercare indicators across the nation.The first part of the data involved finding information about number of Children in Foster Care in the United States. And the second part involves about the number of adoptions that are finalized each year.This would also allow the researcher to drill down to the state level.
                        Another level of detail that we felt would be an important task for the socialworker is to understand people's sentiment about the fostercare and adoption system in the nation."
        )
      ),
      h3("Source Data", style = "text-align:left"),
      div(
        p("Our source data is available at:"),
        a(href = "https://www.acf.hhs.gov/sites/default/files/cb/national_afcars_trends_2009_through_2018.xlsx", "National DataSet"),
        br(),
        a(href = "https://www.acf.hhs.gov/sites/default/files/cb/afcars_state_data_tables_09thru18.xlsx", "State Level DataSet")
        ,
        style = 'text-align:left'
      ),
      br(),
    ),
    tabPanel("Data in Action",
               sidebarPanel(
                 h3("Input"),
                 conditionalPanel(
                     condition = "input.dataActionTab == 'Plot' | input.dataActionTab == 'Data'",
                     selectInput("state", "State", choices = unique(state_df$State)),
                 ),
                 sliderInput("year", "Year :", min=2009, max=2018, value=2010, 
                             animate = animationOptions(interval=1000,loop=TRUE),sep = ""),
                 conditionalPanel(
                   condition = "input.dataActionTab == 'Map'",
                 selectInput("map_category","Select the Category: ",
                             choices = c("Served" = "Served",
                               "In Care as of Sep 30" = "InCare_Sep30",
                               "Entered" = "Entered",
                               "Exited" = "Exited",
                               "Waiting for Adoption" = "Waiting_Adoption",
                               "Parental Rights Terminated" = "parental_rights_terminated",
                               "Adopted" = "adopted"
                             ))
                 ),
                 # show download only for the data tab
                 conditionalPanel(
                   condition = "input.dataActionTab == 'Data'",
                   downloadButton("downloadData", "Download")
                 ),
                 h4("Description"),
                 p("TODO"),
                 width = 3
               ),
               mainPanel(tabsetPanel(id="dataActionTab",
                 tabPanel("Map",
                          textOutput("map_text"),
                          plotlyOutput("map",width = "100%",height="800")
                          ),
                 tabPanel("Plot",
                          plotOutput("pie_chart")),
                 tabPanel("Data",
                          fluidRow(
                            column(4, tableOutput("nation")),
                            column(4, tableOutput("state"))
                          ))
               ))
             ),
    tabPanel("Foster Kids",
               sidebarPanel(
                 h3("Input"),
                 #conditionalPanel(
                 #condition = "input.dataAction == 'Plot' | input.dataAction == 'Data'",
                 radioButtons("category","Select the Category: ",
                             choices = c("Served" = "Served",
                                         "Adopted" = "adopted"
                             ),
                             selected = "Served",
                             inline = TRUE),
                 sliderInput("year2", "Year :", min=2009, max=2018, value=2010, 
                             animate = animationOptions(interval=1000,loop=TRUE),sep = ""),
                 selectInput("state1", "Select a State or Nation:", choices =  unique(sort(state_nation_df$State))),
                 selectInput("state2", "Select Another State to Compare:", choices =  unique(sort(state_df$State)),selected = 'California'),
                 
                 #),
                 h4("Description"),
                 p("TODO"),
                 width = 3
               ),
               mainPanel(tabsetPanel(id="fosterkidsTab",
                                     tabPanel("Plot",
                                              plotOutput("top_ten_countries",height="250px"),
                                              plotlyOutput("parl_coord_plot")),
                                     tabPanel("Data",DT::dataTableOutput("State1"))
               ))
             ),
    tabPanel("Analysis",
               sidebarPanel(
                 h3("Input"),
                 #conditionalPanel(
                 #condition = "input.dataAction == 'Plot' | input.dataAction == 'Data'",
                 #selectInput("ageGroup", "AgeGroup", choices = unique(state_df$State)),
                 radioButtons("ageGroup", "Choose One:",
                              c("0 to 4" = "0 to 4",
                                "5 to 11" = "5 to 11",
                                "12 to 14" = "12 to 14",
                                "15 to 17" = "15 to 17")),
                 selectInput("state3", "State", choices = unique(state_df$State)),
                 #),
                 sliderInput(
                   "year2",
                   "Year :",
                   min = 2009,
                   max = 2018,
                   value = 2010,
                   animate = animationOptions(interval = 1000, loop =
                                                TRUE),
                   sep = ""
                 ),
                 h4("Description"),
                 p("TODO"),
                 width = 3
               ),
               mainPanel(tabsetPanel(id="analysisTab",
                                     tabPanel("Plot",
                                              plotOutput("timeSeries",height="250px"),
                                              plotOutput("sunburst_chart")),
                                     tabPanel("Data",DT::dataTableOutput("state3"))
               ))
             ),
    tabPanel(
      "References",
      h3("Code Repository at Github", style = "text-align:left"),
      div(
        p(
          "All our code is open source, please feel free run our code and use as is or develop new features:"
        ),
        a(
          href = "https://github.com/soumyadipmitra/VisualAnalytics-FinalProject",
          "https://github.com/soumyadipmitra/VisualAnalytics-FinalProject"
        )
        ,
        style = 'text-align:left'
      ),
      br(),
      h3("Ideas for New Development", style = "text-align:left"),
      div(
        p(
          "To review the features we implemented and see new features we would like to add in a future release visit:"
        ),
        a(
          href = "https://github.com/soumyadipmitra/VisualAnalytics-FinalProject",
          "https://github.com/soumyadipmitra/VisualAnalytics-FinalProject"
        )
        ,
        style = 'text-align:left'
      ),
      br(),
      h3("Final Report", style = "text-align:left"),
      div(
        p("Our final report is available for review at:"),
        a(href = "tbd", "tbd")
        ,
        style = 'text-align:left'
      ),
      br(),
      div(
        h3("Created By:"),
        h5("- SoumyaDip Mitra"),
        h5("- Shruti Agarwal"),
        h5("- Ramya prakash")
        ,
        style = "text-align:center"
      )
    )
    
    
  )
)


server <- function(input, output, session) {
  pie_selectdf<- reactive({
    
      state_df %>%
      filter(State==input$state & year==input$year) %>%
      select(Served,InCare_Sep30,Entered,Exited,Waiting_Adoption,parental_rights_terminated,adopted) %>%
      gather(indicators,count,'Served':'adopted')
  })
  
  #<< pie-chart-Starts
  output$pie_chart <- renderPlot({
    
    df <- pie_selectdf() %>%
      # factor levels need to be the opposite order of the cumulative sum of the count
      mutate(Group = factor(indicators, levels = c("Served","InCare_Sep30","Entered","Exited","Waiting_Adoption","parental_rights_terminated","adopted")),
             cumulative = cumsum(count),
             midpoint = cumulative - count / 2,
             #label = paste0(Group, " ", round(count / sum(count) * 100, 1), "%"))
             label = paste0(round(count / sum(count) * 100, 1), "%"))
    
    
    ggplot(df,aes(x = 1, weight = count, fill = indicators)) +
      geom_bar(width = 1, position = "stack") +
      coord_polar(theta = "y") +
      geom_text(aes(x = 1, y = count, label = label),position = position_stack(vjust = .6))+
      theme_void()  

  })
  #<< pie-chart-Ends
  

  
  kids_selectdf<- reactive({
    kids_df<-kids_df %>%
      filter(LocationType=="State",TimeFrame==input$year2,DataFormat=="Number",AgeGroup==input$ageGroup)
    
  })
  
 
  output$sunburst_chart <- renderPlot({
    
    kids_df<-kids_selectdf()
    kids_df$Data <- type.convert(kids_df$Data)
    sum_total_pop = sum(kids_df$Data)
    
    firstLevel = kids_df %>% summarize(total_pop=sum(kids_df$Data))
    
    sunburst_0 = ggplot(firstLevel)
    sunburst_1 = sunburst_0 + 
      geom_bar(data=firstLevel, aes(x=1, y=total_pop), fill='grey', stat='identity') +
      geom_text(aes(x=1, y=sum_total_pop/2, label=paste('Foster kids: ', comma(total_pop))), color='white')
    
    sunburst_1  + coord_polar('y')
    
    loc_pop = kids_df %>% 
      group_by(Location) %>% 
      summarise(total_pop=sum(Data)) %>% 
      arrange(desc(total_pop))
    
    compute_angle = function(perc){
      angle = -1
      #if(perc < 0.25) # 1st q [90,0]
      #angle = 90 - (perc/0.25) * 90
      #else if(perc < 0.5) # 2nd quarter [0, -90]
      #angle = (perc-0.25) / 0.25 * -90
      #else if(perc < 0.75) # 3rd q [90, 0]
      #angle = 90 - ((perc-0.5) / 0.25 * 90)
      #else if(perc < 1.00) # last q [0, -90]
      #angle = ((perc -0.75)/0.25) * -90
      
      if(perc < 0.5) {# 1st half [90, -90]
        angle = (180 - (perc/0.5) * 180) - 90
      }
      else{ # 2nd half [90, -90]
        angle = (90 - ((perc - 0.5)/0.5) * 180)
      }
      
      
      return(angle)
    }
    
    
    secondLevel = loc_pop %>%
      mutate(running=cumsum(total_pop), pos=running - total_pop/2)
    
    secondLevel = secondLevel %>% group_by(1:n())
    
    secondLevel<-secondLevel[!is.na(secondLevel$running) & !is.na(secondLevel$pos),]
    
    
    secondLevel = secondLevel %>%
      mutate(angle=compute_angle((running - total_pop/2) / sum_total_pop))
    
    secondLevel=secondLevel[1:10,]
    
    sunburst_2 = sunburst_1 + geom_bar(data=secondLevel,
                                       aes(x=2, y=total_pop, fill=total_pop, stroke=3),
                                       color='white', position='stack', stat='identity')
    
    sunburst_3 = sunburst_2 + geom_text(data=secondLevel, aes(label=paste(Location, comma(total_pop)), x=2, y=pos, angle=angle))
    sunburst_3 + scale_y_continuous(labels=comma) + scale_fill_continuous(low='white', high='blue') + coord_polar('y') + theme_minimal()
 

  })
  ## Map Text
  output$map_text <- renderText({
    paste(names(choiceNames)[choiceNames == input$map_category]," in ",input$year)
  })
  
  ## Map Plotly Output
  output$map <- renderPlotly({
    ## Read the shape file
    us_states <- st_read("./shp/states.shp")
    
    ### Filter the data for the year & category and rename the state column
    state_data_year <-
      state_df %>% gather(key="Category",value="Value",-State,-year) %>% 
      filter(year == as.numeric(input$year) & Category == as.character(input$map_category) )%>% rename(STATE_NAME = State)
    
    ### Merge the data with the shape file to get the State Codes
    us_states_mapped <-
      inner_join(us_states, state_data_year, by = "STATE_NAME")
    
    #Set hover text
    us_states_mapped$hover <-
      with(us_states_mapped, paste(STATE_NAME))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(us_states_mapped, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ Value,
        text = ~ hover,
        locations = ~ STATE_ABBR,
        color = ~ Value,
        colors = viridis_pal(option = "D")(3)
      ) %>%
      colorbar(title = paste(names(choiceNames)[choiceNames == input$map_category]), x = 0, y = 0.9) %>%
      layout(
        font = list(
          color = 'black'),
        geo = g) %>%
      layout(plot_bgcolor = 'transparent') %>%
      layout(paper_bgcolor = 'transparent')
  })
  
  
  
  ## Horizontal bars for top 10 countries in Category
  output$top_ten_countries <- renderPlot({
    ### Filter the data for the year & category and rename the state column
    top_10_state_category_year <-
      state_df %>% gather(key="Category",value="Value",-State,-year) %>% 
      filter(year == as.numeric(input$year2) & Category == as.character(input$category)) %>%
      arrange(desc(Value)) %>% head(10)
    
    top_10_state_category_year %>% 
      ggplot() +
      geom_col(aes(x=reorder(State,Value),y=Value,fill=Value)) +
      coord_flip() +
      scale_fill_viridis(option = "C") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x=element_blank(),
            # axis.text.y=element_blank(),
            axis.ticks.x=element_blank(),
            # axis.ticks.y=element_blank(),
            panel.grid.major = element_blank()) +
      labs(x="",y="")
    
  })
  
  
  
  ## Parallel Coordinate Plot
  output$parl_coord_plot <- renderPlotly({
    
    state_data_filtered <- state_nation_df %>% filter(State %in% c(input$state1,input$state2))
    
    state_data_filtered %>%
      plot_ly() %>%
      add_trace(type = 'parcoords',
                line = list(color = ~Served,
                            colorscale = 'Jet',
                            showscale = TRUE
                ),
                dimensions = list(
                  list(range=c(0,3),tickvals=c(0,1,2,3),
                       ticktext=c('',input$state2,input$state1,''),label = 'State', 
                       values = ~ifelse(State==input$state2,1,2)),
                  list(label = 'Year', values = ~year),
                  list(range = c(~min(Served),~max(Served)),
                       label = 'Served', values = ~Served),
                  list(range = c(~min(adopted),~max(adopted)),
                       label = 'Adopted', values = ~adopted)
                )
      )

  })
  

  
  
}

shinyApp(ui, server)
