library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(vroom)
library(viridis)
library(plotly)
library(sf)

source("preprocessing.R", local = TRUE)
state_df = state_data()
nation_df = nation_data()

ui <- fluidPage(
  navbarPage(
    "FosterCare and Adoption",
    theme = shinytheme("superhero"),
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
             sidebarLayout(
               sidebarPanel(
                 h3("Input"),
                 conditionalPanel(
                     condition = "input.dataActionTab == 'Plot' | input.dataActionTab == 'Data'",
                     selectInput("state", "State", choices = unique(state_df$State)),
                 ),
                 sliderInput("year", "Year :", min=2009, max=2018, value=2010, 
                             animate = animationOptions(interval=1000,loop=TRUE)),
                 # show download only for the data tab
                 conditionalPanel(
                   condition = "input.dataActionTab == 'Data'",
                   downloadButton("downloadData", "Download")
                 ),
                 h4("Description"),
                 p("TODO"),
               ),
               mainPanel(tabsetPanel(id="dataActionTab",
                 tabPanel("Map",
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
             )),
    tabPanel("Foster Kids",
             sidebarLayout(
               sidebarPanel(
                 h3("Input"),
                 #conditionalPanel(
                 #condition = "input.dataAction == 'Plot' | input.dataAction == 'Data'",
                 selectInput("state1", "State", choices =  unique(state_df$State)),
                 #),
                 sliderInput(
                   "year1",
                   "Year :",
                   min = 2009,
                   max = 2018,
                   value = 2010,
                   animate = animationOptions(interval = 4000, loop =
                                                TRUE),
                   sep = ""
                 ),
                 h4("Description"),
                 p("TODO"),
               ),
               mainPanel(tabsetPanel(id="fosterkidsTab",
                                     tabPanel("Plot",
                                              plotOutput("plot1")),
                                     tabPanel("Data",DT::dataTableOutput("State1"))
               ))
             )),
    tabPanel("Analysis",
             sidebarLayout(
               sidebarPanel(
                 h3("Input"),
                 #conditionalPanel(
                 #condition = "input.dataAction == 'Plot' | input.dataAction == 'Data'",
                 selectInput("state2", "State", choices = unique(state_df$State)),
                 #),
                 sliderInput(
                   "year2",
                   "Year :",
                   min = 2009,
                   max = 2018,
                   value = 2010,
                   animate = animationOptions(interval = 4000, loop =
                                                TRUE),
                   sep = ""
                 ),
                 h4("Description"),
                 p("TODO"),
               ),
               mainPanel(tabsetPanel(id="analysisTab",
                                     tabPanel("Plot",
                                              plotOutput("plot2")),
                                     tabPanel("Data",DT::dataTableOutput("state2"))
               ))
             )),
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
      select(Served,InCare_Sep30,entered,exited,waiting_Adoption,parental_rights_terminated,adopted) %>%
      gather(indicators,count,'Served':'adopted')
  })
  
  
  #<< pie-chart-Starts
  output$pie_chart <- renderPlot({
    
    df <- pie_selectdf() %>%
      # factor levels need to be the opposite order of the cumulative sum of the count
      mutate(Group = factor(indicators, levels = c("Served","InCare_Sep30","entered","exited","waiting_Adoption","parental_rights_terminated","adopted")),
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
  
  ## Map Plotly Output
  output$map <- renderPlotly({
    ## Read the shape file
    us_states <- st_read("./shp/states.shp")
    
    ### Filter the data for 2009 and rename the state column
    state_data_year <-
      state_df %>% filter(year == input$year) %>% rename(STATE_NAME = State)
    
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
        z = ~ Served,
        text = ~ hover,
        locations = ~ STATE_ABBR,
        color = ~ Served,
        colors = viridis_pal(option = "D")(3)
      ) %>%
      colorbar(title = "Served") %>%
      layout(
        title = paste('Orphans Served by each state in ',input$year),
        titlefont = list(
          color = '#ffffff'),
        margin = 10,
        font = list(
          color = '#ffffff'),
        geo = g) %>%
      layout(plot_bgcolor = 'transparent') %>%
      layout(paper_bgcolor = 'transparent')
  })
  

  
  
}

shinyApp(ui, server)
