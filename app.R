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
library(dplyr)
library(magrittr)
library(scales)


source("preprocessing.R", local = TRUE)

state_df = state_data()
nation_df = nation_data()
kids_df = kids_data()
state_nation_df = state_nation_data()


ui <- fluidPage(
  navbarPage(
    "FosterCare and Adoption",
    theme = shinytheme("cosmo"),
    tabPanel(
      "Every Kid Needs a Family",
      tags$div(
        tags$img(src = "Child_Fostercare_adoption.png",height="300px"),style = "text-align:center"),
      h3("Project Objective", style = "text-align:left"),
      tags$div(
        tags$p(
          "This is a research project for DSBA-5122 at UNC Charlotte. For the project we wanted to explore data related to Foster Care and Adoption System across US. 
      Foster care is intended to provide temporary, safe living arrangements and therapeutic services for children who cannot remain safely at home because of risk for maltreatment or inadequate care. Foster care arrangements include non-relative foster homes, relative foster homes (also known as “kinship care”), group homes, institutions, and pre-adoptive homes. "
        ),
        tags$p(
          "Our domain problem is one for a socialworker wanting to provide support and services to the foster kids. But a social worker can only access existing resources in a community. If no resources exist,
      there is nothing that a social worker can really do except provide a continuing, stable presence in the child’s life. This is where we need to do better as a community to help foster kids. 
      We need to give them access to resources so that if an adoption isn’t available, they will still have tools, a supportive environment, and people who care about their dreams. 
      There is great potential for data and technology to be used to help tackle the complex problems faced by the sector, ensure the well-being of foster youth, and even reduce the number of children who end up in foster care in the first place. "
        ),
        tags$p(
          "The report and data visualization presented in the project is the national estimates related to children who experience time in foster care and who are adopted from the foster care system,
      relative to each Federal Fiscal Year."
        ),
        tags$p(
          "This data would allow the social worker to see the distribution of different fostercare indicators across the nation. The first part of the data involved finding information about number of Children in Foster Care in the United States. And the second part involves about the number of adoptions that are finalized each year. This would also allow the researcher to drill down to the state level. 
                        Another level of detail that we felt would be an important task for the socialworker is to understand people's sentiment about the fostercare and adoption system in the nation."
        )
      ),
      h3("Source Data", style = "text-align:left"),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      div(
        p("Our source data is available at:"),
        a(href = "https://www.acf.hhs.gov/sites/default/files/cb/national_afcars_trends_2009_through_2018.xlsx", "National DataSet"),
        br(),
        a(href = "https://www.acf.hhs.gov/sites/default/files/cb/afcars_state_data_tables_09thru18.xlsx", "State Level DataSet"),
        br(),
        a(href = "https://datacenter.kidscount.org/data/tables/101-child-population-by-age-group", "FosterKids Data by Age Group"),
        br(),
        a(href = "https://twitter.com", "Twitter"),
        style = 'text-align:left'
      ),
      br(),
    ),
    ## 'Data in Action' tab
    tabPanel("Data in Action",
               sidebarPanel(
                 h3("Input"),
                 conditionalPanel(
                     condition = "input.dataActionTab == 'Plot'",
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
                 conditionalPanel(
                   condition = "input.dataActionTab == 'Data'",
                 selectInput("table","Choose One: ",
                             choices = c("National" = "National",
                                         "State" = "State")
                             )),
                 h4("Description"),
                 p("This section focuses on looking into FosterCare and Adoption data to see distribution of various fostercare categories across the country. 
                 The user has the ability to select by different categories from the dataset to see how the fostercare distribution changes.  This
            allows the user to examine distribution across each state for different fostercare categories used and these could aid in their research towards understanding each state wise
            Fostercare statistics."),
                 br(),
                 width = 3
               ),
               mainPanel(tabsetPanel(id="dataActionTab",
                 tabPanel("Map",
                          h3(textOutput("map_text"), style = "text-align:center;font-weight:bold"),
                          # textOutput("map_text"),
                          plotlyOutput("map",width = "100%",height="800")
                          ),
                 tabPanel("Plot",
                          h3(textOutput("pie_text"), style = "text-align:center;font-weight:bold"),
                          plotlyOutput("pie_chart"),width="100%",height="800"),
                 tabPanel("Data",
                          DT::dataTableOutput("nation"),
                          DT::dataTableOutput("state"))
               ))
             ),
    tabPanel("Foster Kids",
               sidebarPanel(
                 h3("Input"),
                 
                 radioButtons("category","Select the Category: ",
                             choices = c("Served" = "Served",
                                         "Adopted" = "adopted"
                             ),
                             selected = "Served",
                             inline = TRUE),
                 sliderInput("year2", "Year :", min=2009, max=2018, value=2010, 
                             animate = animationOptions(interval=1000,loop=TRUE),sep = ""),
                 conditionalPanel(
                 condition = "input.fosterkidsTab == 'Plot'",
                 selectInput("state1", "Select a State or Nation:", choices =  unique(sort(state_nation_df$State))),
                 selectInput("state2", "Select Another State to Compare:", choices =  unique(sort(state_df$State)),selected = 'California')
                 ),
                 # show download only for the data tab
                 conditionalPanel(
                   condition = "input.fosterkidsTab == 'Data'",
                   downloadButton("downloadTop10", "Download")
                 ),
                 h4("Description"),
                 p("This section focuses on comparing the most important categories:Served and Adoption across the country. 
                 The first part of the data involved finding information about number of Children in Foster Care in the United States. And the second part involves about the number of adoptions that are finalized each year.
                   This would also allow the researcher to drill down to the state level and helps in understanding current trends between these two categories. "),
                 br(),
                 p("Here it allows a user to select states and compare important categories: Served and Adopted over time.
        The idea is that user can use this section to help guide there focus to certain states and then use the analysis tab to see
        the relationships between the different dataset"),
                 br(),
                 p("The user has the ability to select by these two categories from the dataset to see how the fostercare distribution changes.  We included the
                 national average as a reference to see the level of impact of the selected state over time.This
                 allows the user to examine distribution between two States or State vs National Average."),
                 br(),
                 width = 4
               ),
               mainPanel(tabsetPanel(id="fosterkidsTab",
                                     tabPanel("Plot",
                                              h3(textOutput("top10states_text"), style = "text-align:center;font-weight:bold"),
                                              plotOutput("top_ten_states",height="250px"),
                                              plotlyOutput("parl_coord_plot")),
                                     tabPanel("Data",DT::dataTableOutput("top_ten_countries_data"))
               ))
             ),
    tabPanel("Analysis",
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.analysisTab == 'Kids Distribution'| input.analysisTab == 'Data'",
                 h3("Input")),
                 conditionalPanel(
                   condition = "input.analysisTab == 'Kids Distribution'",
                 sliderInput(
                   "year3",
                   "Year :",
                   min = 2009,
                   max = 2018,
                   value = 2010,
                   animate = animationOptions(interval = 1000, loop =
                                                TRUE),
                   sep = ""
                 )),
                 conditionalPanel(
                   condition = "input.analysisTab == 'Kids Distribution'| input.analysisTab == 'Data'",
                 radioButtons("ageGroup", "Choose One:",
                              c("0 to 4" = "0 to 4",
                                "5 to 11" = "5 to 11",
                                "12 to 14" = "12 to 14",
                                "15 to 17" = "15 to 17"))),
                 # show download only for the data tab
                 conditionalPanel(
                   condition = "input.analysisTab == 'Data'",
                   downloadButton("downloadKidsData", "Download")
                 ),
                 h4("Description"),
                 p("The analysis sections focuses on what the findings are based on the data from the previous sections and help the users draw conclusions from the data."),
                 br(),
                 conditionalPanel(
                   condition = "input.analysisTab == 'Time Series View'",
                 p("Numbers of Children in Foster Care: Since FY 2012, the numbers of children in care on the last day of each fiscal year through FY 2017 have increased. 
                   FY 2017’s 441,000 children represent an 11 percent increase over FY 2012’s 396,000. The numbers of children in care on the last day of FY 2018 as compared to FY 2017 is 
                   virtually unchanged representing a decrease of slightly less than 1 percent."),
                 br(),
                 p("Children Adopted: The number of adoptions that are finalized each year has remained relatively flat during the FYs 2011, 2012, 2013 and 2014. 
                   FY 2015’s 53,600 adoptions represented a nearly 6 percent increase over FY 2014’s 50,700. Each year since FY 2015 adoptions have increased each 
                   year to a historic high of 63,100 in FY 2018."),
                 br()),
                 conditionalPanel(
                   condition = "input.analysisTab == 'Kids Distribution'| input.analysisTab == 'Data'",
        p("This is a sunburst chart visualization that takes FosterCare kids distribution by AgeGroup across the United States and shows how they compare to one another.
        This not only allows the researcher to find patterns about which age groups are high and where to focus research,
        but also how the different fostercare categroies are changing over time.
         ")),
                 width = 4
               ),
               mainPanel(tabsetPanel(id="analysisTab",
                                     tabPanel("Time Series View",
                                              # plotOutput("timeSeries",height="250px")
                                              tags$img(src = "timeseries_plot.gif",width = "100%",height="600px")
                                              ),
                                     tabPanel("Kids Distribution",
                                              plotOutput("sunburst_chart")),
                                     tabPanel("Data",DT::dataTableOutput("kids_by_Age_group_data"))
               ))
             ),
    tabPanel(
      "References",
      h4("Code Repository at Github", style = "text-align:left"),
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
      h4("Ideas for New Development", style = "text-align:left"),
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
      h4("Final Report", style = "text-align:left"),
      div(
        p("Our final report is available for review at:"),
        a(href = "tbd", "tbd")
        ,
        style = 'text-align:left'
      ),
      br(),
      div(
        h4("Created By:"),
        h5("- SoumyaDip Mitra"),
        h5("- Shruti Agarwal"),
        h5("- Ramya Prakash")
        ,
        style = "text-align:center"
      ),
      tags$div(
        tags$img(src = "UNCC_Logo.PNG",height="100px"),style = "text-align:center")
    )
    
    
  )
)


server <- function(input, output, session) {
  
  ## Pie Text
  output$pie_text <- renderText({
    paste("Breakdown for ",input$state," in ",input$year)
  })
    
    
  pie_selectdf<- reactive({
    
      state_df %>%
      filter(State==input$state & year==input$year) %>%
      select(Served,InCare_Sep30,Entered,Exited,Waiting_Adoption,parental_rights_terminated,adopted) %>%
      gather(indicators,count,'Served':'adopted')
  })
  
  #<< pie-chart-Starts
  output$pie_chart <- renderPlotly({
    
    df <- pie_selectdf() %>%
      # factor levels need to be the opposite order of the cumulative sum of the count
      mutate(Group = factor(indicators, levels = c("Served","InCare_Sep30","Entered","Exited","Waiting_Adoption","parental_rights_terminated","adopted")),
             cumulative = cumsum(count),
             midpoint = cumulative - count / 2,
             #label = paste0(Group, " ", round(count / sum(count) * 100, 1), "%"))
             label = paste0(round(count / sum(count) * 100, 1), "%"))
    
    
    # ggplot(df,aes(x = 1, weight = count, fill = indicators)) +
    #   geom_bar(width = 1, position = "stack") +
    #   coord_polar(theta = "y") +
    #   scale_fill_viridis(discrete = TRUE,name="Quartiles") +
    #   geom_text(aes(x = 1, y = count, label = label),position = position_stack(vjust = .6),check_overlap = TRUE,color='white')+
    #   theme_void()  
    
    plot_ly(df, labels = ~Group, values = ~count, type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  })
  #<< pie-chart-Ends
  
  output$nation <- DT::renderDataTable(
    if(input$table=="National"){
      DT::datatable(
        filter(nation_df,FY==input$year))
    })
  output$state <- DT::renderDataTable(
    if(input$table=="State"){
      DT::datatable( filter(state_df,year==input$year))
    })
  
  # download button for kids data grid
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("FosterCare_Adoption_Distribution_",input$table,"_FY",input$year,".csv", sep = "")
    },
    content = function(file) {
      if(input$table=="National"){
      write.csv(filter(nation_df,FY==input$year), file, row.names = FALSE)
      }
      if(input$table=="State"){
        write.csv(filter(state_df,year==input$year), file, row.names = FALSE)
      }
    }
  )
  
  kids_selectdf<- reactive({
    kids_df<-kids_df %>%
      filter(LocationType=="State",TimeFrame==input$year3,DataFormat=="Number",AgeGroup==input$ageGroup)
    
  })
  
  output$kids_by_Age_group_data <- DT::renderDataTable(DT::datatable(kids_selectdf()))
  

# download button for kids data grid
output$downloadKidsData <- downloadHandler(
  filename = function() {
    paste("Kids_distribution_",input$ageGroup,"_FY",input$year3,".csv", sep = "")
  },
  content = function(file) {
    write.csv(kids_selectdf(), file, row.names = FALSE)
  }
)


  output$sunburst_chart <- renderPlot({
    
    kids_df<-kids_selectdf()
    kids_df$Data <- type.convert(kids_df$Data)
    sum_total_pop = sum(kids_df$Data)
    
    firstLevel = kids_df %>% summarize(total_pop=sum(kids_df$Data))
    
    sunburst_0 = ggplot(firstLevel)
    sunburst_1 = sunburst_0 + 
      geom_bar(data=firstLevel, aes(x=1, y=total_pop), fill='grey', stat='identity') +
      geom_text(aes(x=1, y=sum_total_pop/2, label=paste('Foster kids: ', comma(total_pop))), color='blue',size=4,check_overlap = TRUE,fontface='bold')
    
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
    
    sunburst_3 = sunburst_2 + geom_text(data=secondLevel, aes(label=paste(Location, comma(total_pop)), x=2, y=pos, angle=angle),color='black')
    sunburst_3 + scale_y_continuous(labels=comma) + scale_fill_viridis() + coord_polar('y') + theme_minimal()+
    labs(x="",y="")

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
        geo = g)
      # layout(plot_bgcolor = 'transparent') %>%
      # layout(paper_bgcolor = 'transparent')
  })
  
  ## Top Ten States text
  output$top10states_text <- renderText({
    paste("Top Ten States that ",input$category," in ",input$year2)
  })
  
  top_10_state_category_year<- reactive({
  ### Filter the data for the year & category and rename the state column
  top_10_state_category_year <-
    state_df %>% gather(key="Category",value="Value",-State,-year) %>% 
    filter(year == as.numeric(input$year2) & Category == as.character(input$category)) %>%
    arrange(desc(Value)) %>% head(10)
  })
  
  
  ## Horizontal bar plot for top 10 countries in Category
  output$top_ten_states <- renderPlot({

    
    top_10_state_category_year() %>% 
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
  
  output$top_ten_countries_data <- DT::renderDataTable(DT::datatable(top_10_state_category_year()))
  
  # download button for kids data grid
  output$downloadTop10 <- downloadHandler(
    filename = function() {
      paste("Top_10_States_",input$category,"_FY",input$year2,".csv", sep = "")
    },
    content = function(file) {
      write.csv(top_10_state_category_year(), file, row.names = FALSE)
    }
  )
  
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
  

  
  # Timeseries plot in Analysis tab
  # Not working as its taking too much time to render
  # output$timeSeries <- renderPlot({
  #   
  #   ggplot(nation_df, aes(FY, Counts/1000, group = Population, color = Population)) + 
  #     geom_line() +
  #     geom_segment(aes(xend = 2018, yend = Counts/1000), linetype = 5,colour =  'grey')+  
  #     geom_point(size=2)+
  #     transition_reveal(FY) + 
  #     coord_cartesian(clip = 'off') + 
  #     labs(title = 'Indicator values across years', x='Year', y = 'Indicator Count(K)')+
  #     theme_minimal() + 
  #     theme(legend.position = "bottom",
  #           axis.ticks.x=element_blank(),
  #           axis.ticks.y=element_blank(),
  #           panel.grid.major = element_blank())
  # })
  
  
  output$nation <- DT::renderDataTable(
    if(input$table=="National"){
    DT::datatable(nation_df)
    })
  output$state <- DT::renderDataTable(
    if(input$table=="State"){
    DT::datatable(state_df)
      })

  
}

shinyApp(ui, server)
