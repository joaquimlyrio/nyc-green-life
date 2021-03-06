## ui.R

library(shiny)
library(shinydashboard)
library(data.table)
library(ggmap)
library(leaflet)
library(plotly)
library('RJSONIO')
library('geosphere')
library('purrr')

dashboardPage(
  
  ## dashboard header
  dashboardHeader( title = "Green Life in NYC" ),
  
  ## dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      
      ################################################################
      ## Introduction tab side
      ################################################################
      menuItem("Introduction", tabName = "intro"),
      

      
      ################################################################
      ## Maps tab side
      ################################################################
      # create Maps Tab with subitems
      menuItem("Map", tabName = "map",
               
               menuItem('Air Quality',
                            tabName = 'tAirMap'),   
               menuItem('Community Gardens',
                           tabName = 'tGardenMap' ),
               menuItem('Citibike Stations',
                        tabName = 'tCitibikeMap' ) ),
      
      
      
      ################################################################
      ## Statistics tab side
      ################################################################
      menuItem("Report",  tabName = "stats",
               menuSubItem('General',
                           tabName = 'general' ),
               menuSubItem('By Neighborhood',
                           tabName = 'location' )),
      
      
      
      ################################################################
      ## Datasets tab side
      ################################################################
      # create Data Tab with subitems
      menuItem("Data", tabName = "data",
               menuSubItem('Air Quality',
                           tabName = 'tAirData' ),
               menuSubItem('Community Gardens',
                           tabName = 'tGardenData' ),
               menuSubItem('Citibike station',
                           tabName = 'tBikeData' )),
      
      
      ################################################################
      ## Contact tab side
      ################################################################
      # create Data Tab with subitems
      menuItem("Contact us", tabName = "contact")
      
      
    )),
  
  ## dashboard body
  dashboardBody(

    tabItems(
      
      ################################################################
      ## Introduction tab body
      ################################################################
      # Introduction tab content
      tabItem(tabName = "intro",
              
              h2("Introduction"),
              
              h4(type="html", " We care about living a green life, and we have a 
                     hunch that we???re not the only ones. Global warming 
                      is real, and driving our cars creates more 
                      greenhouse gases that are contributing to the 
                      problem. Crops are yielding less food, glaciers 
                      are melting causing the ocean levels to rise, and 
                      droughts are plaguing areas that once had plenty 
                      of water. But living a 
                      green life is important for our personal health, too."),
              h3(""),
              h4("The academic 
                      literature on the effect of air pollutants on our 
                      health has grown dramatically in the last decade, 
                      all pointing in the same direction. We now know 
                      that breathing pollutants has a strong relationship
                       with small health problems like 
                      allergies and asthma, and even deadly 
                      cardiovascular diseases. In fact, the Global 
                      Burden of Disease has ranked exposure to 
                      airborne pollution as the seventh most important 
                      factor to mortality worldwide. And the National 
                      Institute of Health has identified a strong 
                      relationship between those who develop lung cancer 
                      and those who are exposed to air pollution on a 
                      daily basis."),
              h3(""),
              h4("Furthermore, these pollutants are 
                      more present in dense urban areas, and if New York 
                      City is anything, it???s a dense urban area. We want to help you live 
                      a life in New York City and enjoy the benefits of 
                      an urban life while avoiding the problems 
                      associated with breathing bad air. Our tool will 
                      help you:"),
              h3(""),
              
              h4("???	Explore the distribution of air pollution in New York so 
                      you can find a place to live with cleaner air \n"
              ),
              
              tags$h4(" ???	Show you the location of bike-share stations so you can 
                      fight the problem of air pollution by avoiding the harmful fossil-
                      fuel combustion of motor vehicles \n"),
              tags$h4("???	Identify community gardens to help find safe produce 
                      and avoid having to choose between potentially dangerous 
                      genetically modified organisms (GMO???s) and expensive organic 
                      alternatives \n"),
              h3(""),
              
              h4("Living green is important, and we want to empower 
                      you to do your part. Enjoy our tool and learn how to live an 
                      environmentally friendly life. It???s more important now than ever 
                      to take action to solve our environmental problems. Because these changes in our planet are serious, 
and without action, they're only going to get worse."),
              HTML('<p><img src="image_nyc.jpg"/></p>')
              
              
              
      ),
      
     
      
      ################################################################
      ## Maps tab body
      ################################################################
      
      # Garden map tab content
      tabItem(tabName = "tGardenMap",
              
              h2("Community Gardens In New York City"),
              
              h4("You can click the locations you want to explore. The number of green gardens in that region will be shown on the map."),
              
              leafletOutput("leafletGardenPlot")
              
      ),
      
      # Air quality map tab content
      tabItem(tabName = "tAirMap",
              
              h2("  Quantile Map of Air Quality in New York City"),
              
              tabsetPanel(
                
                tabPanel("All", 
                         fluidRow(
                           box(checkboxGroupInput("pollutant_level0", "Select pollutant Level", c("Low (Green)" =1, "Below Medium (yellow)" =2, "Above Medium (orange)" = 3, "High (red)" = 4),
                                                  selected = c(1,2,3,4)),width = 4),
                           #box(analysis, height = 160),
                           column(width=10, box(width = NULL, solidHeader = TRUE, leafletOutput("mapAirPlot0", height = 500))
                           ))), 
                
                tabPanel("PM2.5", 
                         fluidRow(
                                  box(checkboxGroupInput("pollutant_level1", "Select pollutant Level", c("Low (Green)" =1, "Below Medium (yellow)" =2, "Above Medium (orange)" = 3, "High (red)" = 4),
                                                         selected = c(1,2,3,4)),width = 4),
                                  #box(analysis, height = 160),
                                  column(width=10, box(width = NULL, solidHeader = TRUE, leafletOutput("mapAirPlot1", height = 500))
                                  ))), 
                tabPanel("NO2", 
                         fluidRow(
                                  box(checkboxGroupInput("pollutant_level2", "Select pollutant Level", c("Low (Green)" =1, "Below Medium (yellow)" =2, "Above Medium (orange)" = 3, "High (red)" = 4),
                                                         selected = c(1,2,3,4)),width = 4),
                                  #box(analysis, height = 160),
                                  column(width=10, box(width = NULL, solidHeader = TRUE, leafletOutput("mapAirPlot2", height = 500))
                                  ))),
                tabPanel("EC", 
                         fluidRow(
                                  box(checkboxGroupInput("pollutant_level3", "Select pollutant Level", c("Low (Green)" =1, "Below Medium (yellow)" =2, "Above Medium (orange)" = 3, "High (red)" = 4),
                                                         selected = c(1,2,3,4)),width = 4),
                                  #box(analysis, height = 160),
                                  column(width=10, box(width = NULL, solidHeader = TRUE, leafletOutput("mapAirPlot3", height = 500))
                                  ))), 
                tabPanel("NO", 
                         fluidRow(
                                  box(checkboxGroupInput("pollutant_level4", "Select pollutant Level", c("Low (Green)" =1, "Below Medium (yellow)" =2, "Above Medium (orange)" = 3, "High (red)" = 4),
                                                         selected = c(1,2,3,4)),width = 4),
                                  #box(analysis, height = 160),
                                  column(width=10, box(width = NULL, solidHeader = TRUE, leafletOutput("mapAirPlot4", height = 500))
                                  ))), 
                tabPanel("O3", 
                         fluidRow(
                                  box(checkboxGroupInput("pollutant_level5", "Select pollutant Level", c("Low (Green)" =1, "Below Medium (yellow)" =2, "Above Medium (orange)" = 3, "High (red)" = 4),
                                                         selected = c(1,2,3,4)),width = 4),
                                  #box(analysis, height = 160),
                                  column(width=10, box(width = NULL, solidHeader = TRUE, leafletOutput("mapAirPlot5", height = 500))
                                  )))
               
                )
       ),
      
      # Citibike Stations map tab content
      tabItem(tabName = "tCitibikeMap",
              
              h2("Citibike Stations In New York City"),
              
              h4("You can click the locations you want to explore. The number of citibike stations in that region will be shown on the map."),
              
              
              leafletOutput("bikeStationsLeaflet")
              
      ),
      
      
      
      ################################################################
      ## Statistics tab body
      ################################################################
      tabItem(tabName = "general",
              
              h2("Statistical Analysis for Each Air Pollutant"),
              
              uiOutput("uiEda1"),
              plotlyOutput("plot1EDA"),
              h2(""),
              textOutput("analysis"),
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              )
            
              
             
              
      ),
      
      tabItem(tabName = "location",
              
              h2("Analysis for Each Neighborhood"),
              
            
              uiOutput("uiEda2"),
              h3("Composition of Pollutants:"),
              tabsetPanel(
                tabPanel("Radar Plot", plotOutput("radarplot")), 
                tabPanel("Pie Plot", plotlyOutput("plot2EDA"))),
              h3("Nearest Bike Stations:"),
              
              fluidRow(
                column(12,
                       dataTableOutput('tableBikestations')
                )
              )
          
              
              
      ),
      
      ################################################################
      ## Datasets tab body
      ################################################################
      
      ## air quality data tab content
      tabItem( tabName = "tAirData",
               
               fluidRow(
                 column(12,
                        dataTableOutput('tableAir')
                 )
               )
      ),
      
      ## community garden data tab content
      tabItem( tabName = "tGardenData",
               
               fluidRow(
                 column(12,
                        dataTableOutput('tableGarden')
                 )
               )
      ),
      
      ## community bike data tab content
      tabItem( tabName = "tBikeData",
               
               fluidRow(
                 column(12,
                        dataTableOutput('tableBike')
                 )
               )
      ),
      
      ################################################################
      ## Contact  tab body
      ################################################################
      # Introduction tab content
      tabItem(tabName = "contact",
              
              h2("Contact us"),
              
              h3( "We are Group 7!"),
              
              h5("Lyrio, Joaquim jc4637@columbia.edu"),
              h5("Gao, Xin xg2249@columbia.edu"),
              h5("Guo, Xinyao xg2257@columbia.edu"),
              h5("Ni, Jiayu jn2585@columbia.edu"),
              h5("Thompson, Wyatt wct2112@columbia.edu")
           
      )
      
    )
))
