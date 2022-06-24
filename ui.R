#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(dashboardPage(

  dashboardHeader(title = "Aviation Crashes"),

           dashboardSidebar(
             
             # functions for generating the menu and options
             
             sidebarMenu(
               menuItem("Overview", tabName = "Main", icon = icon("signal", lib = "glyphicon")),
               menuItem("Crash Analytics", tabName = "stats", icon = icon("warning-sign", lib = "glyphicon")),
               menuItem("Operator Analytics", tabName = "operator", icon = icon("briefcase", lib = "glyphicon")),
               menuItem("Plane Analytics", tabName = "plane", icon = icon("plane", lib = "glyphicon")),
               menuItem("Database", tabName = "Searcher", icon = icon("zoom-in", lib = "glyphicon")),
               menuItem("Author", tabName = "About", icon = icon("user", lib = "glyphicon"))
               )
             ),
             
             dashboardBody(
               
               tabItems(  
                 # first page
                 tabItem(
                   tabName = "Main",
                   fluidRow(
                       # headline statistics
                       valueBoxOutput("overall", width = 3),
                       valueBoxOutput("survivalrate", width = 3),
                       valueBoxOutput("deadliestcrash", width = 3),
                       valueBoxOutput("frequentreason", width = 3)
                   ),
                   fluidRow(
                     box(
                       title = "Overview",
                       column(
                         # description
                         p("This section shows an overview of all the plane crashes that happened worldwide. Use the filters on the right to naviate to different statistics"),
                         width = 4
                        ),
                       column(
                         # for creating a filter by flight type
                         pickerInput(
                           inputId = "crashflight",
                           label = "Filter by Flight Type",
                           choices = c("All", sort(unique(crashes$`Flight type`))),
                           selected = "All"
                         ),
                         width = 3
                       ),
                       column(
                         # for creating a fliter by year
                          sliderTextInput(
                            inputId = "years",
                            label= "Filter by Year",
                            choices = sort(unique(year(crashes$Date))),
                            selected = c(min(unique(year(crashes$Date))), max(unique(year(crashes$Date)))),
                            grid=TRUE,
                            force_edges = TRUE),
                          width = 5
                       ),
                        status = "primary", solidHeader = TRUE,
                        height = 140,
                       width = 12
                     )
                   ),
                   fluidRow(
                     box(
                       title = "A World Map of Plane Crashes",
                       column(
                         # for generating the frequency table of countries in the world map
                         dataTableOutput("map_table"),
                         width = 3
                       ),
                       column(
                         # for printing the map
                         plotOutput("map", height = 440),
                         width = 9
                       ),
                       width = 12,
                       height = 500,
                       status = "primary", solidHeader = TRUE
                     )
                   )
                 ),
                 # second page
                 tabItem(
                   tabName = "stats",
                   fluidRow(
                     box(
                       title = "Crash Analytics",
                       status = "primary", solidHeader = TRUE,
                       column(
                        # description
                        p("This section shows various trends regarding the circumstances of a plane crash. Interact with the filters to learn more."),
                        width = 4
                      ),
                      column(
                        pickerInput(
                          # creation of filter by crash
                          inputId = "crashcause",
                          label = "Filter by Cause of Crash",
                          choices = c("All", sort(unique(crashes$`Crash cause`))),
                          selected = "All"
                        ),
                        width = 3
                      ),
                      column(
                        sliderTextInput(
                          # creation of filter by year
                          inputId = "crashyears",
                          label= "Filter by Year",
                          choices = sort(unique(year(crashes$Date))),
                          selected = c(min(unique(year(crashes$Date))), max(unique(year(crashes$Date)))),
                          grid=TRUE,
                          force_edges = TRUE
                        ),
                        width = 5
                      ),
                      height = 140,
                      width = 12
                     )
                   ),
                   fluidRow(
                         box(
                           # bar trend of crashes
                           title = "Plane Crashes throughout the Years",
                           status = "primary", solidHeader = TRUE,
                           plotOutput("crashtrend"),
                           width = 6,
                           height = 480
                         ),
                     column(
                       fluidRow(
                         box(
                           title = "Outcome of Plane Crashes",
                           status = "primary", solidHeader = TRUE,
                           column(
                             # pie chart of crashes by outcome (survived or not)
                             plotOutput("survival_dist", height = 145),
                             width = 5
                           ),
                           column(
                             # bar chart of souls on board by outcome (passenger/crew and survived/not)
                             plotOutput("fatalities_dist", height = 155),
                             width = 7
                           ),
                           width = 12,
                           height = 210
                         )
                       ),
                       fluidRow(
                         box(title = "Common Plane Crash Locations",
                             status = "primary", solidHeader = TRUE,
                             column(
                               # top countries with most crashes
                               plotOutput("countries_bar", height = 180),
                               width = 6
                             ),
                             column(
                               # top terrains with most crashes
                               plotOutput("terrain_bar", height = 180),
                               width = 6
                             ),
                             width = 12,
                             height = 250
                         )
                       ),
                       width = 6
                     )
                   )
                 ),
                 # third page
                 tabItem(
                   tabName = "operator",
                   fluidRow(
                     box(
                       title = "Operator Analytics",
                       status = "primary", solidHeader = TRUE,
                       column(
                         # description
                         p("This section shows various trends regarding the circumstances of a plane crash by operator. Interact with the filters to learn more."),
                         width = 4
                       ),
                       column(
                         pickerInput(
                           # creation of filter by operator
                           inputId = "crashoperator",
                           label = "Filter by Operator",
                           choices = c("All", sort(unique(crashes$Operator))),
                           selected = "All",
                           options = list(
                             `live-search` = TRUE)
                         ),
                         width = 3
                       ),
                       column(
                         sliderTextInput(
                           # creation of filter by year
                           inputId = "crashyearsoperator",
                           label= "Filter by Year",
                           choices = sort(unique(year(crashes$Date))),
                           selected = c(min(unique(year(crashes$Date))), max(unique(year(crashes$Date)))),
                           grid=TRUE,
                           force_edges = TRUE
                         ),
                         width = 5
                       ),
                       height = 140,
                       width = 12
                     )
                   ),
                   fluidRow(
                     box(
                       # bar trend of crashes
                       title = "Plane Crashes throughout the Years",
                       status = "primary", solidHeader = TRUE,
                       plotOutput("crashoperatortrend"),
                       width = 6,
                       height = 480
                     ),
                     column(
                       fluidRow(
                         box(
                           title = "Plane Crashes per Cluster",
                           status = "primary", solidHeader = TRUE,
                           column(
                             # top aircrafts with the most crashes
                             plotOutput("planes_dist", height = 145),
                             width = 6
                           ),
                           column(
                             # top countries with the most crashes
                             plotOutput("countries_operator_bar", height = 155),
                             width = 6
                           ),
                           width = 12,
                           height = 210
                         )
                       ),
                       fluidRow(
                         box(title = "Common Circumstances of Plane Crashes",
                             status = "primary", solidHeader = TRUE,
                             column(
                               # top terrains with the most crashes
                               plotOutput("terrain_operator_bar", height = 180),
                               width = 6
                             ),
                             column(
                               # top causes for crashes
                               plotOutput("cause_operator_dist", height = 180),
                               width = 6
                             ),
                             width = 12,
                             height = 250
                         )
                       ),
                       width = 6
                     )
                   )
                 ),
                 # fourth page
                 tabItem(
                   tabName = "plane",
                   fluidRow(
                     box(
                       title = "Plane Analytics",
                       status = "primary", solidHeader = TRUE,
                       column(
                         # description
                         p("This section shows various trends regarding the circumstances of a plane crash by aircraft. Interact with the filters to learn more."),
                         width = 4
                       ),
                       column(
                         pickerInput(
                           # creation of filter by aircraft
                           inputId = "crashaircraft",
                           label = "Filter by Aircraft",
                           choices = c("All", sort(unique(crashes$Aircraft))),
                           selected = "All",
                           options = list(
                             `live-search` = TRUE)
                         ),
                         width = 3
                       ),
                       column(
                         sliderTextInput(
                           # creation of filter by year
                           inputId = "crashyearsaircraft",
                           label= "Filter by Year",
                           choices = sort(unique(year(crashes$Date))),
                           selected = c(min(unique(year(crashes$Date))), max(unique(year(crashes$Date)))),
                           grid=TRUE,
                           force_edges = TRUE
                         ),
                         width = 5
                       ),
                       height = 140,
                       width = 12
                     )
                   ),
                   fluidRow(
                     box(
                       # bar trend of crashes
                       title = "Plane Crashes throughout the Years",
                       status = "primary", solidHeader = TRUE,
                       plotOutput("crashaircrafttrend"),
                       width = 6,
                       height = 480
                     ),
                     column(
                       fluidRow(
                         box(
                           title = "Common Circumstances of Plane Crashes",
                           status = "primary", solidHeader = TRUE,
                           column(
                             # top causes of crashes
                             plotOutput("cause_aircraft_dist", height = 145),
                             width = 6
                           ),
                           column(
                             # most frequent phases of planes in crashes
                             plotOutput("phases_dist", height = 155),
                             width = 6
                           ),
                           width = 12,
                           height = 210
                         )
                       ),
                       fluidRow(
                         box(title = "Common Clusters of Plane Crashes",
                             status = "primary", solidHeader = TRUE,
                             column(
                               # top flight types of crashed aircrafts
                               plotOutput("ftype_dist", height = 180),
                               width = 6
                             ),
                             column(
                               # top countries with most crashes
                               plotOutput("countries_aircraft_bar", height = 180),
                               width = 6
                             ),
                             width = 12,
                             height = 250
                         )
                       ),
                       width = 6
                     )
                   )
                   
                 ),
                 # fifth page
               tabItem(
                 tabName = "Searcher",
                 fluidRow(
                   # filters for creating a table of recorded plane crashes
                   box(
                     title = "Plane Crash Searcher",
                     status = "primary", solidHeader = TRUE,
                     dateRangeInput("dates",
                                    label = "Date range",
                                    start = "1918-05-02",
                                    end = "2022-05-12")
                     ,
                     pickerInput(
                       inputId = "id1",
                       label = "Filter by Region",
                       choices = sort(unique(crashes$Region)),
                       options = list(
                         `live-search` = TRUE, `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id2",
                       label = "Filter by Country",
                       choices = sort(unique(crashes$Country)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id3",
                       label = "Filter by Operator",
                       choices = sort(unique(crashes$Operator)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id4",
                       label = "Filter by Aircraft",
                       choices = sort(unique(crashes$Aircraft)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id5",
                       label = "Filter by Registration",
                       choices = sort(unique(crashes$Registration)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id6",
                       label = "Filter by Crash Cause",
                       choices = sort(unique(crashes$`Crash cause`)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id7",
                       label = "Filter by Terrain Type",
                       choices = sort(unique(crashes$`Crash site`)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id8",
                       label = "Filter by Flight Phase",
                       choices = sort(unique(crashes$`Flight phase`)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id9",
                       label = "Filter by Flight Type",
                       choices = sort(unique(crashes$`Flight type`)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "id10",
                       label = "Filter by Survivors?",
                       choices = sort(unique(crashes$Survivors)),
                       options = list(
                         `live-search` = TRUE,  `actions-box` = TRUE),
                       multiple = TRUE
                     ),
                     # button to generate the table
                     actionBttn(
                       inputId = "id11",
                       label = "Generate Table?",
                       style = "material-flat",
                       color = "danger"
                     ),
                     width = 3
                   ),
                   box(
                     # area where table is generated
                     title = "Plane Crash Table",
                     status = "primary", solidHeader = TRUE,
                     width = 9,
                     height = 910,
                     tabBox(
                       # separates tables into different tabs
                       id = "tabset1", height = "250px",
                       tabPanel("Overview", dataTableOutput('table1')),
                       tabPanel("Aircraft Profile", dataTableOutput('table2')),
                       tabPanel("Destination", dataTableOutput('table3')),
                       tabPanel("Crash Site", dataTableOutput('table4')),
                       tabPanel("Outcome", dataTableOutput('table5')),
                       width = 12
                     )
                   )
                 )
               ),
               tabItem(
                 # about page
                 tabName = "About",
                 box(title = "Author of this Dashboard",
                     status = "primary", solidHeader = TRUE,
                     height = 550,
                     column(
                       img(src='corpopic2.jpg', height = "100%", width = "100%", align = "left"),
                       width = 2
                     ),
                     column(
                       h2(strong("Background")),
                       p("Abe Ceasar Perez or Abe, for short, is a graduate from the University of the Philippines - Los Banos
                       He graduated with Latin Honors, under the degree program, Bachelor of Science in Statistics."),
                       p("He has previously worked as an intern under the Philippine Statistics Authority and is currently working as a Strategy Analyst in Aarki, LLC.
                       He is also serving as a part-time statistician for any clients who are in need of finishing their thesis/research papers"),
                       p("Among the skills that he has accumulated over the years include: report-writing, dashboard creation, data visualization, and data analysis.
                       Currently, he is taking up online courses in Dataquest to practice and further improve his programming/data analysis skills."),
                       h2(strong("Why I made this dashboard")),
                       p("I've thought about making a dashboard on airline accidents based on the data from the Bureau of Aircraft Accidents Archive (https://www.baaa-acro.com/crash-archives)."),
                       p("As someone who has riden an airplane numerous times, never did it pondered to me as to whether or not the plane I'm riding is more likely to crash. Although I am grateful and lucky to have been standing today, others before me may have experienced an unfortunate fate."),
                       p("Such events like these, as sensitive as it may seem, must be further explored and understood by everyone in order to become more aware as to what is or has happened in our skies today. It is our duty, both as passengers and decision-makers to determine as to whether or not a certain flight/operator/aircraft is deemed safe for everyone."),
                       p("Decision-makers, in particular, should have access on a dashboard like this so that such accidents, whether intentional or unintentional, must be addressed immediately so that it will not happen again in the future."),
                       width = 10
                    ),
                     width = 12)
               )
               )
             )
           

))
