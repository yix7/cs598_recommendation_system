## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "black",
          dashboardHeader(
            title = "Movie Recommender",
            titleWidth = 380),
          
          dashboardSidebar(
            width = 380,
            sidebarMenu(
              # Setting id makes input$tabs give the tabName of currently-selected tab
              id = "tabs",
              menuItem("Select Recommendation System:", startExpanded = TRUE,
                       menuSubItem("Genre-based System", tabName = "subitem1"),
                       menuSubItem("Collaborative System", tabName = "subitem2")
              )
            )

          ),

          dashboardBody(
              includeCSS("css/custom.css"),
              tabItems(
                
                tabItem(
                  tabName = "subitem1",
                  h2("Genre-based Recommendation System"),
                  br(),
                  fluidRow(
                    box(width = 12, title = "Step 1: Select the genres you like", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems", uiOutput('genres'))
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btnGenre", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("genre_results")
                    )
                  )
                ),
                
                tabItem(
                  tabName = "subitem2",
                  h2("Collaborative Filtering Recommendation System"),
                  br(),
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btnCf", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("cf_results")
                    )
                  )
                )
                
              )
          )
    )
) 