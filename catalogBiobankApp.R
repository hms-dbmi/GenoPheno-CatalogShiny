library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)


ui <- fluidPage(
  navbarPage(
    title = "", id="main_panel",
    theme = shinythemes::shinytheme("cosmo"),
    tabPanel(value="main",
             title = p("Cataloging large scale clinical and genomic human biobanks", 
                       style = "font-size: 20px; padding-bottom: -0.5cm"),
             fluidRow( 
               column ( 6,
                        sidebarLayout(
                          sidebarPanel(
                            h3( "Welcome to the Biobank Catalog Shiny App!" ),
                            br(),
                            tags$p(HTML( "The objective of this Shiny App is to provide a dynamic online biobank catalog. We welcome the community to correct and complete it." ) ),
                            tags$p(HTML(
                              "Shiny app GitHub repo: <a href=\"\">https://github.com/hms-dbmi/biobankCatalogShiny</a>."
                            )),
                            br(),
                            h5( HTML("What can you find in here?\n\n" ) ),
                            
                            tags$p(
                              HTML(
                                "<ol start='1'> <li>Current catalog of human biobanks.</li>
                                <li>\"Submit a new entry.</li>
                                </ol>"
                              )
                            ),
                            br(),
                            tags$p(HTML(
                              "For further details see the <a href=\"\">awesome paper</a>."
                            )),
                            
                            width = 12
                          ),
                          #mainPanel(img(src = 'testing.png', align = "center", width="1000px", height="500px"))
                          mainPanel(img(src = 'logo.png', align = "center", height="30px"))
                          
                        )
               ), column(
                 6,
                 tabsetPanel(
                   id = "tabsetLoadOptions",
                   tabPanel("Biobank Catalog",
                            mainPanel(
                              p(
                                "blablabla"
                              ),
                              br(),
                              width = 12
                            ),
                            column(12,
                                   sidebarLayout(
                                     sidebarPanel(
                                       # fluidRow(selectInput("demodatasetSelection",  
                                       #                      label = "Select analysis period", 
                                       #                      choices = c("Year", "Month", "Week"), 
                                       #                      selected = "Month")),
                                       width = 12,
                                       fluidRow(
                                         tags$button(id="confirm0", 
                                                     type="button", 
                                                     class="btn action-button btn-large btn-primary", 
                                                     HTML('<i class="icon-star"></i>Go to the catalog!'))
                                       )
                                     ),
                                     mainPanel( )
                                   ))),
                   tabPanel("Submit a new entry",
                            mainPanel(
                              tags$h3(HTML("<u>Inclusion Criteria</u>")),
                              p(
                                HTML("blablabla")
                              ),
                              br(), p(img(src = 'warning.png', align = "rigth", width="25px", height="20px"),
                                      "Do we need a warning? we usually need a warning! :P ", 
                                      img(src = 'warning.png', align = "rigth", width="25px", height="20px")
                                      , style="color:red;"),
                              br(),
                              width = 12
                            ))
                   
                 )
               ))
    ),
    tabPanel( value="catalog",
              p("Biobank Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                sidebarPanel(
                  fluidRow(
                    uiOutput("countryValue"),
                    uiOutput("patientValue"),
                    uiOutput("sampleValue"),
                    uiOutput("radioButtonsDiseaseType")
                  )
                ),
                # Main panel for displaying outputs ----
                mainPanel(
                  tabsetPanel(
                    id = 'dataset',
                    tabPanel("Biobanks table", DT::dataTableOutput("mytable1"))
                  )
                 
                )
              )
    )
  )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output, session) {
  
  attr(input, "readonly") <- FALSE
  dataValues <- reactiveValues()
  
  
  
  observeEvent(input$confirm0, {
     
    biobanks <- read.delim( "/Users/alba/Desktop/biobankPaper/Biobanks/BiobankList_test.csv", 
                            sep = ",", 
                            header = TRUE)
    
    updateTabsetPanel(session, "main_panel",
                      selected = "catalog")
    
  })
  
  output$countryValue <- renderUI({
    selectInput(inputId = "country", 
                label = "Choose a variable:", 
                choices =  c("All", unique(as.character(biobanks$Country)))
    )
    
  })
  
  output$patientValue <-
    renderUI({
      sliderInput("patientValue", "Number of patients:",
                    min = min(as.numeric(as.character(biobanks$Number_Of_Patients)), na.rm = TRUE),
                    max = max(as.numeric(as.character(biobanks$Number_Of_Patients)), na.rm = TRUE),
                    value = c(min(as.numeric(as.character(biobanks$Number_Of_Patients)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$Number_Of_Patients)), na.rm = TRUE))
        )
      })
  
  output$sampleValue <-
    renderUI({
      sliderInput("sampleValue", "Number of samples:",
                  min = min(as.numeric(as.character(biobanks$Number_Of_Samples)), na.rm = TRUE),
                  max = max(as.numeric(as.character(biobanks$Number_Of_Samples)), na.rm = TRUE),
                  value = c(min(as.numeric(as.character(biobanks$Number_Of_Samples)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$Number_Of_Samples)), na.rm = TRUE))
      )
    })
  
  output$radioButtonsDiseaseType <- renderUI({
    radioButtons("diseasetype", "Disease type:",
                   c("All" = "all",
                     "General" = "general", 
                     "Disease specific" = "specific"
                     )
      )
    
  })
  
  
  
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(biobanks, rownames = FALSE)
  # })
  output$mytable1 <- DT::renderDataTable(DT::datatable({
    data <- biobanks
    if (input$diseasetype != "all") {
      if (input$diseasetype == "general") {
        data <- data[data$Disease_Specific == "General",]
      }
      if (input$diseasetype != "general") {
        data <- data[data$Disease_Specific != "General",]
      }
    }
    if (input$country != "All") {
      data <- data[data$Country == input$country,]
    }
    if( ! is.null(input$patientValue) ){
      data <- data[ as.numeric(as.character(data$Number_Of_Patients)) >= input$patientValue[1] &
                      as.numeric(as.character(data$Number_Of_Patients)) <= input$patientValue[2] , ]
    }
    if( ! is.null(input$sampleValue) ){
      data <- data[ as.numeric(as.character(data$Number_Of_Samples)) >= input$sampleValue[1] &
                      as.numeric(as.character(data$Number_Of_Samples)) <= input$sampleValue[2] , ]
    }
    colnames(data) <- gsub("_", " ", colnames(data))
    data
  }))
  
  
}


# Create Shiny app ----
shinyApp(ui, server)