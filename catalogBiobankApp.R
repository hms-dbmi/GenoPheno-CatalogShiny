library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)


######################################################################################
# Define global #
######################################################################################

fieldsMandatory <- c("name", "email", "affiliation", "biobank_submit", 
                     "country_submit", "samples_submit", "patients_submit", 
                     "genomic_submit", "clinical_submit", "diseaseSpec_submit")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }
   #error { color: red; }"

fieldsAll <- c("name", "email", "affiliation", "biobank_submit", 
               "country_submit", "samples_submit", "patients_submit", 
               "genomic_submit", "clinical_submit", "diseaseSpec_submit")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}


######################################################################################
# Define UI #
######################################################################################

ui <- fluidPage(
  
  shinyjs::useShinyjs(), 
  shinyjs::inlineCSS(appCSS), 
  
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
                                "The biobank catalog contains the country where the data was collected, the number of patients present in the biobank, the number of samples collected, the type of phenotypic data (electronic health records -EHR-, questionnaires, clinical notes), the type of genomic data (SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- ) and the disease focus (general or disease specific). Filtering based on all those parameters are available. "
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
                              tags$h5(HTML("<u>Inclusion Criteria</u>")),
                              p(
                                HTML("<ol>
                                <li>XXXX Number of patients</li>
                                <li>XXXX Number of samples</li>
                                <li>Detailed clinical information (~50-100 phenotypes/clinical variables)</li>
                                <li>SNP arrays and/or whole genome or exome sequencing data available</li>
                                <li>The dataset has to be accessible to the user</li>
                                </ol>")
                              ),
                              br(), p(img(src = 'warning.png', align = "rigth", width="25px", height="20px"),
                                      "Do we need a warning? we usually need a warning! :P ", 
                                      img(src = 'warning.png', align = "rigth", width="25px", height="20px")
                                      , style="color:red;"),
                              br(),
                              width = 12, 
                              fluidRow(
                                tags$button(id="confirm1", 
                                            type="button", 
                                            class="btn action-button btn-large btn-primary", 
                                            HTML('<i class="icon-star"></i>Submit a new entry!'))
                              )
                            ))
                   
                 )
               ))
    ),
    tabPanel( value="catalog",
              p("Biobank Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                wellPanel(
                  fluidRow(
                    column( 3, 
                            uiOutput("countryValue")),
                    column( 3, 
                            uiOutput("radioButtonsDiseaseType")),
                    column( 3,
                            uiOutput("patientValue")),
                    column( 3, 
                            uiOutput("sampleValue")))
                  
                )
                ,
                # Main panel for displaying outputs ----
                DT::dataTableOutput("mytable1")
              )
    ), 
    tabPanel( value="submission",
              p("Submit new entry"),
              
              div(
                id = "form",
                column(4, textInput("name", labelMandatory("Curator Name"), "")),
                column(4, textInput("email", labelMandatory("Curator Email"), "")),
                column(4, textInput("affiliation", labelMandatory("Curator Affilitation"), "")),
                
                column(4, textInput("biobank_submit", labelMandatory("Biobank"), "")),
                column(4, textInput("country_submit", labelMandatory("Country"), "")), 
                column(4, textInput("year_submit", "Year of Creation", "")), 
                
                column(4, textInput("samples_submit", labelMandatory("Number of Samples"), "")),
                column(4, textInput("patients_submit", labelMandatory("Number Of Patients"))), 
                column(4, textInput("age_submit", "Age Range Of the Patients", "")), 
                
                column(4, textInput("genomic_submit", labelMandatory("Genomic data information"))), 
                column(4,  textInput("clinical_submit", labelMandatory("Clinical data information"))), 
                column(4,  textInput("diseaseSpec_submit", labelMandatory("Disease information"))), 
                
                actionButton("submit", "Submit", class = "btn-primary")
                
              ),
              shinyjs::hidden(
                div(
                  id = "thankyou_msg",
                  h3("Thanks, your response was submitted successfully!"),
                  actionLink("submit_another", "Submit another response")
                )
              ), 
              shinyjs::hidden(
                span(id = "submit_msg", "Submitting..."),
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "error_msg"))
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
  
  biobanks <- read.csv("https://raw.githubusercontent.com/aGutierrezSacristan/testingApp/master/BiobankList_test.csv")
  
  
  observeEvent(input$confirm0, {
    
    #biobanks <- read.delim( "BiobankList_test.csv", 
    #                        sep = ",", 
    #                        header = TRUE)
    
    biobanks <- read.csv("https://raw.githubusercontent.com/aGutierrezSacristan/testingApp/master/BiobankList_test.csv")
    
    updateTabsetPanel(session, "main_panel",
                      selected = "catalog")
    
  })
  
  output$countryValue <- renderUI({
    selectInput(inputId = "country", 
                label = "Country:", 
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
  
  observeEvent(input$confirm1, {
    
    
    updateTabsetPanel(session, "main_panel",
                      selected = "submission")
    
  })
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  
  formData <- reactive({
    dataSubmission <- sapply(fieldsAll, function(x) input[[x]])
    dataSubmission <- c(dataSubmission, timestamp = epochTime())
    dataSubmission <- t(dataSubmission)
    dataSubmission
  })
  
  saveData <- function(dataSubmission) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(dataSubmission))
    
    write.csv(x = dataSubmission, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })    
  
}


# Create Shiny app ----
shinyApp(ui, server)