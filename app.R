library(shiny)
library(shinythemes)
library(shinyalert)
library(DT)


######################################################################################
# Define UI #
######################################################################################

ui <- fluidPage(

  tags$head(includeHTML(("google-analytics.html"))),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "my.css")
  ),
  
    tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: lightyellow !important;}')),
  
  navbarPage(
    title = "", id="main_panel",
    theme = shinythemes::shinytheme("cosmo"),
    ##first the table
    tabPanel( value="catalog",
              p("GenoPheno Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                wellPanel(
                  fluidRow(
                    column( 3, 
                            uiOutput("countryValue")),
                    column( 3, 
                            uiOutput("radioButtonsDiseaseType")),
                    column( 3, 
                            uiOutput("radioButtonsGenomicType")),
                    column( 3,
                            uiOutput("patientValue"))
                  
                ))
                ,
                # Main panel for displaying outputs ----
                div(DT::dataTableOutput("mytable1", width = 1700), style = "font-size: 75%")
                
              )
    ), 
    tabPanel( value="submit",
              title = p("Submit a new dataset"),
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    tags$p(HTML("To add a new dataset to the catalog, please send an e-mail to <a href='mailto:alba_gutierrez@hms.harvard.edu?subject=GenoPheno%20Catalog%20-%20new%20dataset'>alba_gutierrez@hms.harvard.edu</a> including the following information:")),
                    tags$h5(HTML("<u>Required</u>")),
                    p(
                      HTML("<ol>
                                <li>Dataset name (long name and acronym if any; e.g., UK Biobank)</li>
                                <li>Country (where does the research take place; e.g., UK)</li>
                                <li>Disease/Focus (e.g., general, cancer)</li>
                                <li>Subject count with both genomic and clinical data (e.g., 1000)</li>
                                <li>Number of phenotypic variables per patient (e.g., 1000)</li>
                                <li>Phenotypic data type (e.g., EHR, questionnaires)</li>
                                <li>Sample size (total number of genomic samples; e.g., 1000)</li>
                                <li>Molecular data type (e.g., WGS, WES)</li>
                                <li>Consent groups present in the dataset (e.g., biomedical research)</li>
                                <li>Accession link to the dataset/webpage (e.g., https://dbgap.ncbi.nlm.nih.gov)</li>
                                </ol>")
                    ),
                    tags$h5(HTML("<u>Optional</u>")),
                    p(
                      HTML("<ol>
                                <li>Study design (e.g., case control study, prospective study)</li>
                                <li>Patients age in years (e.g., 40-59, >18)</li>
                                <li>Ancestry (e.g., european(XX%))</li>
                                <li>PubMed ID of the study infrastructure publication (e.g., 25826379)</li>
                                <li>Link to clinical/genomic study (e.g., https://www.ukbiobank.ac.uk/)</li>
                                <li>Link to genomic study if different than the clinical one</li>
                                <li>Genomic markerset (genotyping microarrays or on a technology basis; e.g., grc37, grc38)</li>
                                <li>Notes (additional information)</li>
                                </ol>")
                    ),
                    br(),
                    tags$p(HTML("Please check the inclusion criteria in the <b>About</b> tab before sending a new dataset.")),
                    width = 12
                  ),
                  mainPanel(img(src = 'logo.png', align = "center", height="30px"))
                ))
    ),
    tabPanel(value="main",
             title = p("Suggest a modification on the current catalog data"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   tags$p(HTML("The Shiny App of this on-line catalog is automatically generated based on  the CSV file <a href='https://github.com/hms-dbmi/GenoPheno-CatalogShiny/blob/master/csv/tableData.csv', target='_blank'>\"tableData.csv\"</a> available at the GitHub repo: <a href='https://github.com/hms-dbmi/GenoPheno-CatalogShiny', target='_blank'>GenoPheno-CatalogShiny</a>" )),
                   tags$p(HTML( "To propose any correction, please:")),
                   p(
                     HTML("<ol>
                                <li>Fork the GitHub repo <a href='https://github.com/hms-dbmi/GenoPheno-CatalogShiny', target='_blank'>GenoPheno-CatalogShiny</a></li>
                                <li>Propose the changes in the CSV file <a href='https://github.com/hms-dbmi/GenoPheno-CatalogShiny/blob/master/csv/tableData.csv', target='_blank'>\"tableData.csv\"</a></li>
                                <li>Submit a pull request</li>
                                </ol>")
                    ),
                   br(),
                   h3( "Thank you for your contribution to update and improve the GenoPheno Catalog!" ),
                   br(),
                   tags$p(HTML( "Your proposed changes will be reivewed in the following days")),
                   br(),
                   width = 12
                   ),
                 mainPanel(img(src = 'logo.png', align = "center", height="30px")), 
                 
               ))
    ), 
    tabPanel(value="main",
             title = p("About"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   h3( "Welcome to the GenoPheno Catalog Shiny App!" ),
                   br(),
                   tags$p(HTML( "The objective of this Shiny App is to provide a dynamic online dataset catalog. We welcome the community to correct and complete it." ) ),
                   tags$h5(HTML("<u>Inclusion Criteria</u>")),
                   p(
                     HTML("<ol>
                                <li>Over five hundred (500) human subjects</li>
                                <li>Contain both genotype and phenotype data of the same subjects</li>
                                <li>Include Whole Genome Sequencing (WGS) or Whole Exome Sequencing (WES) data as part of their genomic data content</li>
                                <li>Include at least one hundred (100) recorded phenotypic variables per subject</li>
                                <li>The dataset has to be accessible through a website or via a way of collaboration with investigators</li>
                                <li>All the resources (data and website) have to be available in English</li>
                                </ol>")
                     
                     
                     
                     
                     
                   ),
                   br(),
                   tags$h5(HTML("<u>All five criteria must be meet</u>")),
                   
                   tags$p(HTML( "The GenoPheno catalog contains:
                                        <li>Dataset name (long name and acronym if any)</li>
                                        <li>Country (where does the research take place)</li>
                                        <li>Subject count with both genomic and clinical data</li>
                                        <li>Study design (e.g., cohort, prospective, longitudinal)</li>
                                        <li>Number of phenotypic variables per patient</li>
                                        <li>Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)</li>
                                        <li>Sample size (total number of genomic samples [e.g., # of WGS samples + # of WES samples])</li>
                                        <li>Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )</li>
                                        <li>Genomic Markerset (e.g., genotyping microarrays or on a technology basis)</li>
                                        <li>Disease/Focus (e.g., general or disease specific)</li>
                                        <li>Patients age in years</li>
                                        <li>Ancestry</li>
                                        <li>Consent groups present in the dataset (e.g., biomedical, disease-specific)</li>
                                        <li>Accession link to the dataset (link to the website or contact information to obtain data access)</li>
                                        <li>Link to clinical/genomic study</li>
                                        <li>Link to genomic study if different than the clinical one</li>
                                        <li>Pubmed identifier number to key study infrastructure publication</li>" ) ),
                   br(),
                   
                   tags$p(HTML("For further details see the <a href=\"\">manuscript</a>.")),
                   width = 12
                 ),
                 mainPanel(img(src = 'logo.png', align = "center", height="30px")), 
                 
                 # )
               ))
    )
  )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output, session) {
  
  attr(input, "readonly") <- FALSE
  dataValues <- reactiveValues()
  biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/GenoPheno-CatalogShiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

  observeEvent(input$confirm0, {
    
    
    if( input$dataset == ""){
      biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/GenoPheno-CatalogShiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
    }else{
      biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/GenoPheno-CatalogShiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
      
      output$mytable1 <- DT::renderDataTable(DT::datatable({
        data <- biobanks
        if (input$dataset %in% data$Name) {
          data <- data[data$Name == input$dataset,]
          colnames(data) <- gsub("_", " ", colnames(data))
          data
        }else{
          
          # Show a modal when the button is pressed
          shinyalert("Oops!", "There is not any biobank/datase with that name in the catalog", type = "error")
        }
        
        
      }))
      
    }
    
    
  })
  
  output$countryValue <- renderUI({
    selectInput(inputId = "studydesign", 
                label = "Study Design:", 
                choices =  c("All", unique(tolower(biobanks$Study.Design)))
    )
    
  })
  
  output$patientValue <-
    renderUI({
      sliderInput("patientValue", "Subject count with genomic and clinical data:",
                  min = min(biobanks$Subject, na.rm = TRUE),
                  max = max(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE),
                  value = c(min(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE))
      )
    })
  
  
  output$radioButtonsDiseaseType <- renderUI({
    radioButtons("diseasetype", "Disease/Focus:",
                 c("All" = "all",
                   "General" = "general", 
                   "Disease specific" = "specific"
                 )
    )
    
  })
  
  output$radioButtonsGenomicType <- renderUI({
    radioButtons("genomictype", "Molecular Data Type:",
                 c("All" = "all",
                   "WGS" = "wgs", 
                   "WES" = "wes"
                 )
    )
    
  })
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable({
    data <- biobanks
    if (input$diseasetype != "all") {
      if (input$diseasetype == "general") {
        data <- data[data$Disease.Focus == "General" & !is.na(data$Disease.Focus),]
      }
      if (input$diseasetype != "general") {
        data <- data[data$Disease.Focus != "General"  & !is.na(data$Disease.Focus),]
      }
    }
    if (input$genomictype != "all") {
      if (input$genomictype == "wgs") {
        data <- data[ grep( "WGS", data$Molecular.Data.Type), ]
      }
      if (input$genomictype == "wes") {
        data <- data[ grep( "WES", data$Molecular.Data.Type), ]
      }
    }
    if (input$studydesign != "All") {
      data <- data[tolower(data$Study.Design) == tolower(input$studydesign),]
    }
    if( ! is.null(input$patientValue) ){
      data <- data[ as.numeric(as.character(data$Subject.Count)) >= input$patientValue[1] &
                      as.numeric(as.character(data$Subject.Count)) <= input$patientValue[2] , ]
    }

    colnames(data) <- c("Name","Country", "Subject Count with Genomic and Clinical Data","Study Design","Disease/Focus","Number Of Phenotypic Variables Per Patient",
                         "Phenotypic Data Type","Sample Size","Molecular Data Type","Genomic Markerset",
                         "Patients Age (yrs)","Ancestry","Consent","Accession Link to the Dataset","Link to Clinical And Genomic Study", "Link to Genomic Study (if different than the clinical)","PubMed ID","Notes")
    data
    
  },  filter = "top", escape = FALSE, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 30), callback = JS("
var tips = ['Dataset name (long name and acronym if any)', 'Country (where does the research take place)', 'Subject count with both genomic and clinical data',
'Study design (e.g., cohort, prospective, longitudinal)','Disease/Focus (e.g., general or disease specific)','Number Of Phenotypic Variables Per Patient',
'Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)',
'Sample size (total number of genomic samples [e.g., # of WGS samples + # of WES samples])','Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )','Genomic Markerset (e.g., genotyping microarrays or on a technology basis)',
'Patients Age in years','Ancestry','Consent groups present in the dataset (e.g., biomedical, disease-specific)','Accession link to the dataset (link to the website or contact information to obtain data access)', 
'Link to clinical/genomic study','Link to genomic study if different than the clinical one','Pubmed identifier number to the key study infrastructure publication/s','Notes'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}
")
)
  )

  
  
  
  observeEvent(input$confirm1, {
    
    
    updateTabsetPanel(session, "main_panel",
                      selected = "submission")
    
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)