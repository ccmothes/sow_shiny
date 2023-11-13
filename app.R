#
# Scope of Work (SOW) internal tool to generate client info, budgets, and deliverable timelines
#
# Contributors: Caitlin Mothes (ccmothes) and Juan De La Torre (juandlt-csu)


# set up ---------------------------------------------------
library(shiny)
library(tidyverse)
library(googlesheets4)
library(DT)
library(scales)
library(flextable)

# set flextable defaults
set_flextable_defaults(
  font.size = 12,
  font.family = "calibri",
  font.color = "#000000",
  border.color = "#A6A6A6"
)

# connect to scope of work responses via google sheets

## do not need to authenticate since google sheet is public
gs4_deauth()


## read in sheet as df
sheet_url <- "https://docs.google.com/spreadsheets/d/1miAXjWnqgDg3wbi3Rp3NESF2fs7kTE7mQZNEP6qiOMA/edit#gid=2092154335"

client_data <- gs4_get(sheet_url) %>%
  read_sheet() %>%
  # convert everything to character for printing
  mutate(across(everything(), as.character))


## read in bill rates
rates <- read_csv("billing_rates.csv") %>% 
  # add empty column to enter hours
  mutate(Hours = 0)


# Define UI for application ----------
ui <- fluidPage(

    # Application title
    titlePanel("SOW Development Tool"),

    # Sidebar with a slider input for number of bins 
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("client",
                      "Select client:",
                      choices = pull(client_data, "Name")),
          # Print client name and org
          h4("Client info"),
          strong(textOutput("name")),
          em(textOutput("org")),
          textOutput("account"),
          br(),
          
          # print project description
          h4("Project Description"),
          strong(textOutput("title")),
          textOutput("description"),
          br(),
          
          
          #print tasks
          h4("Tasks/Deliverables"),
          textOutput("tasks"),
          br(),
          
          
          # print start date
          h4("Preferred start date:"),
          textOutput("start"),
          br(),
          
          # print deliverable timeline (if entered)
          h4("Deliverable timeline (if applicable):"),
          textOutput("timeline"),
          br(),
          
          # print project end data
          h4("Project completed by:"),
          textOutput("end"),
          br(),
          
          # print funding
          h4("Approximate funding available:"),
          textOutput("funding"),
          br(),
          
          # need a contract?
          h4("Does this project require a contract?"),
          textOutput("contract"),
          br(),
          
          #print any other comments
          h4("Any additional notes/comments:"),
          textOutput("comments")
          
        ),
        
        mainPanel(
          h2("Budget Calculation"),
          
          h4("Enter # hours for each staff/intern on the project:"),
          em("Double-click on the value in the 'hours' colum to change it"),
          # create editable table to input hours
          DTOutput("rates_table"),
          br(),
          h4(textOutput("total_hours")),
          h4(textOutput("total_budget")),
          hr(),
          
          
          #h2("Deliverables Timeline"),
          downloadButton("generate", "Generate SOW")
        )
        ))

    
)

# Define server logic --------
server <- function(input, output) {
  
  selected_cli <- reactive({
    client_data %>% 
      filter(`Name` == input$client)
  })
  
  # print all client info to help user develop SOW ---------
  output$name <- renderText(selected_cli()$`Name`)
  output$org <- renderText(selected_cli()$`Organization, Department, or Affiliation`)
  output$account <- renderText(selected_cli()$`Will financial support for this project come from a CSU account or external funds?`)
  
  output$title <- renderText(selected_cli()$`Project Name`)
  output$description <- renderText(selected_cli()$`Project Description`)
  
  output$tasks <- renderText(selected_cli()$`Tasks and/or specific deliverables to be performed by the Geospatial Centroid`)
  
  output$start <- renderText(selected_cli()$`Preferred project start date:`)
  
  output$timeline <- renderText(selected_cli()$`Timeline of deliverables (if applicable)`)
  
  output$end <- renderText(selected_cli()$`Closeout: When does this project need to be completed?`)
  
  output$funding <- renderText(selected_cli()$`Amount of funding available for this project (approximate if not known)`)
  
  output$contract <- renderText(selected_cli()$`If you are an off-campus entity, does your organization require a formal contract with CSU?`)
  
  output$comments <- renderText(selected_cli()$`Other comments or information`)
  
  
  # create table -----------------------------
  
  ## filter rates table based on on or of campus
  rates_selected <- reactive({
    if(selected_cli()$`Will financial support for this project come from a CSU account or external funds?` == "On-campus, CSU funding") {

      rates %>%
        select(-off_campus_rate) %>% 
        rename(Rate = on_campus_rate)

    } else {

      rates %>%
        select(-on_campus_rate) %>% 
        rename(Rate = off_campus_rate)
    }
  })
  
  ## create empty reactive values to fill with reactive table
    # Reactive values store data that is reactive
    # Responds to changes
  v <- reactiveValues(data = NULL)
  
  # assigns rates_selected() to v$data
  observe({
    v$data <- rates_selected()
  })
  
  ## render table
  output$rates_table <- renderDT({
    DT::datatable(
      v$data,
      #rates_selected(),
      editable = TRUE,
      options = list(dom = 't',
                     columnDefs = list(
                       list(className = 'dt-center', targets = "_all"),
                       list(targets = 0, visible = FALSE)
                     ))
    )
    
  })
  
  ## create table proxy
  proxy = dataTableProxy("rates_table")
  
  # when table is edited, write that edit to the data frame
  observeEvent(input$rates_table_cell_edit, {
    
    # get value
    info = input$rates_table_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    
    # write values to reactive rates table
    v$data <- editData(v$data, input$rates_table_cell_edit, 'rates_table')
    
    # setting total_budget_text to global variable so that we can access it with params
    total_hours_text <<- paste0("Total Staff Hours: ", comma(sum(v$data[,3])))
    total_budget_text <<- paste0("Total Budget: $", comma(sum(v$data[,2] * v$data[,3])))
    
    output$total_hours <- renderText(total_hours_text)
    output$total_budget <- renderText(total_budget_text)
    
  })
  
                                    
  # render .rmd to produce SOW --------------
  output$generate <- downloadHandler(
    filename = function() {
      paste0(input$client, "_SOW_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # render file in temp directory so .knit files don't go in app directory
      tempSOW <- file.path(tempdir(), "sow_template.Rmd")
      tempImg <- file.path(tempdir(), "CSU-Signature.png")
      tempRef <- file.path(tempdir(), "style_reference.docx")
      file.copy("sow_template.Rmd", tempSOW, overwrite = TRUE)
      file.copy("CSU-Signature.png", tempImg, overwrite = TRUE)
      file.copy("style_reference.docx", tempRef, overwrite = TRUE)
      rmarkdown::render(
        tempSOW,
        output_format = "word_document",
        output_file = file,
        params = list(
          filtered_data = selected_cli(),
          rates_data = v$data,
          total_hours = total_hours_text,
          total_budget = total_budget_text
        ),
        envir = new.env(parent = globalenv()),
        clean = F,
        encoding = "utf-8"
      )
    }
  )
  

}

# Run the application 
shinyApp(ui = ui, server = server)
