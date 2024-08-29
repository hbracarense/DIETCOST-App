library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(readxl)
library(DT)
library(writexl)
library(tools)
library(vroom)

#Load data----------------------------------------------------------------------
foods_df <- read_excel('data.xlsx', sheet = 'food_data')

#UI/Tabs------------------------------------------------------------------------

#'Introduction' tab
intro_tab <- tabPanel('Introduction',
                      fluidRow())

#'Foods' tab
foods_tab <- tabPanel('Foods',
                      useShinyjs(),
                      div(
                        id = 'food_form',
                        conditionalPanel(
                          'input.saving_button == 0 & input.proceed_button == 0 & input.proceed_upload_button == 0',
                          fluidRow(
                            column(
                              width = 3,
                              tags$h3(span(HTML('Data insertion'), style = 'padding-left:15px')),
                              box(
                                height = '120px', width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                radioButtons(
                                  "type_food_insert_input",
                                  label = NULL,
                                  c('Assemble food data from our database', 'Load your own data')
                                ),
                                br(),
                                br()
                              )
                            ),
                            conditionalPanel(
                              condition = "input.type_food_insert_input == 'Assemble food data from our database'",
                              column(
                                width = 2,
                                tags$h3(span(HTML('Varieties'), style = 'padding-left:15px')),
                                box(
                                  height = '120px', width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                  checkboxGroupInput(
                                    "varieties_input",
                                    label = NULL,
                                    c(1:3),
                                    selected = 1
                                  ),
                                  br()
                                )
                              ),
                              column(
                                width = 6,
                                box(
                                  height = '120px', width = 12, solidHeader = TRUE,
                                  br(),
                                  br(),
                                  p('Please select, among the items listed below, all the foods that should be in your dataset.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p('When you are finished, please click the', strong("Save and proceed"), ' or ', strong("Proceed without saving"), ' button below.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  br(),
                                  br(),
                                  br()
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.type_food_insert_input == 'Load your own data'",
                              column(
                                width = 9,
                                tags$h3(span(HTML('Warning!'), style = 'padding-left:15px')),
                                box(
                                  width = 12, solidHeader = FALSE, status = 'warning',
                                  p('Your data must be in an Excel spreadsheet (.xlsx format).', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p('Also, your column names must be',strong('exactly'),'as the ones in the model sheet.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p('Be aware of the variable types: variety, emission and price columns',strong('must'),'be numeric.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p("Don't delete any column. If you don't want to use a given variable, set its value to zero. Food group, food name and food ID are",strong('mandatory'), '.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p('Please download the sheet model if you have any doubts. File size is up to 10MB. After submitting, please click the',strong('Proceed'),' button.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  div(
                                    style = "display: inline-block; position:relative; left:calc(37.5%);",
                                    downloadButton(
                                      "food_data_model",
                                      label = "Download food data model",
                                      style = "color: #fff; background-color: #222222; border-color: #fff;"
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.type_food_insert_input == 'Load your own data'",
                            fluidRow(
                              column(width = 4),
                              column(width = 4,
                                     tags$h3(span(HTML('Data input'), style = 'padding-left:15px')),
                                     box(
                                       width = 12, solidHeader = FALSE, status = 'warning',
                                       fileInput('food_data_input', NULL, accept = '.xlsx')
                                     )),
                              column(width = 4)
                            ),
                            fluidRow(
                              column(width = 4),
                              column(width = 4,
                                     div(
                                       style = "display: inline-block; position:relative; left:calc(37.5%);",
                                       actionButton(
                                         inputId = "proceed_upload_input",
                                         label = "Proceed",
                                         style = "color: #fff; background-color: #222222; border-color: #fff;"
                                       )
                                     )
                                     ),
                              column(width = 4)
                            )
                          ),
                          conditionalPanel(
                            condition = "input.type_food_insert_input == 'Assemble food data from our database'",
                            fluidRow(
                              column(
                                width = 12,
                                tags$h3(span(HTML('Pre-loaded foods'), style = 'padding-left:15px')),
                                box(
                                  width = 12, solidHeader = FALSE, status = 'warning',
                                  DTOutput('food_selection_output'),style = "overflow-y: scroll;overflow-x: scroll;"
                                )
                              )
                              
                              
                            )
                          ),
                          conditionalPanel(
                            condition = "input.type_food_insert_input == 'Assemble food data from our database'",
                            fluidRow(
                              div(
                                style = "display: inline-block; position:relative; left:calc(39%);",
                                downloadButton(
                                  "saving_input",
                                  label = "Save and proceed",
                                  style = "color: #fff; background-color: #222222; border-color: #fff;"
                                )
                              ),
                              div(
                                style = "display: inline-block; position:relative; left:calc(40.5%);",
                                actionButton(
                                  inputId = "proceed_input",
                                  label = "Proceed without saving",
                                  style = "color: #fff; background-color: #222222; border-color: #fff;"
                                )
                              ),
                            )
                          )
                          
                        ),
                        conditionalPanel(
                          'input.saving_button > 0 | input.proceed_button > 0 | input.proceed_upload_button > 0',
                          fluidRow(
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br()
                          ),
                          fluidRow(
                            column(
                              width = 3
                            ),
                            column(
                              width = 6,
                              tags$h3(span(HTML('Food data loaded!'), style = 'padding-left:15px')),
                              box(
                                width = 12, solidHeader = FALSE, status = 'warning',
                                p('Food data loaded with success!', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                p('If you wish to reset the data, please click on the button below.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                div(
                                  style = "display: inline-block; position:relative; left:calc(37.5%);",
                                  actionButton(
                                    inputId = "reset_food_input",
                                    label = "Reset data",
                                    style = "color: #fff; background-color: #222222; border-color: #fff;"
                                  )
                                )
                              )
                            ),
                            column(
                              width = 3
                            )
                          ),
                          fluidRow(
                            br(),
                            br(),
                            br()
                          )
                        )
                      )

                      )

#General
ui <- navbarPage(title = 'DIETCOST',
                 theme = shinytheme("cosmo"),
                 useShinyjs(),
                 tags$head(
                   tags$style(HTML('
                   
                                         .shiny-notification {
                                   position:fixed;
                                   top: calc(50%);
                                   left: calc(42.5%);
                                   }
                                    .box.box-solid.box-warning{
                                    border-bottom-color:#000000;
                                    border-left-color:#000000;
                                    border-right-color:#000000;
                                    border-top-color:#000000;
                                    }
                                    
                                    .box.box-warning>.box-header {
                                    color:#000000;
                                    background:#00000
                                    }

                                    .box.box-warning{
                                    border-bottom-color:#000000;
                                    border-left-color:#000000;
                                    border-right-color:#000000;
                                    border-top-color:#000000;
                                    }
                             '
                   )),
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 ),
                 header = tagList(useShinydashboard()),
                 intro_tab,
                 foods_tab)

#Server-------------------------------------------------------------------------
server <- function(input, output, session){
  options(shiny.maxRequestSize=10*1024^2)
  food_values <- reactiveValues()
  food_values$index <- NULL

  data <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      foods_df[foods_df$variety %in% input$varieties_input,]
    }
  })
  
  #Varieties selection check
  observe({
    if(input$type_food_insert_input == 'Assemble food data from our database' && length(input$varieties_input) < 1)
    {
      updateCheckboxGroupInput(session, "varieties_input", selected = 1)
    }
  })
  
  #Foods selection
  output$food_selection_output <- DT::renderDataTable(
    datatable(
              data(),
              colnames = c('Food group', 'Food name', 'ID', 'Variety', 'CF/g(CO2)', 'WF/L', 'EF/gm2', 'Energy (kJ/g)','Fat (g)','Sat. fat (g)','CHO (g)','Sugars (g)', 'Fibre (g)','Protein (g)','Sodium (mg)','Price ($/100)'),
              selection = 'multiple',
              rownames = FALSE,
              width = '80%'
              )
  )
  
  observe({
    if(input$type_food_insert_input == 'Assemble food data from our database' && !is.null(input$food_selection_output_rows_selected)){
      food_values$index <- input$food_selection_output_rows_selected
    }
  })
  
  df1 <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      data()[food_values$index,]
    } else{
      file <- input$food_data_input
      req(file)
      if(file_ext(file$name) == 'xlsx'){
        temp_df <- read_excel(file$datapath)
        model_df <- read_excel('www/food_data_model.xlsx')
        if(identical(names(temp_df), names(model_df))){
          if(!isTRUE(any(sapply(temp_df[,c('CF_gCO2eq', 'WF_l', 'EF_g_m2', 'price')], is.character)))){
          if(!isTRUE(any(sapply(temp_df[,c('food_name', 'food_id')], is.na)))){
            read_excel(file$datapath)
          }
            
          }
          
        }
      }
      

      }
 

  })
  
  observe({
    useShinyjs()
    if(length(input$food_selection_output_rows_selected)==0){
      disable('saving_input')
      disable('proceed_input')
    } else{
      enable('saving_input')
      enable('proceed_input')
    }
  })
  
  observe({
    useShinyjs()
    if(is.null(input$food_data_input)){
      disable('proceed_upload_input')
    } else{
      if(file_ext(input$food_data_input$name) == 'xlsx'){
        temp_df <- read_excel(input$food_data_input$datapath)
        model_df <- read_excel('www/food_data_model.xlsx')
        if(identical(names(temp_df),names(model_df))){
          if(!isTRUE(any(sapply(temp_df[,c('CF_gCO2eq', 'WF_l', 'EF_g_m2', 'price')], is.character)))){
            if(!isTRUE(any(sapply(temp_df[,c('food_name', 'food_id')], is.na)))){
              enable('proceed_upload_input')
            } else{
              showModal(modalDialog("Check your file! There are missing values either in food name or ID column."))
              disable('proceed_upload_input')
            }
          } else{
            showModal(modalDialog("Check your file! Columns that should be numeric are strings."))
            disable('proceed_upload_input')
          }
        } else{
          showModal(modalDialog("Invalid column names! Check your file."))
          disable('proceed_upload_input')
        }
      } else{
        showModal(modalDialog("Invalid format! Please submit a .xlsx file."))
        disable('proceed_upload_input')
      }
    }
  })
  
  
  output$saving_input <- downloadHandler(
    filename = 'food_data.xlsx',
    content = function(file){
      write_xlsx(df1(), file)
    }
  )
  
  observe({
    if(is.null(input$saving_input)){
      runjs("
            var click = 0;
            Shiny.onInputChange('saving_button', click)
            var saving_input = document.getElementById('saving_input')
            saving_input.onclick = function() {click += 1; Shiny.onInputChange('saving_button', click)};
            ")      
    }
  })
  
  observe({
    if(input$proceed_input == 0){
      runjs("
            var click = 0;
            Shiny.onInputChange('proceed_button', click)
            var proceed_input = document.getElementById('proceed_input')
            proceed_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_button', click)};
            ")      
    }
  })
  
  observe({
    if(input$proceed_upload_input == 0){
      runjs("
            var click = 0;
            Shiny.onInputChange('proceed_upload_button', click)
            var proceed_upload_input = document.getElementById('proceed_upload_input')
            proceed_upload_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_upload_button', click)};
            ")      
    }
  })
  
  
  observeEvent(
    input$reset_food_input,
    {
      runjs("
            var click = 0;
            Shiny.onInputChange('saving_button', click)
            var saving_input = document.getElementById('saving_input')
            saving_input.onclick = function() {click += 1; Shiny.onInputChange('saving_button', click)};
            
            var click = 0;
            Shiny.onInputChange('proceed_button', click)
            var proceed_input = document.getElementById('proceed_input')
            proceed_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_button', click)};
            
            var click = 0;
            Shiny.onInputChange('proceed_upload_button', click)
            var proceed_upload_input = document.getElementById('proceed_upload_input')
            proceed_upload_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_upload_button', click)};
            ")  
    }

  )
  
  output$food_data_model <- downloadHandler(
    filename = 'food_data_model.xlsx',
    content = function(file){
      file.copy('www/food_data_model.xlsx',file)
    }
  )

}

#App creation-------------------------------------------------------------------
shinyApp(ui = ui, server = server)

