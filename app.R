library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(readxl)
library(DT)
library(writexl)

#Load data----------------------------------------------------------------------
foods_df <- read_excel('data.xlsx', sheet = 'food_data')

#UI/Tabs------------------------------------------------------------------------

#'Introduction' tab
intro_tab <- tabPanel('Introduction',
                      fluidRow())

#'Foods' tab
foods_tab <- tabPanel('Foods',
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
                                DTOutput('food_selection_output')
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

                      )

#General
ui <- navbarPage(title = 'DIETCOST',
                 theme = shinytheme("cosmo"),
                 useShinyjs(),
                 tags$head(
                   tags$style(HTML('
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
              colnames = c('Food group', 'Food name', 'ID', 'Variety', 'CF/g(CO2)', 'WF/L', 'EF/gm2', 'Price ($/100)'),
              selection = 'multiple',
              rownames = FALSE
              )
  )
  
  observe({
    if(input$type_food_insert_input == 'Assemble food data from our database' && !is.null(input$food_selection_output_rows_selected)){
      food_values$index <- input$food_selection_output_rows_selected
    }
  })
  
  df1 <- reactive(
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      data()[food_values$index,]
    }
  )
  
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
  
  output$saving_input <- downloadHandler(
    filename = 'food_data.xlsx',
    content = function(file){
      write_xlsx(df1(), file)
    }
  )
  
}

#App creation-------------------------------------------------------------------
shinyApp(ui = ui, server = server)

