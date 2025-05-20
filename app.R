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
library(dplyr)

#Load data----------------------------------------------------------------------
foods_df <- read_excel('data.xlsx', sheet = 'food_data')
food_constraints_df <- read_excel('data.xlsx', sheet = 'food_constraints')
food_group_constraints_df <- read_excel('data.xlsx', sheet = 'food_group_constraints')
nutrient_targets_df <- read_excel('data.xlsx', sheet = 'nutrient_targets')

#Global variables---------------------------------------------------------------
min_grams_food <- round(min(food_constraints_df[,c('man_min', 'woman_min', 'boy_min', 'girl_min')]),0)
max_grams_food <- round(max(food_constraints_df[,c('man_max', 'woman_max', 'boy_max', 'girl_max')]),0)
min_serve_size <- round(min(food_constraints_df$serve_size),0)
max_serve_size <- round(max(food_constraints_df$serve_size),0)
min_grams_food_group <- round(min(food_group_constraints_df[,c('man_min_g', 'woman_min_g', 'boy_min_g', 'girl_min_g')]),0)
max_grams_food_group <- round(max(food_group_constraints_df[,c('man_max_g', 'woman_max_g', 'boy_max_g', 'girl_max_g')]),0)
min_serves <- round(min(food_group_constraints_df[,c('man_min_serve', 'woman_min_serve', 'boy_min_serve', 'girl_min_serve')]),0)
max_serves <- round(max(food_group_constraints_df[,c('man_max_serve', 'woman_max_serve', 'boy_max_serve', 'girl_max_serve')]),0)
min_energy <- round(min(nutrient_targets_df$energy_mj_min),0)
max_energy <- round(max(nutrient_targets_df$energy_mj_max),0)
min_fat <- round(max(nutrient_targets_df$fat_grams_min),0)
max_fat <- round(max(nutrient_targets_df$fat_grams_max),0)
min_sat_fat <- round(max(nutrient_targets_df$sat_fat_grams_min),0)
max_sat_fat <- round(max(nutrient_targets_df$sat_fat_grams_max),0)
min_CHO <- round(max(nutrient_targets_df$CHO_grams_min),0)
max_CHO <- round(max(nutrient_targets_df$CHO_grams_max),0)
min_sugars <- round(max(nutrient_targets_df$sugars_grams_min),0)
max_sugars <- round(max(nutrient_targets_df$sugars_grams_max),0)
min_fibre <- round(max(nutrient_targets_df$fibre_grams_min),0)
max_fibre <- round(max(nutrient_targets_df$fibre_grams_max),0)
min_protein <- round(max(nutrient_targets_df$protein_grams_min),0)
max_protein <- round(max(nutrient_targets_df$protein_grams_max),0)
min_sodium <- round(max(nutrient_targets_df$sodium_mgrams_min),0)
max_sodium <- round(max(nutrient_targets_df$sodium_mgrams_max),0)
min_protein_perc <- round(max(nutrient_targets_df$protein_perc_min),0)
max_protein_perc <- round(max(nutrient_targets_df$protein_perc_max),0)
min_sat_fat_perc <- round(max(nutrient_targets_df$sat_fat_perc_min),0)
max_sat_fat_perc <- round(max(nutrient_targets_df$sat_fat_perc_max),0)
min_fat_perc <- round(max(nutrient_targets_df$fat_perc_min),0)
max_fat_perc <- round(max(nutrient_targets_df$fat_perc_max),0)
min_CHO_perc <- round(max(nutrient_targets_df$CHO_perc_min),0)
max_CHO_perc <- round(max(nutrient_targets_df$CHO_perc_max),0)
min_redmeat <- round(max(nutrient_targets_df$redmeat_grams_min),0)
max_redmeat <- round(max(nutrient_targets_df$redmeat_grams_max),0)
min_sugars_perc <- round(max(nutrient_targets_df$sugars_perc_min),0)
max_sugars_perc <- round(max(nutrient_targets_df$sugars_perc_max),0)
min_alcohol_perc <- 0
max_alcohol_perc <- 100
min_discretionary_perc <- 0
max_discretionary_perc <- 100
min_takeaway_perc <- 0
max_takeaway_perc <- 100
linked_low_1 <- c("69016", "69013", "79065")
linked_high_1 <- c("80066", "80023")
linked_low_2 <- "65021"
linked_high_2 <- c("79006", "79088")
#Functions----------------------------------------------------------------------
nutritentsTableName <- function(col_name){
  switch(col_name,
         'energy_mj_min' = 'Energy min (kJ)',  
         'energy_mj_max' = 'Energy max (kJ)',  
         'fat_grams_min' = 'Fat min (g)',  
         'fat_grams_max' = 'Fat max (g)',  
         'sat_fat_grams_min' = 'Saturated fat min (g)',  
         'sat_fat_grams_max' = 'Saturated fat max (g)',  
         'CHO_grams_min' = 'Carbohydrates min (g)',  
         'CHO_grams_max' = 'Carbohydrates max (g)',  
         'sugars_grams_min' = 'Sugars min (g)',  
         'sugars_grams_max' = 'Sugars max (g)',  
         'fibre_grams_min' = 'Fibre min (g)',  
         'fibre_grams_max' = 'Fibre max (g)',  
         'protein_grams_min' = 'Protein min (g)',  
         'protein_grams_max' = 'Protein max (g)',  
         'sodium_mgrams_min' = 'Sodium min (mg)',  
         'sodium_mgrams_max' = 'Sodium max (mg)',  
         'protein_perc_min' = 'Protein min (%)',  
         'protein_perc_max' = 'Protein max (%)',  
         'sat_fat_perc_min' = 'Saturated fat min (%)',  
         'sat_fat_perc_max' = 'Saturated fat max (%)',  
         'fat_perc_min' = 'Fat min (%)',  
         'fat_perc_max' = 'Fat max (%)',  
         'CHO_perc_min' = 'Carbohydrates min (%)',  
         'CHO_perc_max' = 'Carbohydrates max (%)',  
         'redmeat_grams_min' = 'Red meat min (g)',  
         'redmeat_grams_max' = 'Red meat max (g)',  
         'sugars_perc_min' = 'Sugars min (%)',  
         'sugars_perc_max' = 'Sugars max (%)',  
         'alcohol_perc_min' = 'Alcohol min (%)',  
         'alcohol_perc_max' = 'Alcohol max (%)',  
         'discretionary_perc_min' = 'Discretionary min (%)',  
         'discretionary_perc_max' = 'Discretionary max (%)',  
         'takeaway_perc_min' = 'Takeaway min (%)',  
         'takeaway_perc_max' = 'Takeaway max (%)'
         )
}

transposeNutrientsTable <- function(df){
  df_res <- data.frame(nutrient = character(0),
                       min = double(0),
                       max = double(0))

  for(i in 1:ncol(df)){
    if(i %% 2 == 0) next else{
      switch(colnames(df)[i],
             'energy_mj_min' = {n <- 'energy_mj'
                               min_val <- df$energy_mj_min
                               max_val <- df$energy_mj_max},
             
             'fat_grams_min' = {n <- 'fat_g'
                               min_val <- df$fat_grams_min
                               max_val <- df$fat_grams_max},
             'sat_fat_grams_min' = {n <- 'sat_fat_g'
                                 min_val <- df$sat_fat_grams_min
                                 max_val <- df$sat_fat_grams_max},
             'CHO_grams_min' = {n <- 'CHO_g'
                                   min_val <- df$CHO_grams_min
                                   max_val <- df$CHO_grams_max},
             'sugars_grams_min' = {n <- 'sugars_g'
                               min_val <- df$sugars_grams_min
                               max_val <- df$sugars_grams_max},
             'fibre_grams_min' = {n <- 'fibre_g'
                                   min_val <- df$fibre_grams_min
                                   max_val <- df$fibre_grams_max},
             'protein_grams_min' = {n <- 'protein_g'
                                 min_val <- df$protein_grams_min
                                 max_val <- df$protein_grams_max},
             'sodium_mgrams_min' = {n <- 'sodium_mg'
                                     min_val <- df$sodium_mgrams_min
                                     max_val <- df$sodium_mgrams_max},
             'protein_perc_min' = {n <- 'protein_perc'
                                   min_val <- df$protein_perc_min
                                   max_val <- df$protein_perc_max},
             'sat_fat_perc_min' = {n <- 'sat_fat_perc'
                                   min_val <- df$sat_fat_perc_min
                                   max_val <- df$sat_fat_perc_max},
             'fat_perc_min' = {n <- 'fat_perc'
                                   min_val <- df$fat_perc_min
                                   max_val <- df$fat_perc_max},
             'CHO_perc_min' = {n <- 'CHO_perc'
                               min_val <- df$CHO_perc_min
                               max_val <- df$CHO_perc_max},
             'redmeat_grams_min' = {n <- 'redmeat_g'
                               min_val <- df$redmeat_grams_min
                               max_val <- df$redmeat_grams_max},
             'sugars_perc_min' = {n <- 'sugars_perc'
                                   min_val <- df$sugars_perc_min
                                   max_val <- df$sugars_perc_max},
             'alcohol_perc_min' = {n <- 'alcohol_perc'
                                   min_val <- df$alcohol_perc_min
                                   max_val <- df$alcohol_perc_max},
             'discretionary_perc_min' = {n <- 'discretionary_perc'
                                   min_val <- df$discretionary_perc_min
                                   max_val <- df$discretionary_perc_max},
             'takeaway_perc_min' = {n <- 'takeaway_perc'
                                         min_val <- df$takeaway_perc_min
                                         max_val <- df$takeaway_perc_max}
             )
    }
    df_res[nrow(df_res)+1,] <- c(n, min_val, max_val) 
  }
  return(df_res)
}

tabSelectFoodFunction <- function(food_group_name, df){
  df_foods <- df %>% filter(food_group == food_group_name)
  tabPanel(food_group_name,
           box(
             width = 12, solidHeader = FALSE,
             lapply(1:nrow(df_foods), function(i) {
               
               fixedRow(
                 column(width = 8,
                        sliderInput(inputId = paste0('slider_food_',df_foods$food_id[i]), label = paste0(df_foods$food_name[i]),
                                    min = min_grams_food, max = max_grams_food, value = c(min_grams_food,max_grams_food), step = 50)),
                 column(width = 4,
                        br(),
                        numericInput(inputId = paste0('numeric_food_',df_foods$food_id[i]), label = 'Serve size (g)',
                                     min = min_serve_size, max = max_serve_size, value = min_serve_size)
                 )
                 
               )
             })
             
           )
           
  )
}

tabSelectFoodGroupFunction <- function(food_group){
  fixedRow(
    tags$h4(span(HTML(paste("<b>",food_group,"</b>")), style = 'padding-left:15px')),
    column(width = 6,
           sliderInput(inputId = paste0('slider_food_group_g_',food_group), label = "Intake (g)",
                       min = min_grams_food_group, max = max_grams_food_group, value = c(min_grams_food_group,max_grams_food_group), step = 50)),
    column(width = 6,
           sliderInput(inputId = paste0('slider_food_group_s_',food_group), label = "Serves",
                       min = min_serves, max = max_serves, value = c(min_serves,max_serves), step = 5)
    )
    
  )
}

tabSelectNutrientFunction <- function(nutrient){
  switch(nutrient,
         'Energy' = {n = paste(nutrient, '(kJ)')
                     n_min = min_energy
                     n_max = max_energy},
         'Fat' = {n = paste(nutrient, '(g)')
                   n_min = min_fat
                   n_max = max_fat}, 
         'Saturated fat' = {n = paste(nutrient, '(g)')
                           n_min = min_sat_fat
                           n_max = max_sat_fat}, 
         'Carbohydrates' = {n = paste(nutrient, '(g)')
                             n_min = min_CHO
                             n_max = max_CHO},
         'Sugars'  = {n = paste(nutrient, '(g)')
                       n_min = min_sugars
                       n_max = max_sugars},
         'Fibre' = {n = paste(nutrient, '(g)')
                     n_min = min_fibre
                     n_max = max_fibre},
         'Protein' = {n = paste(nutrient, '(g)')
                     n_min = min_protein
                     n_max = max_protein},
         'Sodium' = {n = paste(nutrient, '(mg)')
                     n_min = min_sodium
                     n_max = max_sodium},
         'Fat (%)' = {n = nutrient
                     n_min = min_fat_perc
                     n_max = max_fat_perc},
         'Saturated fat (%)'= {n = nutrient
                               n_min = min_sat_fat_perc
                               n_max = max_sat_fat_perc},
         'Carbohydrates (%)' = {n = nutrient
                               n_min = min_CHO_perc
                               n_max = max_CHO_perc},
         'Sugars (%)' = {n = nutrient
                         n_min = min_sugars_perc
                         n_max = max_sugars_perc},
         'Protein (%)' = {n = nutrient
                         n_min = min_protein_perc
                         n_max = max_protein_perc},
         'Red meat'  = {n = paste(nutrient, '(g)')
                         n_min = min_redmeat
                         n_max = max_redmeat},
         'Alcohol (%)'  = {n = nutrient
                           n_min = min_alcohol_perc
                           n_max = max_alcohol_perc},
         'Discretionary (%)'  = {n = nutrient
                                 n_min = min_discretionary_perc
                                 n_max = max_discretionary_perc},
         'Takeaway (%)'  = {n = nutrient
                           n_min = min_takeaway_perc
                           n_max = max_takeaway_perc}
         )

  sliderInput(inputId = paste0('slider_nutrient_',nutrient), label = n,
              min = n_min, max = n_max, value = c(n_min,n_max), step = 1)
  
}

changeNamesNutrientTable <- function(x){
  if(x == 'energy_mj'){
    y <- 'Energy (MJ)'
  } else if(x == 'fat_g'){
    y <- 'Fat (g)'
  } else if(x == 'sat_fat_g'){
    y <- 'Saturated fat (g)'
  } else if(x == 'CHO_g'){
    y <- 'CHO (g)'
  } else if(x == 'sugars_g'){
    y <- 'Sugars (g)'
  } else if(x == 'sodium_mg'){
    y <- 'Sodium (mg)'
  } else if(x == 'fibre_g'){
    y <- 'Fibre (g)'
  } else if(x == 'protein_g'){
    y <- 'Protein (g)'
  } else if(x == 'protein_perc'){
    y <- 'Protein (%)'
  } else if(x == 'fat_perc'){
    y <- 'Fat (%)'
  } else if(x == 'sat_fat_perc'){
    y <- 'Saturated fat (%)'
  } else if(x == 'CHO_perc'){
    y <- 'CHO (%)'
  } else if(x == 'redmeat_g'){
    y <- 'Red meat (g)'
  } else if(x == 'sugars_perc'){
    y <- 'Sugars (%)'
  } else if(x == 'alcohol_perc'){
    y <- 'Alcohol (%)'
  } else if(x == 'discretionary_perc'){
    y <- 'Discretionary foods (%)'
  } else{
    y <- 'Takeaway (%)'
  }
  return(y)
  
}

#Modules------------------------------------------------------------------------

#UI - Food table
food_ui <- function(id, group_label){
  ns <- NS(id)
  
  tabPanel(group_label,
           box(
             width = 12, solidHeader = FALSE, status = 'warning',
             DTOutput(NS(id,'food_selection_output')),style = "overflow-y: scroll;overflow-x: scroll;"
           )
    
  )
}

#Server - Food table
food_server <- function(id, group_label){
  moduleServer(id, function(input, output, session){
    food_values <- reactiveValues()
    food_values$index <- NULL
    
    data <- reactive({foods_df[foods_df$food_group == group_label,]})
    
    output$food_selection_output <- DT::renderDataTable(
      datatable(
        data(),
        colnames = c('Food group', 'Food name', 'ID', 'CF/g(CO2)', 'WF/L', 'EF/gm2', 'Energy (kJ/g)','Fat (g)','Sat. fat (g)','CHO (g)','Sugars (g)', 'Fibre (g)','Protein (g)','Sodium (mg)','Price ($/100)'),
        selection = 'multiple',
        rownames = FALSE,
        width = '80%'
      )
    )
    
    food_values$index <- reactive(input$food_selection_output_rows_selected)
    
    return(reactive({data()$food_id[food_values$index()]}))
  })
}


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
                                  p('Be aware of the variable types: emission and price columns',strong('must'),'be numeric.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
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
                                div(
                                  id = 'foods_form',
                                  tabsetPanel(
                                    food_ui('alcohol', 'Alcohol'),
                                    food_ui('beverages', 'Beverages'),
                                    food_ui('dairy', 'Dairy/alternatives'),
                                    food_ui('discretionary', 'Discretionary foods'),
                                    food_ui('fats', 'Fats/oils'),
                                    food_ui('fruit', 'Fruit'),
                                    food_ui('grains', 'Grains'),
                                    food_ui('protein', 'Protein foods'),
                                    food_ui('sauces', 'Sauces/sugars'),
                                    food_ui('starchy', 'Starchy vegetables'),
                                    food_ui('takeaway', 'Takeaway'),
                                    food_ui('vegetables', 'Vegetables')
                                  )
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

#Constraints

#Food constraints
food_constraints_select_tab <- tabPanel('Food constraints',
                            useShinyjs(),

                            
                            )

food_constraints_pre_tab <- tabPanel('Food constraints',
                                        useShinyjs(),
                                        DTOutput('foodConstraintsDisplayOutput'),style = "overflow-y: scroll;overflow-x: scroll;"
                                       
)

#Food group constraints
food_group_constraints_select_tab <- tabPanel('Food group constraints',
                                 useShinyjs(),
)

food_group_constraints_pre_tab <- tabPanel('Food group constraints',
                                              useShinyjs(),
                                           DTOutput('foodGroupConstraintsDisplayOutput'),style = "overflow-y: scroll;overflow-x: scroll;"
)

#Nutrients constraints
nutrient_constraints_select_tab <- tabPanel('Nutrient constraints',
                                       useShinyjs(),
                                       DTOutput('nutrientsSelectOutput'),style = "overflow-y: scroll;overflow-x: scroll;"
)

nutrient_constraints_pre_tab <- tabPanel('Nutrient constraints',
                                            useShinyjs(),
                                         DTOutput('nutrientsConstraintsDisplayOutput'),style = "overflow-y: scroll;overflow-x: scroll;"
)


#General tab
constraint_tabs <- tabPanel('Constraints',
                               fluidRow(
                                 column(width = 3,
                                        tags$h3(span(HTML('Data insertion'), style = 'padding-left:15px')),
                                        box(
                                          width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                          radioButtons(
                                            "type_constraints_input",
                                            label = NULL,
                                            c('Pre-loaded profiles', 'Load your own data')
                                          ),
                                          br(),
                                          br()
                                        )),
                                 column(width = 3,
                                        conditionalPanel(
                                          condition = "input.type_constraints_input == 'Pre-loaded profiles'",
                                          tags$h3(span(HTML('Individual'), style = 'padding-left:15px')),
                                          box(
                                            width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                            radioButtons(
                                              "person_profiles_input",
                                              label = NULL,
                                              c('45-years old man', '37-years old woman', '12-years old boy', '8-years old girl')
                                            ),
                                            br(),
                                            br()
                                        )
                                        )
                                 ),
                                 column(width = 3,
                                        conditionalPanel(
                                          condition = "input.type_constraints_input == 'Pre-loaded profiles'",
                                          tags$h3(span(HTML('Diet'), style = 'padding-left:15px')),
                                          box(
                                            width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                            radioButtons(
                                              "diet_profiles_input",
                                              label = NULL,
                                              c('Current', 'EAT-Lancet', 'Healthy')
                                            ),
                                            br(),
                                            br()
                                          )
                                        )
                                 ),
                                 column(width = 3,
                                        conditionalPanel(
                                          condition = "input.type_constraints_input == 'Pre-loaded profiles' && input.diet_profiles_input != 'Healthy' && input.constraints_panel == 'Nutrient constraints' && (input.person_profiles_input == '45-years old man' || input.person_profiles_input == '37-years old woman') && input.nutrient_columns_input && (input.nutrient_columns_input.indexOf('Alcohol (%)') > -1 || input.nutrient_columns_input.indexOf('Discretionary (%)') > -1 || input.nutrient_columns_input.indexOf('Takeaway (%)') > -1)",
                                          tags$h3(span(HTML('Special groups intake'), style = 'padding-left:15px')),
                                          box(
                                              width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                              conditionalPanel(
                                                condition = "input.nutrient_columns_input && input.nutrient_columns_input.indexOf('Alcohol (%)') > -1",
                                                sliderInput(inputId = 'slider_alcohol_perc_input', label = 'Alcohol energy percentage',
                                                            min = min_alcohol_perc, max = max_alcohol_perc, value = c(min_alcohol_perc,max_alcohol_perc), step = 1)
                                              ),
                                              conditionalPanel(
                                                condition = "input.nutrient_columns_input && input.nutrient_columns_input.indexOf('Discretionary (%)') > -1",
                                                sliderInput(inputId = 'slider_discretionary_perc_input', label = 'Discretionary foods energy percentage',
                                                            min = min_discretionary_perc, max = max_discretionary_perc, value = c(min_discretionary_perc,max_discretionary_perc), step = 1)
                                              ),
                                              conditionalPanel(
                                                condition = "input.nutrient_columns_input && input.nutrient_columns_input.indexOf('Takeaway (%)') > -1",
                                                sliderInput(inputId = 'slider_takeaway_perc_input', label = 'Takeaway energy percentage',
                                                            min = min_takeaway_perc, max = max_takeaway_perc, value = c(min_takeaway_perc,max_takeaway_perc), step = 1)
                                              )
                                          )
                                        )
                                 )
                               ),
                            fluidRow(
                              column(width = 9,
                                     tabsetPanel(id = "constraints_panel",
                                                 useShinyjs(),
                                                 tabPanel("Food constraints",
                                                          box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                              conditionalPanel(
                                                                condition = "input.type_constraints_input == 'Load your own data'",
                                                                tags$h3(span(HTML('Value selection'), style = 'padding-left:15px')),
                                                                fluidRow(
                                                                  column(width = 8, 
                                                                         uiOutput('foodConstraintsSelectOutput'))
                                                                ),
                                                                tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                              ),       
                                                              DTOutput('foodConstraintsDisplayOutput', width = '95%'),style = "overflow-y: scroll;overflow-x: scroll;"
                                                              
                                                          )
                                                          
                                                          
                                                 ),
                                                 tabPanel('Food group constraints',
                                                          box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                              conditionalPanel(
                                                                condition = "input.type_constraints_input == 'Load your own data'",
                                                                tags$h3(span(HTML('Value selection'), style = 'padding-left:15px')),
                                                                fluidRow(
                                                                  column(width = 8, 
                                                                         uiOutput('foodGroupConstraintsSelectOutput'))
                                                                ),
                                                                tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                              ),
                                                              DTOutput('foodGroupConstraintsDisplayOutput'),style = "overflow-y: scroll;overflow-x: scroll;"
                                                          )
                                                 ),
                                                 tabPanel("Nutrient constraints",
                                                          box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                              conditionalPanel(
                                                                condition = "input.type_constraints_input == 'Load your own data'",
                                                                tags$h3(span(HTML('Value selection'), style = 'padding-left:15px')),
                                                                fluidRow(
                                                                  column(width = 8, 
                                                                         uiOutput('nutrientsConstraintsSelectOutput'))
                                                                ),
                                                                tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                              ),
                                                              DTOutput('nutrientsConstraintsDisplayOutput'),style = "overflow-y: scroll;overflow-x: scroll;"
                                                          )
                                                          
                                                          
                                                 ),
                                                 tabPanel('Linked foods',
                                                          box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                          conditionalPanel(
                                                            condition = "input.type_constraints_input == 'Pre-loaded profiles'",
                                                          conditionalPanel(
                                                            condition = 'output.linkedFoods1 == true || output.linkedFoods2 == true',
                                                                fluidRow(p('Linked foods are edibles whose consumption is evaluated together. The total serves of the foods in the lower bracket must be equal or lower than the consumption of the foods in the higher bracket.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                                                         p('I.e. since', strong("bread"), " and ",strong("butter")," are linked, and ", strong("bread"), " is the ", strong ("higher"), " food, it must have a total amount of serves at least equal to ", strong("butter"),".",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;")),
                                                                
                                                                conditionalPanel(
                                                                  condition = 'output.linkedFoods1 == true && output.linkedFoods2 == false',
                                                                  fluidRow(p('The standard dataset of DIETCOST has two pairs of linked foods: ', strong("bread/cream"), " and ",strong("milk/cereal"),". In your food database, only items for the first pair were selected. Please check the checkbox bellow if you want to add it as a constraint.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;")),
                                                                  fluidRow(
                                                                    column(width = 4,
                                                                           checkboxInput(inputId = 'linked_foods_1_input',
                                                                                         label = 'Bread/cream',
                                                                                         value = TRUE)),
                                                                    conditionalPanel(
                                                                      condition = 'input.linked_foods_1_input == true',
                                                                      column(width = 8,
                                                                             tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                             box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                               tabsetPanel(
                                                                               tabPanel('Lower foods',
                                                                                        DTOutput('linkedFoodsLowA1Output'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                               tabPanel('Higher foods',
                                                                                        DTOutput('linkedFoodsHighA1Output'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                             )))
                                                                    )

                                                                  )),
                                                            conditionalPanel(
                                                              condition = 'output.linkedFoods1 == false && output.linkedFoods2 == true',
                                                              fluidRow(p('The standard dataset of DIETCOST has two pairs of linked foods: ', strong("bread/cream"), " and ",strong("milk/cereal"),". In your food database, only items for the second pair were selected. Please check the checkbox bellow if you want to add it as a constraint.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;")),
                                                              fluidRow(
                                                                column(width = 4,
                                                                       checkboxInput(inputId = 'linked_foods_2_input',
                                                                                     label = 'Milk/cereal',
                                                                                     value = TRUE),
                                                                       ),
                                                                conditionalPanel(
                                                                  condition = 'input.linked_foods_2_input == true',
                                                                  column(width = 8,
                                                                         tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                         box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                             tabsetPanel(
                                                                               tabPanel('Lower foods',
                                                                                        DTOutput('linkedFoodsLowA2Output'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                               tabPanel('Higher foods',
                                                                                        DTOutput('linkedFoodsHighA2Output'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                             )))
                                                                )
                                                              )),
                                                            conditionalPanel(
                                                              condition = 'output.linkedFoods1 == true && output.linkedFoods2 == true',
                                                              fluidRow(p('The standard dataset of DIETCOST has two pairs of linked foods: ', strong("bread/cream"), " and ",strong("milk/cereal"),". Please check the checkboxes bellow if you want to add them as a constraint.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;")),
                                                              fluidRow(
                                                                column(width = 4,
                                                                       checkboxInput(inputId = 'linked_foods_t1_input',
                                                                                     label = 'Bread/cream',
                                                                                     value = TRUE),
                                                                       checkboxInput(inputId = 'linked_foods_t2_input',
                                                                                     label = 'Milk/cereal',
                                                                                     value = TRUE)),
                                                                

                                                                      column(width = 8,
                                                                            conditionalPanel(
                                                                              condition = 'input.linked_foods_t1_input == true && input.linked_foods_t2_input == true',
                                                                              tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                              box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                              tabsetPanel(
                                                                                tabPanel('Bread/cream',
                                                                                         tabsetPanel(
                                                                                           tabPanel('Lower foods',
                                                                                                    DTOutput('linkedFoodsLowB1Output'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                           tabPanel('Higher foods',
                                                                                                    DTOutput('linkedFoodsHighB1Output'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                           
                                                                                         )
                                                                                         ),
                                                                                tabPanel('Milk/cereal',
                                                                                         tabsetPanel(
                                                                                           tabPanel('Lower foods',
                                                                                                    DTOutput('linkedFoodsLowB2Output'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                           tabPanel('Higher foods',
                                                                                                    DTOutput('linkedFoodsHighB2Output'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                           
                                                                                         )
                                                                                )
                                                                              )
                                                                            )
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = 'input.linked_foods_t1_input == true && input.linked_foods_t2_input == false',
                                                                              tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                              box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                  tabsetPanel(
                                                                                    tabPanel('Lower foods',
                                                                                             DTOutput('linkedFoodsLowC1Output'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                    tabPanel('Higher foods',
                                                                                             DTOutput('linkedFoodsHighC1Output'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                    
                                                                                  )
                                                                              )
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = 'input.linked_foods_t1_input == false && input.linked_foods_t2_input == true',
                                                                              tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                              box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                  tabsetPanel(
                                                                                    tabPanel('Lower foods',
                                                                                             DTOutput('linkedFoodsLowC2Output'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                    tabPanel('Higher foods',
                                                                                             DTOutput('linkedFoodsHighC2Output'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                    
                                                                                  )
                                                                              )
                                                                            )
                                                                      )
                                                                )
                                                            

                                                            )
                                                            
                                                          )

                                                 )

                                                 )
                                                 )      
                                                 
                                                 
                                     )
                                     
                                     ),
                              conditionalPanel(
                                condition = "(input.type_constraints_input == 'Load your own data' && input.constraints_panel == 'Nutrient constraints')||(input.type_constraints_input == 'Pre-loaded profiles' && input.constraints_panel == 'Nutrient constraints')",
                                column(width = 3,
                                       tags$h3(span(HTML('Nutrients'), style = 'padding-left:15px')),
                                       box(
                                         width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                         checkboxGroupInput(
                                           "nutrient_columns_input",
                                           label = NULL,
                                           choices = c('Energy', 'Fat', 'Saturated fat', 'Carbohydrates', 'Sugars', 'Fibre', 'Protein', 'Sodium', 'Fat (%)', 'Saturated fat (%)', 'Carbohydrates (%)', 'Sugars (%)', 'Protein (%)', 'Red meat', 'Alcohol (%)', 'Discretionary (%)', 'Takeaway (%)'),
                                           selected = c('Energy', 'Fat', 'Saturated fat', 'Carbohydrates', 'Sugars', 'Fibre', 'Protein', 'Sodium', 'Fat (%)', 'Saturated fat (%)', 'Carbohydrates (%)', 'Sugars (%)', 'Protein (%)', 'Red meat', 'Alcohol (%)', 'Discretionary (%)', 'Takeaway (%)')
                                         ),
                                         br(),
                                         br()
                                       )
                                       )
                              )
                              

                              
                              #conditionalPanel(
                               # condition = "input.type_constraints_input == 'Load your own data'",
                                #tabsetPanel(
                                 # id = "tabset_select_profile",
                                  #food_constraints_select_tab,
                                  #food_group_constraints_select_tab,
                                  #nutrient_constraints_select_tab,
                                #)
                              #),
                              #tabsetPanel(
                               # food_constraints_pre_tab
                              #)
                              #conditionalPanel(
                                #condition = "input.type_constraints_input == 'Pre-loaded profiles'",
                                #column(width = 9,
                                      # tabsetPanel(
                                        # id = "tabset_pre_profile",
                                         #food_constraints_pre_tab,
                                         #food_group_constraints_pre_tab,
                                         #nutrient_constraints_pre_tab,
                                       #))

                                

                              #),
                              #conditionalPanel(
                                #condition = "(input.type_constraints_input == 'Load your own data' && input.tabset_select_profile == 'Nutrient constraints')||(input.type_constraints_input == 'Pre-loaded profiles' && input.tabset_pre_profile == 'Nutrient constraints')",
                                #tags$h3(span(HTML('Nutrients'), style = 'padding-left:15px')),
                                #column(width = 3,
                                 #      box(
                                  #       width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                  #       checkboxGroupInput(
                                  #         "nutrient_columns_input",
                                  #         label = NULL,
                                  #         choices = c('Energy', 'Fat', 'Saturated fat', 'Carbohydrates', 'Sugars', 'Fibre', 'Protein', 'Sodium', 'Fat (%)', 'Saturated fat (%)', 'Carbohydrates (%)', 'Sugars (%)', 'Protein (%)', 'Red meat', 'Alcohol (%)', 'Discretionary (%)', 'Takeaway (%)'),
                                  #         selected = c('Energy', 'Fat', 'Saturated fat', 'Carbohydrates', 'Sugars', 'Fibre', 'Protein', 'Sodium', 'Fat (%)', 'Saturated fat (%)', 'Carbohydrates (%)', 'Sugars (%)', 'Protein (%)', 'Red meat', 'Alcohol (%)', 'Discretionary (%)', 'Takeaway (%)')
                                  #       ),
                                   #      br(),
                                    #     br()
                                      # )
                                #)
                              #)
                              
                            )

                              
)

#General
ui <- navbarPage(title = 'DIETCOST',
                 theme = shinytheme("cosmo"),
                 useShinyjs(),
                 tags$head(
                   tags$style(HTML('
                                    rat{height: 60px}
                                    
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
                   ),
                   tags$script("var linked_foods_low_1 = ['69016', '69013', '79065'];
                                var linked_foods_high_1 = ['80066','80023']
                                
                                const indexesOf = (arr, item) => 
                                arr.reduce(
                                  (acc, v, i) => (v === item && acc.push(i), acc),
                                []);")
                 ),
                 header = tagList(useShinydashboard()),
                 intro_tab,
                 foods_tab,
                 constraint_tabs)

#Server-------------------------------------------------------------------------
server <- function(input, output, session){
  options(shiny.maxRequestSize=10*1024^2)
  data <- reactive(foods_df)
  data2 <- reactive(food_constraints_df)
  data3 <- reactive(food_group_constraints_df)
  data4 <- reactive(nutrient_targets_df)
  
  food_ids <- reactiveValues(ids = list(alcohol = food_server('alcohol', 'Alcohol'), 
                                        beverages = food_server('beverages', 'Beverages'),
                                        dairy = food_server('dairy', 'Dairy/alternatives'),
                                        discretionary = food_server('discretionary', 'Discretionary foods'),
                                        fats = food_server('fats', 'Fats/oils'),
                                        fruit = food_server('fruit', 'Fruit'),
                                        grains = food_server('grains', 'Grains'),
                                        protein = food_server('protein', 'Protein foods'),
                                        sauces = food_server('sauces', 'Sauces/sugars'),
                                        starchy = food_server('starchy', 'Starchy vegetables'),
                                        takeaway = food_server('takeaway', 'Takeaway'),
                                        vegetables = food_server('vegetables', 'Vegetables')))

  
  min_groups <- reactive(list(alcohol = input$slider_alcohol_perc_input[1],
                                             discretionary = input$slider_discretionary_perc_input[1],
                                             takeaway = input$slider_takeaway_perc_input[1]))
  
  max_groups <- reactive(list(alcohol = input$slider_alcohol_perc_input[2],
                                             discretionary = input$slider_discretionary_perc_input[2],
                                             takeaway = input$slider_takeaway_perc_input[2]))
  
  choices <- reactive(list(load_type = input$type_constraints_input,
                            foods = switch (input$person_profiles_input,
                                                                           '45-years old man' = {c('man_min', 'man_max')},
                                                                           '37-years old woman' = {c('woman_min', 'woman_max')},
                                                                           '12-years old boy' = {c('boy_min', 'boy_max')},
                                                                           '8-years old girl' = {c('girl_min', 'girl_max')}),
                           food_groups = switch (input$person_profiles_input,
                                                 '45-years old man' = {c('man_min_g', 'man_max_g', 'man_min_serve', 'man_max_serve')},
                                                 '37-years old woman' = {c('woman_min_g', 'woman_max_g', 'woman_min_serve', 'woman_max_serve')},
                                                 '12-years old boy' = {c('boy_min_g', 'boy_max_g', 'boy_min_serve', 'boy_max_serve')},
                                                 '8-years old girl' = {c('girl_min_g', 'girl_max_g', 'girl_min_serve', 'girl_max_serve')}),
                           nutrient_targets = switch(input$person_profiles_input,
                                                     '45-years old man' = 'man',
                                                     '37-years old woman' = 'woman',
                                                     '12-years old boy' = 'boy',
                                                     '8-years old girl' = 'girl'),
                                          plan = switch(input$diet_profiles_input,
                                                                      'Current' = 'C',
                                                                      'EAT-Lancet' = 'PF',
                                                                      'Healthy' = 'H')
                                          
  ))
  
  nutrient_cols <- reactive({
    ec <- ef <- esf <- echo <- es <- efib <- ep <- esod <- epperc <-  esfperc <-  efperc <-  echoperc <-  em <-  esperc <-  eaperc <-  edperc <-  etperc <- NULL
    for(i in 1:length(input$nutrient_columns_input)){
      switch(input$nutrient_columns_input[i],
             'Energy' = {ec <- c('energy_mj_min', 'energy_mj_max')}, 
             'Fat' = {ef <- c('fat_grams_min', 'fat_grams_max')}, 
             'Saturated fat' = {esf <- c('sat_fat_grams_min', 'sat_fat_grams_max')},
             'Carbohydrates' = {echo <- c('CHO_grams_min', 'CHO_grams_max')}, 
             'Sugars'= {es <- c('sugars_grams_min', 'sugars_grams_max')}, 
             'Fibre' = {efib <- c('fibre_grams_min', 'fibre_grams_max')}, 
             'Protein' = {ep <- c('protein_grams_min', 'protein_grams_max')}, 
             'Sodium' = {esod <- c('sodium_mgrams_min', 'sodium_mgrams_max')},
             'Protein (%)' = {epperc <- c('protein_perc_min', 'protein_perc_max')},
             'Saturated fat (%)' = {esfperc <- c('sat_fat_perc_min', 'sat_fat_perc_max')},
             'Fat (%)' = {efperc <- c('fat_perc_min', 'fat_perc_max')},
             'Carbohydrates (%)' = {echoperc <- c('CHO_perc_min', 'CHO_perc_max')},
             'Red meat' = {em <- c('redmeat_grams_min', 'redmeat_grams_max')},
             'Sugars (%)' = {esperc <- c('sugars_perc_min', 'sugars_perc_max')},
             'Alcohol (%)' = {eaperc <- c('alcohol_perc_min', 'alcohol_perc_max')},
             'Discretionary (%)' = {edperc <- c('discretionary_perc_min', 'discretionary_perc_max')},
             'Takeaway (%)' = {etperc <- c('takeaway_perc_min', 'takeaway_perc_max')},
      )
    }
    c(ec, ef, esf, echo, es, efib, ep, esod, epperc, esfperc, efperc, echoperc, em, esperc, eaperc, edperc, etperc)
  })
  
  restriction_food_values <- reactive({
    df_r <- data.frame(food_group = df1()$food_group,
                       food_name = df1()$food_name,
                       food_id = df1()$food_id)
    df_r$max_g <- df_r$min_g <- df_r$serve_size <- double(nrow(df1()))

    for(i in 1:nrow(df_r)){
      df_r$serve_size[i] <- coalesce(input[[paste0('numeric_food_', df_r$food_id[i])]],min_serve_size)
      df_r$min_g[i] <- coalesce(input[[paste0('slider_food_', df_r$food_id[i])]][1], min_grams_food)
      df_r$max_g[i] <- coalesce(input[[paste0('slider_food_', df_r$food_id[i])]][2], max_grams_food)
    }
    df_r
  })
  
  restriction_food_group_values <- reactive({
    food_groups <- sort(unique(df1()$food_group))
    df_r <- data.frame(food_group = food_groups)
    df_r$max_serve <- df_r$min_serve <- df_r$max_g <-df_r$min_g <-double(length(food_groups))
    
    for(food_group in food_groups){
      df_r$min_g[df_r$food_group == food_group] <- coalesce(input[[paste0('slider_food_group_g_',food_group)]][1], min_grams_food_group)
      df_r$max_g[df_r$food_group == food_group] <- coalesce(input[[paste0('slider_food_group_g_',food_group)]][2], max_grams_food_group)
      df_r$min_serve[df_r$food_group == food_group] <- coalesce(input[[paste0('slider_food_group_s_',food_group)]][1], min_serves)
      df_r$max_serve[df_r$food_group == food_group] <- coalesce(input[[paste0('slider_food_group_s_',food_group)]][2], max_serves)
    }
    df_r
  })
  
  
  restriction_nutrient_values <- reactive({
    df_r <- data.frame(nutrient = character(0),
                       min = double(0),
                       max = double(0))
    nutrients <- input$nutrient_columns_input
    for(nutrient in nutrients){
      switch(nutrient,
             'Energy' = {n <- 'energy_kj'},
             'Fat' = {n <- 'fat_g'},
             'Saturated fat' = {n <- 'fat_g'},
             'Carbohydrates' = {n <- 'CHO_g'},
             'Sugars' = {n <- 'sugars_g'},
             'Fibre' = {n <- 'fibre_g'},
             'Protein' = {n <- 'protein_g'},
             'Sodium' = {n <- 'sodium_mg'},
             'Fat (%)' = {n <- 'fat_perc'},
             'Saturated fat (%)' = {n <- 'sat_fat_perc'},
             'Carbohydrates (%)' = {n <- 'CHO_perc'},
             'Sugars (%)' = {n <- 'sugars_perc'},
             'Protein (%)' = {n <- 'protein_perc'},
             'Red meat' = {n <- 'redmeat_g'},
             'Alcohol (%)' = {n <- 'alcohol_perc'},
             'Discretionary (%)' = {n <- 'discretionary_perc'},
             'Takeaway (%)' = {n <- 'takeaway_perc'})
      df_r[nrow(df_r)+1,] <- c(n, input[[paste0('slider_nutrient_',nutrient)]][1], input[[paste0('slider_nutrient_',nutrient)]][2])
    }
    df_r
  })

  df1 <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      data() %>% filter(food_id %in% c(food_ids$ids$alcohol(), food_ids$ids$beverages(), food_ids$ids$dairy(), food_ids$ids$discretionary(), food_ids$ids$fats(), food_ids$ids$fruit(), food_ids$ids$grains(), food_ids$ids$protein(), food_ids$ids$sauces(), food_ids$ids$starchy(), food_ids$ids$takeaway(), food_ids$ids$vegetables())) 
    } else{
      file <- input$food_data_input
      req(file)
      if(file_ext(file$name) == 'xlsx'){
        temp_df <- read_excel(file$datapath)
        model_df <- read_excel('www/food_data_model.xlsx')
        if(identical(names(temp_df), names(model_df))){
          if(!isTRUE(any(sapply(temp_df[,c('CF_gCO2eq', 'WF_l', 'EF_g_m2', 'price')], is.character)))){
          if(!isTRUE(any(sapply(temp_df[,c('food_group','food_name', 'food_id')], is.na)))){
            read_excel(file$datapath)
          }
            
          }
          
        }
      }
      

      }
 

  })
  
  output$teste <- renderPrint({
    box({
      text <- list()
      for(i in 1:nrow(df1())){
        text <- append(text, list(paste0(input[[paste0('numeric_', df1()$food_id[i])]], ", ", input[[paste0('slider_', df1()$food_id[i])]][1], ", ", input[[paste0('slider_', df1()$food_id[i])]][2])))
      }
      text
    })

  })
  
  df2 <- reactive({
    if(input$type_constraints_input == 'Pre-loaded profiles'){
      columns <- c('food_group', 'food_name', 'food_id', 'serve_size', choices()$foods)
      data2() %>% filter(food_id %in% c(food_ids$ids$alcohol(), food_ids$ids$beverages(), food_ids$ids$dairy(), food_ids$ids$discretionary(), food_ids$ids$fats(), food_ids$ids$fruit(), food_ids$ids$grains(), food_ids$ids$protein(), food_ids$ids$sauces(), food_ids$ids$starchy(), food_ids$ids$takeaway(), food_ids$ids$vegetables()) & diet == choices()$plan) %>% select(all_of(columns))
    } else{
      restriction_food_values()
    }
  })
  
  df3 <- reactive({
    if(input$type_constraints_input == 'Pre-loaded profiles'){
      columns <- c('food_group', choices()$food_groups)
      data3() %>% filter(food_group %in% unique(df1()$food_group) & diet == choices()$plan) %>% select(all_of(columns))
    } else{
      restriction_food_group_values()
    }
  })
  
  df4 <- reactive({
    if(input$type_constraints_input == 'Pre-loaded profiles'){
      #columns <- c('energy_mj_min','energy_mj_max','fat_grams_min','fat_grams_max','sat_fat_grams_min','sat_fat_grams_max','CHO_grams_min','CHO_grams_max','sugars_grams_min','sugars_grams_max','fibre_grams_min','fibre_grams_max','protein_grams_min','protein_grams_max','sodium_mgrams_min','sodium_mgrams_max','protein_perc_min','protein_perc_max','sat_fat_perc_min','sat_fat_perc_max','fat_perc_min','fat_perc_max','CHO_perc_min','CHO_perc_max','redmeat_grams_min','redmeat_grams_max','sugars_perc_min','sugars_perc_max','alcohol_perc_min','alcohol_perc_max','discretionary_perc_min','discretionary_perc_max','takeaway_perc_min','takeaway_perc_max')
      data4() %>% filter(diet == choices()$plan & individual == choices()$nutrient_targets) %>% select(all_of(nutrient_cols())) %>% transposeNutrientsTable()
    } else{
      restriction_nutrient_values()
    }
  })
  
  observe({
    useShinyjs()
    if(length(food_ids$ids$alcohol()) == 0 && length(food_ids$ids$beverages()) == 0 && length(food_ids$ids$dairy()) == 0 && length(food_ids$ids$discretionary()) == 0 && length(food_ids$ids$fats()) == 0 && length(food_ids$ids$fruit()) == 0 && length(food_ids$ids$grains()) == 0 && length(food_ids$ids$protein()) == 0 && length(food_ids$ids$sauces()) == 0 && length(food_ids$ids$starchy()) == 0 && length(food_ids$ids$takeaway()) == 0 && length(food_ids$ids$vegetables()) == 0){
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
            if(!isTRUE(any(sapply(temp_df[,c('food_group','food_name', 'food_id')], is.na)))){
              enable('proceed_upload_input')
            } else{
              showModal(modalDialog("Check your file! There are missing values either in food group, name or ID columns."))
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
      food_ids <- reactiveValues(ids = list(  food_ids <- reactiveValues(ids =   list(alcohol = NULL, 
                                                                                      beverages = NULL,
                                                                                      dairy = NULL,
                                                                                      discretionary = NULL,
                                                                                      fats = NULL,
                                                                                      fruit = NULL,
                                                                                      grains = NULL,
                                                                                      protein = NULL,
                                                                                      sauces = NULL,
                                                                                      starchy = NULL,
                                                                                      takeaway = NULL,
                                                                                      vegetables = NULL))))

    }

  )

  observeEvent(
    input$nutrient_columns_input,
    {
      runjs(
        "
          $(document).ready(function(){
            $('input[name=nutrient_columns_input]').on('click', function(event){
              if($('input[name=nutrient_columns_input]:checked').length == 0){
                $(this).prop('checked', true);
              }
            });
          });
            "
      )
    }
  )
  
  output$food_data_model <- downloadHandler(
    filename = 'food_data_model.xlsx',
    content = function(file){
      file.copy('www/food_data_model.xlsx',file)
    }
  )


  
  output$foodConstraintsSelectOutput <- renderUI({
    tabs <- lapply(sort(unique(df1()$food_group)), tabSelectFoodFunction, df = df1())
    do.call(tabsetPanel, tabs)


  })
  
  output$foodGroupConstraintsSelectOutput <- renderUI({
    lapply(sort(unique(df1()$food_group)), tabSelectFoodGroupFunction) 
  })
  
  output$nutrientsConstraintsSelectOutput <- renderUI({
    lapply(input$nutrient_columns_input, tabSelectNutrientFunction) 
  })
  
  
  output$foodConstraintsDisplayOutput <- DT::renderDataTable({
    df_f <- data.frame(df2())
    colnames(df_f) <- c('food_group', 'food_name', 'food_id', 'serve_size', 'min_g', 'max_g')
    datatable(
      df_f,
      colnames = c('Food group', 'Food name', 'ID','Serve size (g)', 'Minimum intake (g)', 'Maximum intake (g)'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$foodGroupConstraintsDisplayOutput <- DT::renderDataTable({
    df_fg <- data.frame(df3())
    colnames(df_fg) <- c('food_group', 'min_g', 'max_g', 'min_serve', 'max_serve')
    datatable(
      df_fg,
      colnames = c('Food group', 'Minimum intake (g)', 'Maximum intake (g)', 'Minimum serves', 'Maximum serves'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  

  output$nutrientsConstraintsDisplayOutput <- DT::renderDataTable({
    df_n <- df4()
    df_n2 <- df_n
    df_n2$nutrient <- unlist(lapply(df_n2$nutrient, changeNamesNutrientTable))
    #if('discretionary_perc_min' %in% names(df_n) && (choices()$load_type == 'Pre-loaded profiles'||choices()$load_type == 'Load your own data')){
    #  df_n$discretionary_perc_min <- min_groups()$discretionary
    #  df_n$discretionary_perc_max <- max_groups()$discretionary
    #}
    #if('alcohol_perc_min' %in% names(df_n) && (choices()$load_type == 'Pre-loaded profiles'||choices()$load_type == 'Load your own data')){
    #  df_n$alcohol_perc_min <- min_groups()$alcohol
    #  df_n$alcohol_perc_max <- max_groups()$alcohol
    #}
    #if('takeaway_perc_min' %in% names(df_n) && (choices()$load_type == 'Pre-loaded profiles'||choices()$load_type == 'Load your own data')){
    #  df_n$takeaway_perc_min <- min_groups()$takeaway
    #  df_n$takeaway_perc_max <- max_groups()$takeaway
    #}
    
    #df4()$energy_mj_min <- df4()$energy_mj_min*1000
    #df4()$energy_mj_max <- df4()$energy_mj_max*1000
    #names(df4())[names(df4()) == 'energy_mj_min'] <- 'energy_kj_min'
    #names(df4())[names(df4()) == 'energy_mj_max'] <- 'energy_kj_max'
    datatable(
      df_n2,
      colnames = c('Nutrient', 'Minimum', 'Maximum'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsLowC1Output <- output$linkedFoodsLowB1Output <- output$linkedFoodsLowA1Output <- DT::renderDataTable({
    dfl1 <- df1() %>% filter(food_id %in% linked_low_1) %>% select(food_id, food_name, food_group)
    datatable(
      dfl1,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsHighC1Output <- output$linkedFoodsHighB1Output <- output$linkedFoodsHighA1Output <- DT::renderDataTable({
    dfh1 <- df1() %>% filter(food_id %in% linked_high_1) %>% select(food_id, food_name, food_group)
    datatable(
      dfh1,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsLowC2Output <- output$linkedFoodsLowB2Output <- output$linkedFoodsLowA2Output <- DT::renderDataTable({
    dfl2 <- df1() %>% filter(food_id %in% linked_low_2) %>% select(food_id, food_name, food_group)
    datatable(
      dfl2,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsHighC2Output <-output$linkedFoodsHighB2Output <-output$linkedFoodsHighA2Output <- DT::renderDataTable({
    dfh2 <- df1() %>% filter(food_id %in% linked_high_2) %>% select(food_id, food_name, food_group)
    datatable(
      dfh2,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoods1 <- reactive(any(linked_low_1 %in% df1()$food_id) && any(linked_high_1 %in% df1()$food_id))
  output$linkedFoods2 <- reactive(any(linked_low_2 %in% df1()$food_id) && any(linked_high_2 %in% df1()$food_id))
  
  
  outputOptions(output, "linkedFoods1", suspendWhenHidden = FALSE)
  outputOptions(output, "linkedFoods2", suspendWhenHidden = FALSE)

}

#App creation-------------------------------------------------------------------
shinyApp(ui = ui, server = server)

