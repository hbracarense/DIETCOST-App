library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyFiles)
library(readxl)
library(DT)
library(writexl)
library(tools)
library(vroom)
library(dplyr)
library(rlang)

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
min_energy <- round(min(nutrient_targets_df$energy_mj_min)*1000,0)
max_energy <- round(max(nutrient_targets_df$energy_mj_max)*1000,0)
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
linked_low_1_def <- c("69016", "69013", "79065")
linked_high_1_def <- c("80066", "80023")
linked_low_2_def <- "65021"
linked_high_2_def <- c("79006", "79088")
redmeat_ids <- c('71003', '71008', '71041', '81005', '81021', '81022', '81026', '81027', '81029')
model_foods <- c('food_group', 'food_name', 'food_id', 'CF_gCO2eq', 'WF_l', 'EF_g_m2', 'energy_kj_g', 'fat_g', 'sat_fat_g', 'CHO_g', 'sugars_g', 'fibre_g', 'protein_g', 'sodium_mg', 'price')
nutrient_colnames <- c('energy_kj_g', 'fat_g', 'sat_fat_g', 'CHO_g', 'sugars_g', 'fibre_g', 'protein_g', 'sodium_mg')
model_foods_cons_names <- c('food_group','food_name','food_id','size','min','max')
model_food_groups_cons_names <- c('food_group','min_g','max_g','min_serve','max_serve')
model_nutrients_cons_names <- c('energy_mj_min', 'energy_mj_max', 'fat_grams_min', 'fat_grams_max', 'sat_fat_grams_min', 'sat_fat_grams_max', 'CHO_grams_min', 'CHO_grams_max', 'sugars_grams_min', 'sugars_grams_max', 'fibre_grams_min', 'fibre_grams_max', 'protein_grams_min', 'protein_grams_max', 'sodium_mgrams_min', 'sodium_mgrams_max', 'protein_perc_min', 'protein_perc_max', 'sat_fat_perc_min', 'sat_fat_perc_max', 'fat_perc_min', 'fat_perc_max', 'CHO_perc_min', 'CHO_perc_max', 'redmeat_grams_min', 'redmeat_grams_max', 'sugars_perc_min', 'sugars_perc_max', 'alcohol_perc_min', 'alcohol_perc_max', 'discretionary_perc_min', 'discretionary_perc_max', 'takeaway_perc_min', 'takeaway_perc_max')
model_linked_names <- c('low', 'high')
emission_cols <- c('CF_gCO2eq', 'WF_l', 'EF_g_m2')
nutrient_pairs <- list(list('energy_mj_min','energy_mj_max'),
                       list('fat_grams_min', 'fat_grams_max'),
                       list('sat_fat_grams_min', 'sat_fat_grams_max'),
                       list('CHO_grams_min', 'CHO_grams_max'),
                       list('sugars_grams_min', 'sugars_grams_max'),
                       list('fibre_grams_min', 'fibre_grams_max'),
                       list('protein_grams_min', 'protein_grams_max'),
                       list('sodium_mgrams_min', 'sodium_mgrams_max'),
                       list('protein_perc_min', 'protein_perc_max'),
                       list('sat_fat_perc_min', 'sat_fat_perc_max'),
                       list('fat_perc_min', 'fat_perc_max'),
                       list('CHO_perc_min', 'CHO_perc_max'),
                       list('redmeat_grams_min', 'redmeat_grams_max'),
                       list('sugars_perc_min', 'sugars_perc_max'),
                       list('alcohol_perc_min', 'alcohol_perc_max'),
                       list('discretionary_perc_min', 'discretionary_perc_max'),
                       list('takeaway_perc_min', 'takeaway_perc_max'))

np <- nutrient_pairs[1:8]
nperc <- nutrient_pairs[c(11,10,12,14,9)]
n_b <- nutrient_colnames[c(2:5,7)]
f1 <- 37.7
f2 <- 16.7


#Functions----------------------------------------------------------------------

transposeNutrientsTable <- function(df){
  df_res <- data.frame(nutrient = character(0),
                       min = double(0),
                       max = double(0))
  
  for(i in 1:ncol(df)){
    if(i %% 2 == 0) next else{
      switch(colnames(df)[i],
             'energy_mj_min' = {n <- 'energy_kj_g'
             min_val <- df$energy_mj_min*1000
             max_val <- df$energy_mj_max*1000},
             
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
                        tags$div(class = "slider-custom",
                                 sliderInput(inputId = paste0('slider_food_',df_foods$food_id[i]), label = paste0(df_foods$food_name[i]),
                                             min = min_grams_food, max = max_grams_food, value = c(min_grams_food,max_grams_food), step = 50))),
                 column(width = 4,
                        br(),
                        numericInput(inputId = paste0('numeric_food_',df_foods$food_id[i]), label = 'Serve size (g)',
                                     min = min_serve_size, max = max_serve_size, value = 20)
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
           tags$div(class = "slider-custom",
                    sliderInput(inputId = paste0('slider_food_group_g_',food_group), label = "Intake (g)",
                                min = min_grams_food_group, max = max_grams_food_group, value = c(min_grams_food_group,max_grams_food_group), step = 50))),
    column(width = 6,
           tags$div(class = "slider-custom",
                    sliderInput(inputId = paste0('slider_food_group_s_',food_group), label = "Serves",
                                min = min_serves, max = max_serves, value = c(min_serves,max_serves), step = 5))
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
  tags$div(class = "slider-custom",
           sliderInput(inputId = paste0('slider_nutrient_',nutrient), label = n,
                       min = n_min, max = n_max, value = c(n_min,n_max), step = 1))
  
}

changeNamesNutrientTable <- function(x){
  if(x == 'energy_kj_g'){
    y <- 'Energy (kJ)'
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

verifyTabFile <- function(input_file, sheet_name, input_name){
  df <- tryCatch(expr = {read_excel(input_file, sheet = sheet_name)},
                 error = function(e){
                   showModal(
                     modalDialog(
                       title = 'Warning!',
                       p("Sheet ", strong(sheet_name), " wasn't found on file. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                       
                     )
                   )
                   
                   shinyjs::reset(input_name)})
  if(any(sapply(df,anyNA))){
    showModal(
      modalDialog(
        title = 'Warning!',
        p('Check your data. There are missing values.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')
        
      )
    )
    
    shinyjs::reset(input_name)
  }
  
  return(df)
}

verifyColumnNames <- function(df,model_names, sheet, input_name, mandatory = NULL){
  if(any(!(names(df) %in% model_names))){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Sheet ", strong(sheet), " has non-standard column names. Please check the model and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    
    shinyjs::reset(input_name)
    return(0)
  }
  
  if(!is.null(mandatory)){
    if(any(!(mandatory %in% names(df)))){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("Sheet ", strong(sheet), " doesn't have one of the mandatory columns. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      shinyjs::reset(input_name)
      return(0)
    } 
  }
  
  return(1)
}

whichNonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}

nonNumericCheck <- function(df, columns, sheet, input_name){
  for(column in columns){
    if(any(grepl('[a-zA-Z]',as.character(df[[column]])))){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("Sheets ", strong(sheet)," has non-numeric data in column: ", strong(column),". Please check and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset(input_name)
      return(0)
    }
  }
  return(1)
}

nutrientValueCheck <- function(df, sheet, nutrient_pairs, input_name){
  columns <- names(df)
  for(column in columns){
    if(any(grepl('[a-zA-Z]',as.character(df[[column]])))){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("Sheets ", strong(sheet)," has non-numeric data in column: ", strong(column),". Please check and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset(input_name)
      return(0)
    }
  }
  
  for(i in 1:length(nutrient_pairs)){
    if(nutrient_pairs[[i]][[1]] %in% names(df) && !is.na(df[nutrient_pairs[[i]][[1]]]) && !is.na(df[nutrient_pairs[[i]][[2]]]) && df[nutrient_pairs[[i]][[1]]] > df[nutrient_pairs[[i]][[2]]]){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("There are minimum nutrient constraints that exceed its maximum value. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset(input_name)
      return(0)
    }
    
  }
  return(1)
}

verifySpecialGroups <- function(col_name, group_name, df, df_foods, input_name){
  if(col_name %in% names(df)){
    if(df[col_name] > 0 && !(group_name %in% unique(df_foods))){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("The minimum energy intake from ", group_name, " is positive but there are no ", group_name, " in food data. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset(input_name)
      return(0)
    }
    
    col_name_max <- paste0(strsplit(col_name, 'min')[[1]], 'max')
    if(df[col_name] < 0 || df[col_name_max] > 100){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("Check the lower and upper bounds for ", group_name, ". Their percentage value must be between 0 and 100, respectively.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset(input_name)
      return(0)
    }
  }
  return(1)
}

verifyLinkedSingleTab <- function(path_name, sheet_name, model, input_name, food_ids){
  df <- read_excel(path_name, sheet = sheet_name)
  
  if(!identical(sort(names(df)), sort(model))){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Please check the column names in tab ", strong(sheet_name), ".", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    shinyjs::reset(input_name)
    return(list(low = NULL,
                high = NULL))
  }
  
  lk_low <- df$low[!is.na(df$low)]
  lk_high <- df$high[!is.na(df$high)]
  
  if(length(lk_low) == 0 || length(lk_high) == 0){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Please fill the tab ", strong(sheet_name), " with non-null values.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    shinyjs::reset(input_name)
    return(list(low = NULL,
                high = NULL))
  }
  
  if(length(whichNonnum(lk_low)) > 0 || length(whichNonnum(lk_high)) > 0){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Please use only numeric food IDs in tab ", strong(sheet_name), ".", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    shinyjs::reset(input_name)
    return(list(low = NULL,
                high = NULL))
  }
  
  if(length(intersect(lk_low, lk_high))>0){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Please make sure that values in low and high columns in ", strong(sheet_name), " are unique.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    shinyjs::reset(input_name)
    return(list(low = NULL,
                high = NULL))
  }
  
  if(any(!(c(lk_low, lk_high) %in% unique(food_ids)))){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Please make sure that all food IDs in low and high columns at tab ", strong(sheet_name), " are in your food data.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    shinyjs::reset(input_name)
    return(list(low = NULL,
                high = NULL))
  }
  return(list(low = lk_low,
              high = lk_high))
}

verifyGeneralIntersect <- function(list_vectors, input_name){
  results <- list()
  for(i in 1:length(list_vectors)){
    result <- mapply(intersect, x = list_vectors[i], y = list_vectors[c(-i)])
    results <- append(results, result)
  }
  if(length(unique(unlist(results)))>0){
    showModal(
      modalDialog(
        title = 'Warning!',
        p("Please make sure that all food IDs between both tabs of linked foods are unique.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
        
      )
    )
    shinyjs::reset(input_name)
  }
}

random_plan <- function(df, column, condition){
  random_parameter <- 0.4
  for(i in 1:nrow(df)){
    if(unlist(df[i, column]) %in% condition){
      selector <- runif(1)
      if(selector > random_parameter){
        df <- df[-i,]
      }
    }
  }
  return(df)
}

sample_safe <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}

calculateNutrientsRandomMeal <- function(df, nutrient_colnames){
  nutrient_c <- names(df)[names(df) %in% nutrient_colnames]
  
  col_n <- integer(length(nutrient_c))
  suppressWarnings(for(i in 1:length(nutrient_c)){
    col_n[i] <- grep(nutrient_c[i], colnames(df))
  }) 
  df <- df %>% mutate(
    across(
      .cols = all_of(col_n),
      .fns = function(x){
        (x/100)*df$intake
      }
    )
  )
  return(df)
}

priceEmissionData <- function(df, emission_cols){
  if('price' %in% names(df)){
    df$price <- (df$price/100)*df$intake
  }
  
  if('CF_gCO2eq' %in% names(df)){
    df$CF_gCO2eq <- (df$CF_gCO2eq/1000)*df$intake
  }
  
  if('WF_l' %in% names(df)){
    df$WF_l <- (df$WF_l/1000)*df$intake
  }
  
  if('EF_g_m2' %in% names(df)){
    df$EF_g_m2 <- (df$EF_g_m2/1000)*df$intake
  }
  return(df)
  
}

diff_calc <- function(val, min, max){
  res <- ifelse((val < min),
                {val - min},
                ifelse((val > max),
                       {val - max},
                       0))
  return(res)
}

checkLinkedFoods <- function(df, low, high){
  ls <- 0
  hs <- 0
  for(i in 1:length(low)){
    ls <- ifelse((low[i] %in% df$food_id),
                 {ls + df$serves[df$food_id == low[i]]},
                 ls)
  }
  for(i in 1:length(high)){
    hs <- ifelse((high[i] %in% df$food_id),
                 {hs + df$serves[df$food_id == high[i]]},
                 hs)
  }
  net <- hs - ls
  return(net)
}

calculateNutrientsTotal <- function(meal_df, df_cons){
  df_res <- data.frame(nutrient = unique(df_cons$nutrient),
                       value = double(nrow(df_cons)))
  
  for(i in 1:nrow(df_res)){
    switch(df_res$nutrient[i],
           'energy_kj_g' = {df_res$value[df_res$nutrient == 'energy_kj_g'] <- sum(meal_df$energy_kj_g, na.rm = TRUE)},
           'fat_g' = {df_res$value[df_res$nutrient == 'fat_g'] <- sum(meal_df$fat_g, na.rm = TRUE)},
           'sat_fat_g' = {df_res$value[df_res$nutrient == 'sat_fat_g'] <- sum(meal_df$sat_fat_g, na.rm = TRUE)},
           'CHO_g' = {df_res$value[df_res$nutrient == 'CHO_g'] <- sum(meal_df$CHO_g, na.rm = TRUE)},
           'sugars_g' = {df_res$value[df_res$nutrient == 'sugars_g'] <- sum(meal_df$sugars_g, na.rm = TRUE)},
           'protein_g' = {df_res$value[df_res$nutrient == 'protein_g'] <- sum(meal_df$protein_g, na.rm = TRUE)},
           'fat_perc' = {df_res$value[df_res$nutrient == 'fat_perc'] <- (sum(meal_df$fat_g, na.rm = TRUE)*f1)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
           'sat_fat_perc' = {df_res$value[df_res$nutrient == 'sat_fat_perc'] <- (sum(meal_df$sat_fat_g, na.rm = TRUE)*f1)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
           'CHO_perc' = {df_res$value[df_res$nutrient == 'CHO_perc'] <- (sum(meal_df$CHO_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
           'sugars_perc' = {df_res$value[df_res$nutrient == 'sugars_perc'] <- (sum(meal_df$sugars_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
           'fibre_g' = {df_res$value[df_res$nutrient == 'fibre_g'] <- sum(meal_df$fibre_g, na.rm = TRUE)},
           'protein_perc' = {df_res$value[df_res$nutrient == 'protein_perc'] <- (sum(meal_df$protein_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
           'redmeat_g' = {df_res$value[df_res$nutrient == 'redmeat_g'] <- sum(meal_df$intake[meal_df$food_id %in% redmeat_ids], na.rm = TRUE)},
           'sodium_mg' = {df_res$value[df_res$nutrient == 'sodium_mg'] <- sum(meal_df$sodium_mg, na.rm = TRUE)},
           'alcohol_perc' = {df_res$value[df_res$nutrient == 'alcohol_perc'] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Alcohol'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
           'discretionary_perc' = {df_res$value[df_res$nutrient == 'discretionary_perc'] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Discretionary foods'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
           'takeaway_perc' = {df_res$value[df_res$nutrient == 'takeaway_perc'] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Takeaway'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100}
    )
    
  }
  return(df_res)
}

calculateNutrientsDiff <- function(df_res, df_cons){
  df_diff <- df_res %>% left_join(df_cons, by = 'nutrient')
  df_diff$diff <- double(nrow(df_diff))
  for(i in 1:nrow(df_diff)){
    df_diff$diff[i] <- diff_calc(as.numeric(df_diff$value[i]), as.numeric(df_diff$min[i]), as.numeric(df_diff$max[i]))
  }
  df_diff <- df_diff[,-which(names(df_diff) %in% c('min', 'max', 'value'))]
  names(df_diff)[names(df_diff) == 'diff'] <- 'value'
  return(df_diff)
}

calculateServes <- function(df_meal){
  df_serves <- df_meal %>% group_by(food_group) %>% summarise(value = sum(serves))
  
  return(df_serves)
}

calculateServesDiff <- function(df_cons, df_serves){
  df_diff <- df_serves %>% left_join(df_cons, by = 'food_group')
  df_diff$diff <- double(nrow(df_diff))                             
  for(i in 1:nrow(df_diff)){
    df_diff$diff[i] <- diff_calc(as.numeric(df_diff$value[i]), as.numeric(df_diff$min_serve[i]), as.numeric(df_diff$max_serve[i]))
  }
  df_diff <- df_diff[,-which(names(df_diff) %in% c('min_g', 'max_g', 'min_serve','max_serve','value'))]
  names(df_diff)[names(df_diff) == 'diff'] <- 'value'
  return(df_diff)
}

calculateLinkedSum <- function(df_meal, low, high){
  ls <- NULL
  if(!is.null(low) && !is.null(high)){
    ls <- checkLinkedFoods(df_meal, low, high)
  }
  return(ls)
}

checkZeroDiff <- function(diff){
  if(all(diff$value == 0)) return(TRUE) else return(FALSE)
}

#Modules------------------------------------------------------------------------

#UI - Food table
food_ui <- function(id, group_label){
  ns <- NS(id)
  
  tabPanel(group_label,
           box(
             width = 12, solidHeader = FALSE, status = 'warning',
             DTOutput(NS(id,'food_selection_output')) %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"
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
intro_tab <- tabPanel(
  'Introduction',
  useShinyjs(),
  fluidRow(
    column(width = 2),
    column(
      width = 8,
      align = 'center',
      
      h1("Welcome to DIETCOST!"),
      br(),
      
      p(
        "The DIETCOST is a package originally developed by researchers at the University of Auckland, with the aim of estimating and comparing the cost of different dietary patterns. The package is currently available on CRAN and can be used by researchers, students, and policymakers interested in analyzing the economic accessibility of healthy and sustainable diets.",
        style = "text-align: justify; color: black; font-size:24px;"
      ),
      
      p(
        "The tool uses modeling techniques to generate food combinations that meet nutritional, cultural, and acceptability criteria, making it possible to estimate the cost of current, healthy, and sustainable diets in different contexts. In addition to cost, DIETCOST can be integrated with environmental indicators, such as carbon footprint and water footprint, expanding its application to studies on the sustainability of food systems.",
        style = "text-align: justify; color: black; font-size:24px;"
      ),
      
      p(
        "To facilitate the use of DIETCOST and broaden its reach, we have developed this application, which allows two forms of use: manual entry of information or upload of users’ own datasets. In this way, researchers, students, and professionals can carry out analyses in a simpler and more intuitive manner, even without prior programming experience.",
        style = "text-align: justify; color: black; font-size:24px;"
      ),
      
      p(
        "We hope this tool will contribute to advancing research on the cost and environmental impact of diets, supporting the production of scientific evidence and the development of public policies aimed at promoting healthy, sustainable, and economically accessible food.",
        style = "text-align: justify; color: black; font-size:24px;"
      ),
      
      p(
        "For questions or suggestions, please contact h@bracarense.com.",
        style = "text-align: justify; color: black; font-size:24px;"
      ),
      
      p(
        "We, researchers at the Federal University of Minas Gerais (UFMG), remain committed to promoting open science and to building a fairer, healthier, and more sustainable society.",
        style = "text-align: justify; color: black; font-size:24px;"
      ),
      
      br(),
      
      div(
        style = "text-align: center; margin-top: 20px;",
        tags$a(
          href = "dietcost_manual.pdf",
          download = "dietcost_manual.pdf",
          class = "btn btn-default",
          "Download manual",
          style = "color: #fff; background-color: #222222; border-color: #fff;"
        )
      )
    ),
    column(width = 2)
  )
)

#'Foods' tab
foods_tab <- tabPanel('Foods',
                      useShinyjs(),
                      div(
                        id = 'food_form',
                        conditionalPanel(
                          'input.saving_button == 0 & input.proceed_button == 0 & input.proceed_upload_button == 0',
                          style = "display: none;",
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
                              style = "display: none;",
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
                              style = "display: none;",
                              column(
                                width = 9,
                                tags$h3(span(HTML('Warning!'), style = 'padding-left:15px')),
                                box(
                                  width = 12, solidHeader = FALSE, status = 'warning',
                                  p('Your data must be in an Excel spreadsheet (.xlsx format).', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p('Also, your column names must be',strong('exactly'),'as the ones in the model sheet.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p('Be aware of the variable types: emission and price columns',strong('must'),'be numeric.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p("You may delete any nutrient column you don't want to evaluate, but please be aware that it must also be absent from the nutrient constraints file. Food group, food name and food ID are",strong('mandatory'), '.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                  p("However, your file must have at least",strong('one'), " nutrient column. If you don't wish to calculate the cost or the environmental impact of your diet, you're free to delete its columns from the file.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
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
                            style = "display: none;",
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
                                       shinyjs::disabled(                                       
                                         actionButton(
                                           inputId = "proceed_upload_input",
                                           label = "Proceed",
                                           style = "color: #fff; background-color: #222222; border-color: #fff;"
                                         ))
                                       
                                     )
                                     
                              ),
                              column(width = 4)
                            )
                          ),
                          conditionalPanel(
                            condition = "input.type_food_insert_input == 'Assemble food data from our database'",
                            style = "display: none;",
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
                            style = "display: none;",
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
                          style = "display: none;",
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

#General tab
constraint_tabs <- tabPanel('Constraints',
                            conditionalPanel(
                              style = "display: none;",
                              condition = '(input.saving_button == 0 && input.proceed_button == 0 && input.proceed_upload_button == 0) || (input.reset_food_button > 0 && input.reset_cons_button == 0)',
                              column(width = 4),
                              column(width = 4,
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     tags$h3(span(HTML('Warning!'), style = 'padding-left:15px')),
                                     box(
                                       width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                       p("Please set up your food database at the ", strong('Foods'), " tab before accessing this one.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                     ),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br()
                              ),
                              column(width = 4),
                            ),
                            conditionalPanel(
                              condition = '(input.saving_button > 0 || input.proceed_button > 0 || input.proceed_upload_button > 0) && input.proceed_upload_cons_button == 0 && input.proceed_cons_button == 0 && input.saving_cons_button == 0',
                              style = "display: none;",
                              conditionalPanel(
                                condition = "input.type_food_insert_input == 'Assemble food data from our database'",
                                style = "display: none;",
                                fluidRow(
                                  column(width = 3,
                                         tags$h3(span(HTML('Data insertion'), style = 'padding-left:15px')),
                                         box(
                                           width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                           radioButtons(
                                             "type_constraints_input",
                                             label = NULL,
                                             c('Pre-loaded profiles', 'Assemble your own constraints')
                                           ),
                                           br(),
                                           br()
                                         )),
                                  column(width = 3,
                                         conditionalPanel(
                                           condition = "input.type_constraints_input == 'Pre-loaded profiles'",
                                           style = "display: none;",
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
                                           style = "display: none;",
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
                                           condition = "input.type_constraints_input == 'Pre-loaded profiles' && input.diet_profiles_input != 'Healthy' && input.constraints_panel == 'Nutrient constraints' && (input.person_profiles_input == '45-years old man' || input.person_profiles_input == '37-years old woman') && input.nutrient_columns_input && (input.nutrient_columns_input.indexOf('Alcohol (%)') > -1 || input.nutrient_columns_input.indexOf('Discretionary (%)') > -1 || input.nutrient_columns_input.indexOf('Takeaway (%)') > -1) && (output.alcoholSelected == true || output.discretionarySelected == true || output.takeawaySelected == true)",
                                           style = "display: none;",
                                           tags$h3(span(HTML('Special groups intake'), style = 'padding-left:15px')),
                                           box(
                                             width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                             conditionalPanel(
                                               condition = "input.nutrient_columns_input && input.nutrient_columns_input.indexOf('Alcohol (%)') > -1 && output.alcoholSelected == true",
                                               style = "display: none;",
                                               tags$div(class = "slider-custom",
                                                        sliderInput(inputId = 'slider_alcohol_perc_input', label = 'Alcohol energy percentage',
                                                                    min = min_alcohol_perc, max = max_alcohol_perc, value = c(min_alcohol_perc,max_alcohol_perc), step = 1))
                                             ),
                                             conditionalPanel(
                                               condition = "input.nutrient_columns_input && input.nutrient_columns_input.indexOf('Discretionary (%)') > -1 && output.discretionarySelected == true",
                                               style = "display: none;",
                                               tags$div(class = "slider-custom",
                                                        sliderInput(inputId = 'slider_discretionary_perc_input', label = 'Discretionary foods energy percentage',
                                                                    min = min_discretionary_perc, max = max_discretionary_perc, value = c(min_discretionary_perc,max_discretionary_perc), step = 1))
                                             ),
                                             conditionalPanel(
                                               condition = "input.nutrient_columns_input && input.nutrient_columns_input.indexOf('Takeaway (%)') > -1 && output.takeawaySelected == true",
                                               style = "display: none;",
                                               tags$div(class = "slider-custom",
                                                        sliderInput(inputId = 'slider_takeaway_perc_input', label = 'Takeaway energy percentage',
                                                                    min = min_takeaway_perc, max = max_takeaway_perc, value = c(min_takeaway_perc,max_takeaway_perc), step = 1))
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
                                                                    condition = "input.type_constraints_input == 'Assemble your own constraints'",
                                                                    style = "display: none;",
                                                                    tags$h3(span(HTML('Value selection'), style = 'padding-left:15px')),
                                                                    fluidRow(
                                                                      column(width = 8, 
                                                                             uiOutput('foodConstraintsSelectOutput') %>% withSpinner(type = 4, color = '#000000'))
                                                                    ),
                                                                    tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                  ),       
                                                                  DTOutput('foodConstraintsDisplayOutput', width = '95%') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"
                                                                  
                                                              )
                                                              
                                                              
                                                     ),
                                                     tabPanel('Food group constraints',
                                                              box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                                  conditionalPanel(
                                                                    condition = "input.type_constraints_input == 'Assemble your own constraints'",
                                                                    style = "display: none;",
                                                                    tags$h3(span(HTML('Value selection'), style = 'padding-left:15px')),
                                                                    fluidRow(
                                                                      column(width = 8, 
                                                                             uiOutput('foodGroupConstraintsSelectOutput') %>% withSpinner(type = 4, color = '#000000'))
                                                                    ),
                                                                    tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                  ),
                                                                  DTOutput('foodGroupConstraintsDisplayOutput') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"
                                                              )
                                                     ),
                                                     tabPanel("Nutrient constraints",
                                                              box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                                  conditionalPanel(
                                                                    condition = "input.type_constraints_input == 'Assemble your own constraints'",
                                                                    style = "display: none;",
                                                                    tags$h3(span(HTML('Value selection'), style = 'padding-left:15px')),
                                                                    fluidRow(
                                                                      column(width = 8, 
                                                                             uiOutput('nutrientsConstraintsSelectOutput') %>% withSpinner(type = 4, color = '#000000'))
                                                                    ),
                                                                    tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                  ),
                                                                  DTOutput('nutrientsConstraintsDisplayOutput') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"
                                                              )
                                                              
                                                              
                                                     ),
                                                     tabPanel('Linked foods',
                                                              box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                                                  conditionalPanel(
                                                                    condition = "input.type_constraints_input == 'Pre-loaded profiles'",
                                                                    style = "display: none;",
                                                                    conditionalPanel(
                                                                      condition = 'output.linkedFoods1 == true || output.linkedFoods2 == true',
                                                                      style = "display: none;",
                                                                      fluidRow(p('Linked foods are edibles whose consumption is evaluated together. The total serves of the foods in the lower bracket must be equal or lower than the consumption of the foods in the higher bracket.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                                                                               p('I.e. since', strong("bread"), " and ",strong("butter")," are linked, and ", strong("bread"), " is the ", strong ("higher"), " food, it must have a total amount of serves at least equal to ", strong("butter"),".",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')),
                                                                      
                                                                      conditionalPanel(
                                                                        condition = 'output.linkedFoods1 == true && output.linkedFoods2 == false',
                                                                        style = "display: none;",
                                                                        fluidRow(p('The standard dataset of DIETCOST has two pairs of linked foods: ', strong("bread/cream"), " and ",strong("milk/cereal"),". In your food database, only items for the first pair were selected. Please check the checkbox bellow if you want to add it as a constraint.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')),
                                                                        fluidRow(
                                                                          column(width = 4,
                                                                                 checkboxInput(inputId = 'linked_foods_1_input',
                                                                                               label = 'Bread/cream',
                                                                                               value = TRUE)),
                                                                          conditionalPanel(
                                                                            condition = 'input.linked_foods_1_input == true',
                                                                            style = "display: none;",
                                                                            column(width = 8,
                                                                                   tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                                   box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                       tabsetPanel(
                                                                                         tabPanel('Lower foods',
                                                                                                  DTOutput('linkedFoodsLowA1Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                         tabPanel('Higher foods',
                                                                                                  DTOutput('linkedFoodsHighA1Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                       )))
                                                                          )
                                                                          
                                                                        )),
                                                                      conditionalPanel(
                                                                        condition = 'output.linkedFoods1 == false && output.linkedFoods2 == true',
                                                                        style = "display: none;",
                                                                        fluidRow(p('The standard dataset of DIETCOST has two pairs of linked foods: ', strong("bread/cream"), " and ",strong("milk/cereal"),". In your food database, only items for the second pair were selected. Please check the checkbox bellow if you want to add it as a constraint.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')),
                                                                        fluidRow(
                                                                          column(width = 4,
                                                                                 checkboxInput(inputId = 'linked_foods_2_input',
                                                                                               label = 'Milk/cereal',
                                                                                               value = TRUE),
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = 'input.linked_foods_2_input == true',
                                                                            style = "display: none;",
                                                                            column(width = 8,
                                                                                   tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                                   box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                       tabsetPanel(
                                                                                         tabPanel('Lower foods',
                                                                                                  DTOutput('linkedFoodsLowA2Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                         tabPanel('Higher foods',
                                                                                                  DTOutput('linkedFoodsHighA2Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                       )))
                                                                          )
                                                                        )),
                                                                      conditionalPanel(
                                                                        condition = 'output.linkedFoods1 == true && output.linkedFoods2 == true',
                                                                        style = "display: none;",
                                                                        fluidRow(p('The standard dataset of DIETCOST has two pairs of linked foods: ', strong("bread/cream"), " and ",strong("milk/cereal"),". Please check the checkboxes bellow if you want to add them as a constraint.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')),
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
                                                                                   style = "display: none;",
                                                                                   tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                                   box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                       tabsetPanel(
                                                                                         tabPanel('Bread/cream',
                                                                                                  tabsetPanel(
                                                                                                    tabPanel('Lower foods',
                                                                                                             DTOutput('linkedFoodsLowB1Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                                    tabPanel('Higher foods',
                                                                                                             DTOutput('linkedFoodsHighB1Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                                    
                                                                                                  )
                                                                                         ),
                                                                                         tabPanel('Milk/cereal',
                                                                                                  tabsetPanel(
                                                                                                    tabPanel('Lower foods',
                                                                                                             DTOutput('linkedFoodsLowB2Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                                    tabPanel('Higher foods',
                                                                                                             DTOutput('linkedFoodsHighB2Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                                    
                                                                                                  )
                                                                                         )
                                                                                       )
                                                                                   )
                                                                                 ),
                                                                                 conditionalPanel(
                                                                                   condition = 'input.linked_foods_t1_input == true && input.linked_foods_t2_input == false',
                                                                                   style = "display: none;",
                                                                                   tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                                   box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                       tabsetPanel(
                                                                                         tabPanel('Lower foods',
                                                                                                  DTOutput('linkedFoodsLowC1Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                         tabPanel('Higher foods',
                                                                                                  DTOutput('linkedFoodsHighC1Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                         
                                                                                       )
                                                                                   )
                                                                                 ),
                                                                                 conditionalPanel(
                                                                                   condition = 'input.linked_foods_t1_input == false && input.linked_foods_t2_input == true',
                                                                                   style = "display: none;",
                                                                                   tags$h3(span(HTML('Data display'), style = 'padding-left:15px')),
                                                                                   box(width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #ffffff",
                                                                                       tabsetPanel(
                                                                                         tabPanel('Lower foods',
                                                                                                  DTOutput('linkedFoodsLowC2Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"),
                                                                                         tabPanel('Higher foods',
                                                                                                  DTOutput('linkedFoodsHighC2Output') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;")
                                                                                         
                                                                                       )
                                                                                   )
                                                                                 )
                                                                          )
                                                                        )
                                                                        
                                                                        
                                                                      )
                                                                      
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = 'output.linkedFoods1 == false && output.linkedFoods2 == false',
                                                                      style = "display: none;",
                                                                      fluidRow(
                                                                        p("There are no foods whose consumption should be evaluated together at your database. If you wish to add this constraint, please select ", strong("Reset"), ' at the ', strong('Foods'), 'tab.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')
                                                                      )
                                                                    )
                                                                    
                                                                  ),
                                                                  conditionalPanel(
                                                                    condition = "input.type_constraints_input == 'Assemble your own constraints'",
                                                                    style = "display: none;",
                                                                    fluidRow(p('Linked foods are edibles whose consumption is evaluated together. The total serves of the foods in the lower bracket must be equal or lower than the consumption of the foods in the higher bracket.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                                                                             p('I.e. since', strong("bread"), " and ",strong("butter")," are linked, and ", strong("bread"), " is the ", strong ("higher"), " food, it must have a total amount of serves at least equal to ", strong("butter"),".",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                                                                             p('You can form up to two pairs of linked foods, from the database assembled at the ', strong('Foods'),' tab. Each low/high bracket can have a maximum of 8 distinct foods. Please check the checkbox below if you wish to add this constraint.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;')),
                                                                    fluidRow(column(width = 4,
                                                                                    checkboxInput(inputId = 'linked_foods_l1_input',
                                                                                                  label = 'Pair 1',
                                                                                                  value = TRUE)),
                                                                             conditionalPanel(
                                                                               condition = 'input.linked_foods_l1_input == true',
                                                                               style = "display: none;",
                                                                               column(width = 8,
                                                                                      selectizeInput(
                                                                                        inputId = 'pair_1_lower_input',
                                                                                        label = 'Low Foods',
                                                                                        choices = foods_df$food_name,
                                                                                        multiple = TRUE,
                                                                                        options = list(maxItems = 8)
                                                                                      ),
                                                                                      conditionalPanel(
                                                                                        condition = 'input.pair_1_lower_input.length > 0',
                                                                                        style = "display: none;",
                                                                                        selectizeInput(
                                                                                          inputId = 'pair_1_higher_input',
                                                                                          label = 'High Foods',
                                                                                          choices = foods_df$food_name,
                                                                                          multiple = TRUE,
                                                                                          options = list(maxItems = 8))
                                                                                      ))
                                                                             )),
                                                                    fluidRow(
                                                                      conditionalPanel(
                                                                        condition = 'input.linked_foods_l1_input == true && (input.pair_1_lower_input.length + input.pair_1_higher_input.length <= output.sizeFoods - 2) && input.pair_1_higher_input.length > 0',
                                                                        style = "display: none;",
                                                                        column(width = 4,
                                                                               checkboxInput(inputId = 'linked_foods_l2_input',
                                                                                             label = 'Pair 2',
                                                                                             value = TRUE)),
                                                                        conditionalPanel(
                                                                          condition = 'input.linked_foods_l2_input == true',
                                                                          style = "display: none;",
                                                                          column(width = 8,
                                                                                 selectizeInput(
                                                                                   inputId = 'pair_2_lower_input',
                                                                                   label = 'Low Foods',
                                                                                   choices = foods_df$food_name,
                                                                                   multiple = TRUE,
                                                                                   options = list(maxItems = 8)
                                                                                 ),
                                                                                 conditionalPanel(
                                                                                   condition = 'input.pair_2_lower_input.length > 0',
                                                                                   style = "display: none;",
                                                                                   selectizeInput(
                                                                                     inputId = 'pair_2_higher_input',
                                                                                     label = 'High Foods',
                                                                                     choices = foods_df$food_name,
                                                                                     multiple = TRUE,
                                                                                     options = list(maxItems = 8))
                                                                                 ))
                                                                        )
                                                                      )
                                                                    )
                                                                    
                                                                    
                                                                  )
                                                              )      
                                                              
                                                     ) 
                                         )
                                         
                                  ),
                                  conditionalPanel(
                                    condition = "(input.type_constraints_input == 'Assemble your own constraints' && input.constraints_panel == 'Nutrient constraints')||(input.type_constraints_input == 'Pre-loaded profiles' && input.constraints_panel == 'Nutrient constraints')",
                                    style = "display: none;",
                                    uiOutput('nutrientSelectionBox') %>% withSpinner(type = 4, color = '#000000')
                                    
                                  )
                                  
                                  
                                ),
                                conditionalPanel(
                                  condition = "input.type_food_insert_input == 'Assemble food data from our database'",
                                  style = "display: none;",
                                  fluidRow(
                                    div(
                                      style = "display: inline-block; position:relative; left:calc(39%);",
                                      shinyjs::disabled(downloadButton(
                                        "saving_cons_input",
                                        label = "Save and proceed",
                                        style = "color: #fff; background-color: #222222; border-color: #fff;"
                                      ))
                                    ),
                                    div(
                                      style = "display: inline-block; position:relative; left:calc(40.5%);",
                                      shinyjs::disabled(actionButton(
                                        inputId = "proceed_cons_input",
                                        label = "Proceed without saving",
                                        style = "color: #fff; background-color: #222222; border-color: #fff;"
                                      )
                                      )),
                                  )
                                )
                                
                              ),
                              conditionalPanel(
                                condition = "input.type_food_insert_input == 'Load your own data'",
                                style = "display: none;",
                                fluidRow(column(width = 2),
                                         column(
                                           width = 9,
                                           tags$h3(span(HTML('Warning!'), style = 'padding-left:15px')),
                                           box(
                                             width = 12, solidHeader = FALSE, status = 'warning',
                                             p('Your data must be in an Excel workbook (.xlsx format).', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p('This workbook ', strong('must'), ' follow the model attached below. It is ', strong('strongly advised'), ' to download the model before proceeding.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p('The Excel file must have three ', strong('mandatory'), ' tabs, named as: ', strong('food_constraints'), ', ', strong('food_group_constraints'), ' and ', strong('nutrient_targets'), '.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p('Also, it can contain two ',strong('optional'),' tabs: ', strong('linked_foods_pair_1'), ' and ', strong('linked_foods_pair_2'), ". If you don't want to use linked foods as a constraint, you can safely delete both tabs. Alternatively, if you wish to use only one pair of linked foods, delete only ", strong('linked_foods_pair_2'), " and keep ", strong('linked_foods_pair_1'), ". Please don't include a ", strong('linked_foods_pair_2'), " tab without a ", strong('linked_foods_pair_1'), ".", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p('The linked foods tab is comprised of two columns, named ',strong('low'),' and ', strong('high'), ", that should receive only the unique food IDs of the linked foods. It's not necessary for the columns to be of the same size, i.e. the low bracket can contain three foods and the high bracket only one. But please make sure that all the IDs referred in this tab are actually present at the food data previously uploaded and that they are not repeated - a food cannot be simultaneously in both brackets or in both pairs at the same time.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p('All the data should refer only to a single individual and diet, i.e. ', strong('man'),' with a ', strong('healthy'), " diet. It isn't necessary to explicitly name them in the dataset. As such, you shouldn't include columns such as ", strong('individual'),'/',strong('person'), ' or ', strong('diet'), style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p("Don't change column names. All the columns are mandatory for proper data loading, except the constraints in the ", strong('nutrient_targets')," tab. For an instance, If you don't want to use fat (g) intake as a constraint, you can safely delete both ", strong('fat_grams_min'), ' and ', strong('fat_grams_max'), '.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p(strong("Attention!"), "All constraints work in pairs. If you don't want to use one, you have to delete both the ", strong('minimum'), " and ", strong('maximum'), " columns.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p("Finally, please make sure that all the foods submitted in the previous section have constraints data in the .", strong("food_constraints"), " tab. The same is valid for all the food groups present in the food data in the ", strong("food_group_constraints"), " tab.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             p("Once again, it's advised to download the reference file. In case of any doubts, please refer to it. File size is up to 10MB. After submitting, please click the ", strong('Proceed'),' button below.',style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                             div(
                                               style = "display: inline-block; position:relative; left:calc(37.5%);",
                                               downloadButton(
                                                 "constraints_data_model",
                                                 label = "Download constraints model",
                                                 style = "color: #fff; background-color: #222222; border-color: #fff;"
                                               )
                                             )
                                           )
                                         ),
                                         column(width = 2)),
                                fluidRow(
                                  column(width = 4),
                                  column(width = 4,
                                         tags$h3(span(HTML('Data input'), style = 'padding-left:15px')),
                                         box(
                                           width = 12, solidHeader = FALSE, status = 'warning',
                                           fileInput('constraints_data_input', NULL, accept = '.xlsx')
                                         )),
                                  column(width = 4)
                                ),
                                fluidRow(
                                  column(width = 4),
                                  column(width = 4,
                                         div(
                                           style = "display: inline-block; position:relative; left:calc(37.5%);",
                                           shinyjs::disabled(                                       
                                             actionButton(
                                               inputId = "proceed_upload_cons_input",
                                               label = "Proceed",
                                               style = "color: #fff; background-color: #222222; border-color: #fff;"
                                             ))
                                           
                                         )
                                         
                                  ),
                                  column(width = 4)
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = '(input.proceed_upload_cons_button > 0 || input.proceed_cons_button > 0 || input.saving_cons_button > 0) && (input.saving_button > 0 || input.proceed_button > 0 || input.proceed_upload_button > 0)',
                              style = "display: none;",
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
                                  tags$h3(span(HTML('Constraints data loaded!'), style = 'padding-left:15px')),
                                  box(
                                    width = 12, solidHeader = FALSE, status = 'warning',
                                    p('Constraints data loaded with success!', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                    p('If you wish to reset the data, please click on the button below.', style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                    div(
                                      style = "display: inline-block; position:relative; left:calc(37.5%);",
                                      actionButton(
                                        inputId = "reset_cons_input",
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



#Simulation tab
simulation_tab <- tabPanel("Simulation",
                           useShinyjs(),
                           tags$style(HTML("#log {height:200px}")),
                           conditionalPanel(
                             condition = '(input.proceed_upload_cons_button == 0 && input.proceed_cons_button == 0 && input.saving_cons_button == 0) || input.reset_button >0 || input.reset_cons_button > 0',
                             style = "display: none;",
                             column(width = 4),
                             column(width = 4,
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    tags$h3(span(HTML('Warning!'), style = 'padding-left:15px')),
                                    box(
                                      width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                      p("Please set up your food database at the ", strong('Foods'), " tab and your constraints at ", strong("Constraints"), " before accessing this one.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                    ),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                             ),
                             column(width = 4),
                           ),
                           conditionalPanel(
                             condition = '(input.saving_button > 0 || input.proceed_button > 0 || input.proceed_upload_button > 0) && (input.proceed_upload_cons_button > 0 || input.proceed_cons_button > 0 || input.saving_cons_button > 0)',
                             style = "display: none;",
                             useShinyjs(),
                             column(width = 4,
                                    fluidRow(
                                      column(width = 12,
                                             tags$h3(span(HTML('Folder'), style = 'padding-left:15px')),
                                             box(
                                               width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                               p("Please select the directory where the results folder will be created.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                               div(style = "display: inline-block; position:relative; left:calc(37.5%);",
                                                   shinyDirButton(id = 'folder_input', title = '', label = 'Selection', multiple = FALSE)
                                               )
                                               
                                             ),
                                      )
                                    ),
                                    fluidRow(
                                      
                                      column(width = 6,
                                             tags$h3(span(HTML('Iterations'), style = 'padding-left:15px')),
                                             box(
                                               width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                               p("Set the number of iterations of the Monte Carlo simulation.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                               numericInput('iteration_input', '', min = 1, max =1000000000, value = 1000000)
                                             )
                                      ),
                                      column(
                                        width = 6,
                                        tags$h3(span(HTML('Serve size difference'), style = 'padding-left:15px')),
                                        box(
                                          width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                          p("Set the minimum serve size difference.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
                                          numericInput('difference_input', '', min = 0.01, max =0.99, value = 0.5)
                                        )
                                      )
                                      
                                    ),
                                    uiOutput("pickerColumn") %>% withSpinner(type = 4, color = '#000000'),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             div(style = "display: inline-block; position:relative; left:calc(73.5%);",
                                                 shinyjs::disabled(actionButton('run_input', label = 'Run'))
                                                 
                                             )
                                      ),
                                      fluidRow(
                                        div(style = "display: inline-block; position:relative; left:calc(0.5%);",
                                            shinyjs::disabled(actionButton('stop_input', label = 'Stop')))
                                        
                                      )
                                      
                                    )
                                    
                             ),
                             
                             column(width = 8,
                                    fluidRow(tags$h3(span(HTML('Initial random meal'), style = 'padding-left:15px')),
                                             box(
                                               width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                               DTOutput('randomMeal') %>% withSpinner(type = 4, color = '#000000'),style = "overflow-y: scroll;overflow-x: scroll;"
                                             )
                                             
                                             
                                    ),
                                    conditionalPanel(
                                      condition = 'input.run_input > 0',
                                      style = "display: none;",
                                      fluidRow(
                                        tags$h3(span(HTML('Monte Carlo simulation'), style = 'padding-left:15px')),
                                        box(
                                          width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
                                          verbatimTextOutput('log')
                                        )
                                      ))
                                    
                             ),
                             #fluidRow(DTOutput('teste2'),style = "overflow-y: scroll;overflow-x: scroll;")
                           ),
                           fluidRow(uiOutput('exportButtons'))
                           
)

#General
ui <- navbarPage(title = 'DIETCOST',
                 theme = shinytheme("cosmo"),
                 useShinyjs(),
                 tags$head(
                   tags$script(HTML("
                                    $(document).ready(function(){
  var objDiv = document.getElementById('log');
  // create an observer instance
  var observer = new MutationObserver(function(mutations) {
    objDiv.scrollTop = objDiv.scrollHeight - objDiv.clientHeight;
  });
  // configuration of the observer
  var config = {childList: true};
  // observe objDiv
  observer.observe(objDiv, config);
});
                                    ")),
                   
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
                   tags$style(HTML("
                                    .slider-custom .irs-bar {
                                      background: #000000;
                                      border-top-color: #000000;
                                      border-bottom-color: #000000;
                                    }
                                    .slider-custom .irs-slider {
                                      background: #000000;
                                    }
                                    
                                          .slider-custom .irs-from, .slider-custom .irs-to, .slider-custom .irs-single {
                                          background-color: #dddddd;
                                          color: #000000;
                                        }
                                    
                                    .slider-custom .irs-handle {
                                        background-color: #636363;
                                        border-color: #636363;
                                      }
                                  ")),
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   ),
                   tags$style("input[type=checkbox] { accent-color: black; }"),
                   tags$style("input[type=radio] { accent-color: black; }"),
                   tags$style(HTML("table.dataTable tbody tr.selected td,
                                  table.dataTable tbody td.selected {
                                      border-top-color: white !important;
                                      box-shadow: inset 0 0 0 9999px #000000 !important;
                                  }
                                  
                                      table.dataTable tbody tr:active td {
                                      background-color: #000000 !important;
                                  }
                                  
                                  :root {
                                      --dt-row-selected: transparent !important;
                                  }
                                  
                                  table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
                                      background-color: #000000 !important;
                                      color: #ffffff;
                                  }
                                  "))
                 ),
                 header = tagList(useShinydashboard()),
                 intro_tab,
                 foods_tab,
                 constraint_tabs,
                 simulation_tab)

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
    req(input$nutrient_columns_input)
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
    df_r$max <- df_r$min <- df_r$size <- double(nrow(df1()))
    
    for(i in 1:nrow(df_r)){
      df_r$size[i] <- coalesce(input[[paste0('numeric_food_', df_r$food_id[i])]],min_serve_size)
      df_r$min[i] <- coalesce(input[[paste0('slider_food_', df_r$food_id[i])]][1], min_grams_food)
      df_r$max[i] <- coalesce(input[[paste0('slider_food_', df_r$food_id[i])]][2], max_grams_food)
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
             'Energy' = {n <- 'energy_kj_g'},
             'Fat' = {n <- 'fat_g'},
             'Saturated fat' = {n <- 'sat_fat_g'},
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
      food_upload_inputs$foods
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
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        columns <- c('food_group', 'food_name', 'food_id', 'serve_size', choices()$foods)
        df_pr <- data2() %>% filter(food_id %in% c(food_ids$ids$alcohol(), food_ids$ids$beverages(), food_ids$ids$dairy(), food_ids$ids$discretionary(), food_ids$ids$fats(), food_ids$ids$fruit(), food_ids$ids$grains(), food_ids$ids$protein(), food_ids$ids$sauces(), food_ids$ids$starchy(), food_ids$ids$takeaway(), food_ids$ids$vegetables()) & diet == choices()$plan) %>% select(all_of(columns))
        colnames(df_pr) <- c('food_group', 'food_name','food_id','size', 'min', 'max' )
        df_pr
      } else{
        restriction_food_values()
      }
    }else{
      constraint_inputs$foods
    }
    
    
  })
  
  
  df3 <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        columns <- c('food_group', choices()$food_groups)
        df_prov <- data3() %>% filter(food_group %in% unique(df1()$food_group) & diet == choices()$plan) %>% select(all_of(columns))
        colnames(df_prov) <- c('food_group', 'min_g', 'max_g', 'min_serve', 'max_serve')
        df_prov
      } else{
        restriction_food_group_values()
      }
    }else{
      constraint_inputs$food_groups
    }
    
    
  })
  
  df4 <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        req(nutrient_cols())
        #columns <- c('energy_mj_min','energy_mj_max','fat_grams_min','fat_grams_max','sat_fat_grams_min','sat_fat_grams_max','CHO_grams_min','CHO_grams_max','sugars_grams_min','sugars_grams_max','fibre_grams_min','fibre_grams_max','protein_grams_min','protein_grams_max','sodium_mgrams_min','sodium_mgrams_max','protein_perc_min','protein_perc_max','sat_fat_perc_min','sat_fat_perc_max','fat_perc_min','fat_perc_max','CHO_perc_min','CHO_perc_max','redmeat_grams_min','redmeat_grams_max','sugars_perc_min','sugars_perc_max','alcohol_perc_min','alcohol_perc_max','discretionary_perc_min','discretionary_perc_max','takeaway_perc_min','takeaway_perc_max')
        df_prov <- data4() %>% filter(diet == choices()$plan & individual == choices()$nutrient_targets) %>% select(all_of(nutrient_cols())) 
        if('alcohol_perc_min' %in% nutrient_cols()){
          df_prov$alcohol_perc_min <- min_groups()$alcohol
          df_prov$alcohol_perc_max <- max_groups()$alcohol
        }
        if('discretionary_perc_min' %in% nutrient_cols()){
          df_prov$discretionary_perc_min <- min_groups()$discretionary
          df_prov$discretionary_perc_max <- max_groups()$discretionary
        }
        if('takeaway_perc_min' %in% nutrient_cols()){
          df_prov$takeaway_perc_min <- min_groups()$takeaway
          df_prov$takeaway_perc_max <- max_groups()$takeaway
        }
        df_prov %>% transposeNutrientsTable()
      } else{
        restriction_nutrient_values()
      }
    }else{
      if(!is.null(constraint_inputs$nutrients)){
        constraint_inputs$nutrients %>% transposeNutrientsTable()
        
      }
      
    }
    
    
  })
  
  linked_1_low <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        if(any(linked_low_1_def %in% df1()$food_id) && any(linked_high_1_def %in% df1()$food_id)){
          if(isTRUE(input$linked_foods_1_input)||isTRUE(input$linked_foods_t1_input)){
            df1() %>% filter(food_id %in% linked_low_1_def) %>% pull(food_id)
          } else NULL
        } else NULL
      }else{
        if(isTRUE(input$linked_foods_l1_input)){
          if(length(input$pair_1_lower_input) > 0 && length(input$pair_1_higher_input) > 0){
            df1() %>% filter(food_name %in% input$pair_1_lower_input) %>% pull(food_id)
          } else NULL
        } else NULL
      }} else{
        constraint_inputs$linked_1_low
      }
    
    
    
  })
  
  linked_1_high <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        if(any(linked_low_1_def %in% df1()$food_id) && any(linked_high_1_def %in% df1()$food_id)){
          if(isTRUE(input$linked_foods_1_input)||isTRUE(input$linked_foods_t1_input)){
            df1() %>% filter(food_id %in% linked_high_1_def) %>% pull(food_id)
          } else NULL
        } else NULL
      }else{
        if(isTRUE(input$linked_foods_l1_input)){
          if(length(input$pair_1_lower_input) > 0 && length(input$pair_1_higher_input) > 0){
            df1() %>% filter(food_name %in% input$pair_1_higher_input) %>% pull(food_id)
          } else NULL
        } else NULL
      }} else{
        constraint_inputs$linked_1_high
      }
    
    
    
  })
  
  linked_2_low <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        if(any(linked_low_2_def %in% df1()$food_id) && any(linked_high_2_def %in% df1()$food_id)){
          if(isTRUE(input$linked_foods_2_input)||isTRUE(input$linked_foods_t2_input)){
            df2() %>% filter(food_id %in% linked_low_2_def) %>% pull(food_id)
          } else NULL
        } else NULL
      }else{
        if(isTRUE(input$linked_foods_l2_input)){
          if(length(input$pair_2_lower_input) > 0 && length(input$pair_2_higher_input) > 0){
            df2() %>% filter(food_name %in% input$pair_2_lower_input) %>% pull(food_id)
          } else NULL
        } else NULL
      }} else{
        constraint_inputs$linked_2_low
      }
    
    
    
  })
  
  linked_2_high <- reactive({
    if(input$type_food_insert_input == 'Assemble food data from our database'){
      if(input$type_constraints_input == 'Pre-loaded profiles'){
        if(any(linked_low_2_def %in% df1()$food_id) && any(linked_high_2_def %in% df1()$food_id)){
          if(isTRUE(input$linked_foods_2_input)||isTRUE(input$linked_foods_t2_input)){
            df2() %>% filter(food_id %in% linked_high_2_def) %>% pull(food_id)
          } else NULL
        } else NULL
      }else{
        if(isTRUE(input$linked_foods_l2_input)){
          if(length(input$pair_2_lower_input) > 0 && length(input$pair_2_higher_input) > 0){
            df2() %>% filter(food_name %in% input$pair_2_higher_input) %>% pull(food_id)
          } else NULL
        } else NULL
      }} else{
        constraint_inputs$linked_2_high
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
  
  food_upload_inputs <- reactiveValues(foods = NULL)
  
  observeEvent(input$food_data_input,
               {
                 x1f <- 1
                 x2f <- x3f <- x4f <- x5f <- x6f <- x7f <-  0
                 df_food_data <- tryCatch(expr = {read_excel(input$food_data_input$datapath)},
                                          error = function(e){
                                            showModal(
                                              modalDialog(
                                                title = 'Warning!',
                                                p("It wasn't possible to read the file. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                                                
                                              )
                                            )
                                            shinyjs::reset('food_data_input')
                                            x1f <- 0
                                          })
                 
                 if(any(!(names(df_food_data) %in% model_foods))){
                   showModal(
                     modalDialog(
                       title = 'Warning!',
                       p("The column names in your file don't match the standard ones. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                       
                     )
                   )
                   
                   shinyjs::reset('food_data_input')
                 } else x2f <- 1
                 
                 
                 if(any(!(c('food_group','food_name','food_id') %in% names(df_food_data)))){
                   showModal(
                     modalDialog(
                       title = 'Warning!',
                       p("Your data doesn't have one of the mandatory columns: food ID, food name or food group. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                       
                     )
                   )
                   
                   shinyjs::reset('food_data_input')
                 } else x3f <- 1
                 
                 
                 if("food_id" %in% names(df_food_data)){
                   if(any(is.na(df_food_data$food_id))||(length(unique(df_food_data$food_id))!=nrow(df_food_data))||length(whichNonnum(df_food_data$food_id))>0){
                     showModal(
                       modalDialog(
                         title = 'Warning!',
                         p("Please check if every food has an unique numeric ID.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                         
                       )
                     )
                     
                     shinyjs::reset('food_data_input')
                   } else x4f <- 1
                 } 
                 
                 if(all(c('food_group', 'food_name') %in% names(df_food_data))){
                   if(any(is.na(df_food_data$food_group))||any(is.na(df_food_data$food_name))){
                     showModal(
                       modalDialog(
                         title = 'Warning!',
                         p("Please check if every food has a name and a group.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                         
                       )
                     )
                     
                     shinyjs::reset('food_data_input')
                   }  else x5f <- 1
                 }
                 
                 if(length(intersect(nutrient_colnames, names(df_food_data))) == 0){
                   showModal(
                     modalDialog(
                       title = 'Warning!',
                       p("Your data must have at least one nutrient column. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                       
                     )
                   )
                   
                   shinyjs::reset('food_data_input')
                 }  else x6f <- 1
                 
                 for(i in 4:length(model_foods)){
                   if(model_foods[i] %in% names(df_food_data)){
                     if((any(grepl('[a-zA-Z]',as.character(df_food_data[[model_foods[i]]]))))||(any(is.na(df_food_data[model_foods[i]])))){
                       showModal(
                         modalDialog(
                           title = 'Warning!',
                           p("Column ", strong(model_foods[i]), " must have only non-NA numeric data. Please check and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
                           
                         )
                       )
                       
                       shinyjs::reset('food_data_input')
                       break
                     } else{
                       if(i == length(model_foods)) x7f <- 1
                     }
                   }
                 }
                 
                 
                 if(x1f + x2f + x3f + x4f + x5f + x6f  + x7f == 7){
                   food_upload_inputs$foods <- df_food_data
                   shinyjs::enable('proceed_upload_input')
                   shinyjs::disable('food_data_input')
                 }
                 
               })
  
  
  output$saving_input <- downloadHandler(
    filename = 'food_data.xlsx',
    content = function(file){
      write_xlsx(list('Foods' = df1()), file)
    }
  )
  
  output$saving_cons_input <- downloadHandler(
    filename = 'constraints_data.xlsx',
    content = function(file){
      file_content <- list(df2(), df3(), df4(), data.frame('low' = linked_1_low()), data.frame('high' = linked_1_high()), data.frame('low' = linked_2_low()), data.frame('high' = linked_2_high()))
      write_xlsx(setNames(file_content, c('Foods', 'Food groups', 'Nutrients', 'Linked 1 - low', 'Linked 1 - high', 'Linked 2 - low', 'Linked 2 - high')), file)
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
  
  observe({
    if(input$reset_food_input == 0){
      runjs("
            var click = 0;
            Shiny.onInputChange('reset_food_button', click)
            var reset_food_input = document.getElementById('reset_food_input')
            reset_food_input.onclick = function() {click += 1; Shiny.onInputChange('reset_food_button', click)};
            ") 
    }
  })
  
  observe({
    if(is.null(input$saving_cons_input)){
      runjs("
            var click = 0;
            Shiny.onInputChange('saving_cons_button', click)
            var saving_cons_input = document.getElementById('saving_cons_input')
            saving_cons_input.onclick = function() {click += 1; Shiny.onInputChange('saving_cons_button', click)};
            ")      
    }
  })
  
  observe({
    if(input$proceed_cons_input == 0){
      runjs("
            var click = 0;
            Shiny.onInputChange('proceed_cons_button', click)
            var proceed_cons_input = document.getElementById('proceed_cons_input')
            proceed_cons_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_cons_button', click)};
            ")      
    }
  })
  
  observe({
    if(input$proceed_upload_cons_input == 0){
      runjs("
            var click = 0;
            Shiny.onInputChange('proceed_upload_cons_button', click)
            var proceed_upload_cons_input = document.getElementById('proceed_upload_cons_input')
            proceed_upload_cons_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_upload_cons_button', click)};
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
            
            var click = 0;
            Shiny.onInputChange('reset_food_button', click)
            var reset_food_input = document.getElementById('reset_food_input')
            reset_food_input.onclick = function() {click += 1; Shiny.onInputChange('reset_food_button', click)};
            
            ")
      shinyjs::enable('food_data_input')
      shinyjs::disable('proceed_upload_input')
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
  
  observe({
    if(input$reset_cons_input == 0){
      runjs("
            var click = 0;
            Shiny.onInputChange('reset_cons_button', click)
            var reset_cons_input = document.getElementById('reset_cons_input')
            reset_cons_input.onclick = function() {click += 1; Shiny.onInputChange('reset_cons_button', click)};
            ") 
    }
  })
  
  observeEvent(
    input$reset_cons_input,
    {
      runjs("
            var click = 0;
            Shiny.onInputChange('saving_cons_button', click)
            var saving_cons_input = document.getElementById('saving_cons_input')
            saving_cons_input.onclick = function() {click += 1; Shiny.onInputChange('saving_cons_button', click)};
            
            var click = 0;
            Shiny.onInputChange('proceed_cons_button', click)
            var proceed_cons_input = document.getElementById('proceed_cons_input')
            proceed_cons_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_cons_button', click)};
            
            var click = 0;
            Shiny.onInputChange('proceed_upload_cons_button', click)
            var proceed_upload_cons_input = document.getElementById('proceed_upload_cons_input')
            proceed_upload_cons_input.onclick = function() {click += 1; Shiny.onInputChange('proceed_upload_cons_button', click)};
            
            var click = 0;
            Shiny.onInputChange('reset_cons_button', click)
            var reset_cons_input = document.getElementById('reset_cons_input')
            reset_cons_input.onclick = function() {click += 1; Shiny.onInputChange('reset_cons_button', click)};
            
            ")
      shinyjs::enable('constraints_data_input')
      shinyjs::disable('proceed_upload_cons_input')
      #df2() <- df3() <- df4() <- linked_1_low() <- linked_1_high() <- linked_2_low() <- linked_2_high() <- NULL
      constraint_inputs <- reactiveValues(foods = NULL,
                                          food_groups = NULL,
                                          nutrients = NULL,
                                          linked_1_low = NULL,
                                          linked_1_high = NULL,
                                          linked_2_low = NULL,
                                          linked_2_high = NULL)
      
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
  
  output$constraints_data_model <- downloadHandler(
    filename = 'constraints_data_model.xlsx',
    content = function(file){
      file.copy('www/constraints_data_model.xlsx',file)
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
    #colnames(df_fg) <- c('food_group', 'min_g', 'max_g', 'min_serve', 'max_serve')
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
    #df4 <- reactive(df_n)
    
    df_n2$nutrient <- unlist(lapply(df_n2$nutrient, changeNamesNutrientTable))
    
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
    dfl1 <- df1() %>% filter(food_id %in% linked_low_1_def) %>% select(food_id, food_name, food_group)
    datatable(
      dfl1,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsHighC1Output <- output$linkedFoodsHighB1Output <- output$linkedFoodsHighA1Output <- DT::renderDataTable({
    dfh1 <- df1() %>% filter(food_id %in% linked_high_1_def) %>% select(food_id, food_name, food_group)
    datatable(
      dfh1,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsLowC2Output <- output$linkedFoodsLowB2Output <- output$linkedFoodsLowA2Output <- DT::renderDataTable({
    dfl2 <- df1() %>% filter(food_id %in% linked_low_2_def) %>% select(food_id, food_name, food_group)
    datatable(
      dfl2,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$linkedFoodsHighC2Output <-output$linkedFoodsHighB2Output <-output$linkedFoodsHighA2Output <- DT::renderDataTable({
    dfh2 <- df1() %>% filter(food_id %in% linked_high_2_def) %>% select(food_id, food_name, food_group)
    datatable(
      dfh2,
      colnames = c('ID', 'Name', 'Group'),
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  output$nutrientSelectionBox <- renderUI({
    basic_choices = c('Energy', 'Fat', 'Saturated fat', 'Carbohydrates', 'Sugars', 'Fibre', 'Protein', 'Sodium', 'Fat (%)', 'Saturated fat (%)', 'Carbohydrates (%)', 'Sugars (%)', 'Protein (%)')
    if(('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)', 'Discretionary (%)', 'Takeaway (%)', 'Red meat')
    }  else if(('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)', 'Discretionary (%)', 'Takeaway (%)')
    }  else if(('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)', 'Discretionary (%)', 'Red meat')
    } else if(('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)', 'Discretionary (%)')
    } else if(('Alcohol' %in% unique(df1()$food_group)) && !('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)', 'Takeaway (%)')
    } else if(('Alcohol' %in% unique(df1()$food_group)) && !('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)', 'Red meat')
    } else if(('Alcohol' %in% unique(df1()$food_group)) && !('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Alcohol (%)')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Discretionary (%)')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && !('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Takeaway (%)')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && !('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Red meat')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Discretionary (%)', 'Takeaway (%)', 'Red meat')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && !('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Discretionary (%)', 'Red meat')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && ('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && !any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Discretionary (%)', 'Takeaway (%)')
    } else if(!('Alcohol' %in% unique(df1()$food_group)) && !('Discretionary foods' %in% unique(df1()$food_group)) && ('Takeaway' %in% unique(df1()$food_group)) && any(redmeat_ids %in% unique(df1()$food_id))){
      full_choices <- c(basic_choices, 'Takeaway (%)', 'Red meat')
    } else{
      full_choices <- basic_choices
    }
    column(width = 3,
           tags$h3(span(HTML('Nutrients'), style = 'padding-left:15px')),
           box(
             width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
             checkboxGroupInput(
               "nutrient_columns_input",
               label = NULL,
               choices = full_choices,
               selected = full_choices
             ),
             br(),
             br()
           )
    )
  })
  
  output$linkedFoods1 <- reactive(any(linked_low_1_def %in% df1()$food_id) && any(linked_high_1_def %in% df1()$food_id))
  output$linkedFoods2 <- reactive(any(linked_low_2_def %in% df1()$food_id) && any(linked_high_2_def %in% df1()$food_id))
  output$sizeFoods <- reactive(nrow(df1()))
  output$alcoholSelected <- reactive('Alcohol' %in% unique(df1()$food_group))
  output$discretionarySelected <- reactive('Discretionary foods' %in% unique(df1()$food_group))
  output$takeawaySelected <- reactive('Takeaway' %in% unique(df1()$food_group))
  
  
  
  observe({
    updateSelectizeInput(inputId = 'pair_1_lower_input', choices = df1()$food_name)
  })
  
  observe({
    if(length(input$pair_1_lower_input) > 0){
      if(length(input$pair_1_lower_input) < nrow(df1()) - 1){
        shinyjs::enable('pair_1_lower_input')
      } else{
        shinyjs::disable('pair_1_lower_input')
        showModal(
          modalDialog(
            title = 'Warning!',
            p("You've allocated almost all foods in the lower bracket. At least one food must remain to be inserted into the higher bracket. If you wish to re-start the selection, click the button below.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            div(
              style = "display: inline-block; position:relative; left:calc(42.5%);",
              actionButton(
                inputId = "reset_pair_linked_1",
                label = "Reset",
                style = "color: #fff; background-color: #222222; border-color: #fff;"
              )
            )
          )
        )
      }
      
    }
    
    
  })
  
  observe({
    if(length(input$pair_1_lower_input) >0 && length(input$pair_1_higher_input) >0 && length(input$pair_2_lower_input) >0){
      if((length(input$pair_1_lower_input) + length(input$pair_1_higher_input) + length(input$pair_2_lower_input))< nrow(df1()) - 1){
        shinyjs::enable('pair_2_lower_input')
      } else{
        shinyjs::disable('pair_2_lower_input')
        showModal(
          modalDialog(
            title = 'Warning!',
            p("You've allocated almost all foods in the lower bracket. At least one food must remain to be inserted into the higher bracket. If you wish to re-start the selection, click the button below.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            div(
              style = "display: inline-block; position:relative; left:calc(42.5%);",
              actionButton(
                inputId = "reset_pair_linked_2",
                label = "Reset",
                style = "color: #fff; background-color: #222222; border-color: #fff;"
              )
            )
          )
        )
      }
    }
    
    
  })
  
  observe({
    updateSelectizeInput(inputId = 'pair_1_higher_input', choices = df1()$food_name[!(df1()$food_name %in% input$pair_1_lower_input)])
  })
  
  observe({
    updateSelectizeInput(inputId = 'pair_2_lower_input', choices = df1()$food_name[!((df1()$food_name %in% input$pair_1_lower_input)|(df1()$food_name %in% input$pair_1_higher_input))])
  })
  
  observe({
    updateSelectizeInput(inputId = 'pair_2_higher_input', choices = df1()$food_name[!((df1()$food_name %in% input$pair_1_lower_input)|(df1()$food_name %in% input$pair_1_higher_input)|(df1()$food_name %in% input$pair_2_lower_input))])
  })
  
  observeEvent(input$reset_pair_linked_1, {
    shinyjs::reset("pair_1_lower_input")
    shinyjs::enable('pair_1_lower_input')
    removeModal()
  })
  
  observeEvent(input$reset_pair_linked_2, {
    shinyjs::reset("pair_2_lower_input")
    shinyjs::enable('pair_2_lower_input')
    removeModal()
  })
  
  constraint_inputs <- reactiveValues(foods = NULL,
                                      food_groups = NULL,
                                      nutrients = NULL,
                                      linked_1_low = NULL,
                                      linked_1_high = NULL,
                                      linked_2_low = NULL,
                                      linked_2_high = NULL)
  
  observeEvent(
    input$constraints_data_input,
    {x1c <- x2c <- x3c <-  x4c <- x5c <- x6c <- x7c <- x8c <- x9c <- x10c <- x11c <- x12c <- x13c <- x14c <- x15c <- x16c <- x17c <- x18c <- 0
    df_foods_input <- verifyTabFile(input$constraints_data_input$datapath, 'food_constraints', 'constraints_data_input')
    df_food_groups_input <- verifyTabFile(input$constraints_data_input$datapath, 'food_group_constraints', 'constraints_data_input')
    df_nutrients_input <- verifyTabFile(input$constraints_data_input$datapath, 'nutrient_targets', 'constraints_data_input')
    
    x1c <- verifyColumnNames(df_foods_input, model_foods_cons_names, 'food_constraints', 'constraints_data_input', c('food_group', 'food_name', 'food_id', 'size', 'min', 'max'))
    x2c <- verifyColumnNames(df_food_groups_input, model_food_groups_cons_names, 'food_group_constraints', 'constraints_data_input', c('food_group', 'min_g',	'max_g',	'min_serve',	'max_serve'))
    x3c <- verifyColumnNames(df_nutrients_input, model_nutrients_cons_names, 'nutrient_targets', 'constraints_data_input')
    
    if('food_id' %in% names(df1()) && 'food_id' %in% names(df_foods_input)){
      if(!identical(sort(unique(df1()$food_id)),sort(unique(df_foods_input$food_id)))){
        showModal(
          modalDialog(
            title = 'Warning!',
            p("Sheet ", strong('food_constraints')," has distinct food IDs from food data. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            
          )
        )
        
        shinyjs::reset('constraints_data_input')
      } else x4c <- 1
    }
    if('food_group' %in% names(df1()) && 'food_group' %in% names(df_foods_input)){
      if(!identical(sort(unique(df1()$food_group)),sort(unique(df_foods_input$food_group)))){
        showModal(
          modalDialog(
            title = 'Warning!',
            p("Sheet ", strong('food_constraints')," has distinct food groups from food data. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            
          )
        )
        
        shinyjs::reset('constraints_data_input')
      } else x5c <- 1
    }
    if('food_group' %in% names(df_food_groups_input) && 'food_group' %in% names(df_foods_input)){
      if(!identical(sort(unique(df_food_groups_input$food_group)), sort(unique(df_foods_input$food_group)))){
        showModal(
          modalDialog(
            title = 'Warning!',
            p("Sheets ", strong('food_constraints')," and ", strong('food_group_constraints'), " have mismatched food groups. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            
          )
        )
        
        shinyjs::reset('constraints_data_input')
      }  else x6c <- 1
    }
    x7c <- nonNumericCheck(df_foods_input, c('size', 'min', 'max'), 'food_constraints', 'constraints_data_input')
    x8c <- nonNumericCheck(df_food_groups_input, c('min_g','max_g','min_serve','max_serve'), 'food_group_constraints', 'constraints_data_input')
    x9c <- nutrientValueCheck(df_nutrients_input, 'nutrient_targets', nutrient_pairs, 'constraints_data_input')
    
    for(i in 1:length(nutrient_pairs)){
      if(((nutrient_pairs[[i]][[1]] %in% names(df_nutrients_input)) && !(nutrient_pairs[[i]][[2]] %in% names(df_nutrients_input)))||(!(nutrient_pairs[[i]][[1]] %in% names(df_nutrients_input)) && (nutrient_pairs[[i]][[2]] %in% names(df_nutrients_input)))){
        showModal(
          modalDialog(
            title = 'Warning!',
            p("There are mismatched nutrient constraints pairs. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            
          )
        )
        
        shinyjs::reset('constraints_data_input')
        break
      } else{
        if(i == length(nutrient_pairs)) x10c <- 1
      }
    }
    
    for(i in 1:length(np)){
      if((np[[i]][[1]] %in% names(df_nutrients_input)) && !(nutrient_colnames[i] %in% names(df1()))){
        showModal(
          modalDialog(
            title = 'Warning!',
            p("There are nutrient constraints that are absent in food data. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
            
          )
        )
        
        shinyjs::reset('constraints_data_input')
        break
      } else{
        if(i == length(np)) x11c <- 1
      }
    }
    if(any(grepl('perc', names(df_nutrients_input))) && !('energy_kj_g' %in% names(df1()))){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("Energy column is absent from food data and is mandatory to calculate percentage restrictions. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset('constraints_data_input')
    }  else x12c <- 1
    
    for(i in 1:length(nperc)){
      if(nperc[[i]][[1]] %in% names(df_nutrients_input)){
        if(!(n_b[i] %in% names(df1()))){
          showModal(
            modalDialog(
              title = 'Warning!',
              p("A percentage constraint is adopted but its base nutrient is absent from food data. Please check your data and try again.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
              
            )
          )
          
          shinyjs::reset('constraints_data_input')
          break
        }
        if(df_nutrients_input[nperc[[i]][[1]]]<0||df_nutrients_input[nperc[[i]][[2]]]>100){
          showModal(
            modalDialog(
              title = 'Warning!',
              p("Check the percentage constraints. There are minimum values lower than 0 or maximum higher than 100.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
              
            )
          )
          
          shinyjs::reset('constraints_data_input')
          break
        }  else{
          if(i == length(nperc)) x13c <- 1
        }
      }
      
    }
    
    x14c <- verifySpecialGroups('alcohol_perc_min', 'Alcohol', df_nutrients_input, df1()$food_group, 'constraints_data_input')
    x15c <- verifySpecialGroups('discretionary_perc_min', 'Discretionary foods', df_nutrients_input, df1()$food_group, 'constraints_data_input')
    x16c <- verifySpecialGroups('takeaway_perc_min', 'Takeaway', df_nutrients_input, df1()$food_group, 'constraints_data_input')
    constraint_inputs$foods <- df_foods_input
    constraint_inputs$food_groups <- df_food_groups_input
    constraint_inputs$nutrients <- df_nutrients_input
    
    sheets <- excel_sheets(input$constraints_data_input$datapath)
    
    if('linked_foods_pair_1' %in% sheets){
      lk1 <- verifyLinkedSingleTab(input$constraints_data_input$datapath, 'linked_foods_pair_1', model_linked_names, 'constraints_data_input', df1()$food_id)
      lk1_low <- lk1[['low']]
      lk1_high <- lk1[['high']]
      
      constraint_inputs$linked_1_low <- lk1_low
      constraint_inputs$linked_1_high <- lk1_high
      
      if(!is.null(constraint_inputs$linked_1_low) && !is.null(constraint_inputs$linked_1_high)){
        x17c <- 1
      }
      if('linked_foods_pair_2' %in% sheets){
        lk2 <- verifyLinkedSingleTab(input$constraints_data_input$datapath, 'linked_foods_pair_2', model_linked_names, 'constraints_data_input', df1()$food_id)
        lk2_low <- lk2[['low']]
        lk2_high <- lk2[['high']]
        
        if(!is.null(lk1_low) && !is.null(lk1_high) && !is.null(lk2_low) && !is.null(lk2_high)){
          verifyGeneralIntersect(list(lk1_low, lk1_high, lk2_low, lk2_high), 'constraints_data_input')
          
          constraint_inputs$linked_2_low <- lk2_low
          constraint_inputs$linked_2_high <- lk2_high
          
          if(!is.null(constraint_inputs$linked_2_low) && !is.null(constraint_inputs$linked_2_high)){
            x18c <- 1
          }
        }
        
        
        
      } 
    } else if('linked_foods_pair_2' %in% sheets){
      showModal(
        modalDialog(
          title = 'Warning!',
          p("Please insert a ", strong('linked_foods_pair_1'), " tab or rename ", strong('linked_foods_pair_2'), ".", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
          
        )
      )
      
      shinyjs::reset('constraints_data_input')
    }
    
    if('linked_foods_pair_1' %in% sheets && 'linked_foods_pair_2' %in% sheets){
      if(x1c + x2c + x3c +  x4c + x5c + x6c + x7c + x8c + x9c + x10c + x11c + x12c + x13c + x14c + x15c + x16c + x17c + x18c == 18){
        shinyjs::enable('proceed_upload_cons_input')
        shinyjs::disable('constraints_data_input')
      }
    } else if('linked_foods_pair_1' %in% sheets && !('linked_foods_pair_2' %in% sheets)){
      if(x1c + x2c + x3c +  x4c + x5c + x6c + x7c + x8c + x9c + x10c + x11c + x12c + x13c + x14c + x15c + x16c + x17c == 17){
        shinyjs::enable('proceed_upload_cons_input')
        shinyjs::disable('constraints_data_input')
      }
    } else{
      if(x1c + x2c + x3c +  x4c + x5c + x6c + x7c + x8c + x9c + x10c + x11c + x12c + x13c + x14c + x15c + x16c == 16){
        shinyjs::enable('proceed_upload_cons_input')
        shinyjs::disable('constraints_data_input')
      }
    }
    
    
    }
  )
  
  volumes = getVolumes()()
  shinyDirChoose(input, 'folder_input', roots=volumes, filetypes = c('', 'txt', 'csv', 'xlsx'))
  file_path <- reactive(input$folder_input)
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {input$folder_input},
               handlerExpr = {req(is.list(input$folder_input))
                 shinyjs::enable('run_input')
               }
  )
  path_csv <- reactiveValues(dir_path = NULL)
  observeEvent(input$run_input,
               {if(!is.null(input$run_input)){
                 dir_name <- paste0('results_', format(Sys.time(), "%Y%m%d%H%M%S"))
                 path_dir <- paste0(parseDirPath(volumes, file_path()), '/', dir_name)
                 path_csv$dir_path <- path_dir
                 dir.create(path_csv$dir_path)
               }
                 shinyjs::enable('stop_input')
                 shinyjs::disable('reset_food_input')
                 shinyjs::disable('reset_cons_input')
                 shinyjs::disable('run_input')
                 shinyjs::disable('folder_input')
                 shinyjs::disable('pick_column')
                 shinyjs::disable('iteration_input')
                 shinyjs::disable('difference_input')
                 output$log <- renderText({a$logOutput})
               })
  
  observeEvent(input$stop_input,
               { shinyjs::disable('stop_input')
                 shinyjs::enable('reset_food_input')
                 shinyjs::enable('reset_cons_input')
                 shinyjs::reset('folder_input')
                 shinyjs::enable('folder_input')
                 shinyjs::reset('pick_column')
                 shinyjs::enable('pick_column')
                 shinyjs::reset('iteration_input')
                 shinyjs::enable('iteration_input')
                 shinyjs::reset('difference_input')
                 shinyjs::enable('difference_input')
                 a$logOutput <- NULL
                 output$log <- renderText({a$logOutput})
                 report$results$path_dir <- NULL
                 report$results$iterations <- NULL
                 report$results$meals_created <- NULL
                 report$results$last_meal <- NULL
                 report$results$iterations_constraints <- NULL
                 report$results$iterations_fg <- NULL
                 report$results$nutrients_diff <- NULL
                 report$results$nutrient_targets_wk <- NULL
                 report$results$food_groups_wk <- NULL
                 rv$runLoop <- FALSE}
  )
  
  output$pickerColumn <- renderUI({
    if(('price' %in% names(df1()))||('CF_gCO2eq' %in% names(df1()))||('WF_l' %in% names(df1()))||('EF_g_m2' %in% names(df1()))){
      if(('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Carbon footprint', 'Water footprint', 'Ecological footprint')
      } else if(('price' %in% names(df1()))&&!('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Water footprint', 'Ecological footprint')
      } else if(('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&!('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Carbon footprint', 'Ecological footprint')
      } else if(('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Carbon footprint', 'Water footprint')
      } else if(('price' %in% names(df1()))&&!('CF_gCO2eq' %in% names(df1()))&&!('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Ecological footprint')
      } else if(('price' %in% names(df1()))&&!('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Water footprint')
      } else if(('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&!('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Price', 'Carbon footprint')
      } else if(('price' %in% names(df1()))&&!('CF_gCO2eq' %in% names(df1()))&&!('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- 'Price'
      } else if(!('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&!('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- 'Carbon footprint'
      } else if(!('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Carbon footprint','Water footprint', 'Ecological footprint')
      } else if(!('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&!('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Carbon footprint', 'Ecological footprint')
      } else if(!('price' %in% names(df1()))&&('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Carbon footprint', 'Water footprint')
      } else if(!('price' %in% names(df1()))&&!('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&('EF_g_m2' %in% names(df1()))){
        full_choices <- c('Water footprint', 'Ecological footprint')
      }  else if(!('price' %in% names(df1()))&&!('CF_gCO2eq' %in% names(df1()))&&('WF_l' %in% names(df1()))&&!('EF_g_m2' %in% names(df1()))){
        full_choices <- 'Water footprint'
      } else {
        full_choices <- 'Ecological footprint'
      }
      column(width = 12,
             tags$h3(span(HTML('Columns'), style = 'padding-left:15px')),
             box(
               width = 12, solidHeader = FALSE, status = 'warning', style = "border-radius: 5px; background-color: #f2f0eb",
               p("Choose which values will be evaluated by the simulation.", style ="text-align: justify;", style = "color: black;", style = "font-size:18px;"),
               #checkboxGroupInput('columns_mc_input', label = '', choices = full_choices, selected = full_choices)
               pickerInput(inputId = 'pick_column', 
                           label = '', 
                           choices = full_choices,
                           selected = full_choices,
                           options = list(`actions-box` = TRUE),multiple = T)
               
             )
             
      )
      
      
    }
  })
  
  df5 <- reactive({
    df_foods <- df1()
    df_cons <- df2()
    
    df <- df_foods %>% left_join(df_cons[,c('food_id', 'size', 'min', 'max')], by = 'food_id')
    
    if('Alcohol' %in% df$food_group){
      df <- random_plan(df, 'food_group', 'Alcohol')
    }
    
    if('Discretionary foods' %in% df_foods$food_group){
      df <- random_plan(df, 'food_group', 'Discretionary foods')
    }
    
    if('Takeaway' %in% df_foods$food_group){
      df <- random_plan(df, 'food_group', 'Takeaway')
    }
    
    df$intake <- double(nrow(df))
    for(i in 1:nrow(df)){
      
      ifelse(unlist(df[i,'min'])<=unlist(df[i,'max']),
             {
               serve_range <- seq(unlist(df[i,'min']), unlist(df[i,'max']), unlist(df[i,'size'])*input$difference_input)
               df$intake[i] <- sample_safe(serve_range) 
             },
      )
    }
    df$serves <- df$intake/df$size
    df
  })
  
  
  df6 <- reactive({
    df_prov <- df5()
    all_names <- names(df_prov)
    all_names <- all_names[!all_names %in% c('price', 'CF_gCO2eq', 'WF_l', 'EF_g_m2')]
    list_names <- c()
    if(length(input$pick_column) > 0){
      for(i in 1:length(input$pick_column)){
        switch(input$pick_column[i],
               'Price' = {list_names <- append(list_names, 'price')},
               'Carbon footprint' = {list_names <- append(list_names, 'CF_gCO2eq')},
               'Water footprint' = {list_names <- append(list_names, 'WF_l')},
               'Ecological footprint' = {list_names <- append(list_names, 'EF_g_m2')}
        )
      }
    }
    final_names <- append(all_names, list_names)
    df6 <- df_prov %>% 
      select(all_of(final_names))
    
    df6 <- priceEmissionData(calculateNutrientsRandomMeal(df6, nutrient_colnames), emission_cols)
    
    df6
    
    
  })
  
  
  #observeEvent(ignoreNULL = TRUE,
  #             eventExpr = {input$columns_mc_input},
  #             handlerExpr = {req(is.list(input$columns_mc_input))
  #               cols = c()
  #              for(i in 1:length(input$columns_mc_input)){
  #                switch(input$columns_mc_input[i],
  #                       'Price' = {cols <- append(cols, 'price')},
  #                       'Carbon footprint' = {cols <- append(col)}
  #                       )
  #              }
  #             }
  #             
  #             )
  
  output$randomMeal <- DT::renderDataTable({
    df_prov <- df6()
    #df_prov <- priceEmissionData(calculateNutrientsRandomMeal(df_prov, nutrient_colnames), emission_cols)
    colnames_df <- names(df_prov)
    names_df <- c()
    for(i in 1:length(colnames_df)){
      switch(colnames_df[i],
             'food_group' = {names_df <- append(names_df, 'Food group')},
             'food_name' = {names_df <- append(names_df, 'Name')},
             'food_id' = {names_df <- append(names_df, 'ID')},
             'CF_gCO2eq' = {names_df <- append(names_df, 'CF/CO2 (g)')},
             'WF_l' = {names_df <- append(names_df, 'WF/L')},
             'EF_g_m2' = {names_df <- append(names_df, 'EF/gm2')},
             'energy_kj_g' = {names_df <- append(names_df, 'Energy (kj/g)')},
             'fat_g' = {names_df <- append(names_df, 'Fat (g)')},
             'sat_fat_g' = {names_df <- append(names_df, 'Saturated fat (g)')},
             'CHO_g' = {names_df <- append(names_df, 'Carbohydrates (g)')},
             'sugars_g' = {names_df <- append(names_df, 'Sugars (g)')},
             'fibre_g' = {names_df <- append(names_df, 'Fibre (g)')},
             'protein_g' = {names_df <- append(names_df, 'Protein (g)')},
             'sodium_mg' = {names_df <- append(names_df, 'Sodium (mg)')},
             'price' = {names_df <- append(names_df, 'Price ($/100 g)')},
             'size' = {names_df <- append(names_df, 'Serve size (g)')},
             'min' = {names_df <- append(names_df, 'Minimum intake (g)')},
             'max' = {names_df <- append(names_df, 'Maximum intake (g)')},
             'intake' = {names_df <- append(names_df, 'Intake (g)')},
             'serves' = {names_df <- append(names_df, 'Serves')}
      )
    }
    
    datatable(
      df_prov,
      colnames = names_df,
      selection = 'none',
      rownames = FALSE,
      width = '80%'
    )
  })
  
  
  observeEvent(df4(),
               {
                 if(nrow(df2())>0 && nrow(df3())>0 && nrow(df4())>0){
                   shinyjs::enable('saving_cons_input')
                   shinyjs::enable('proceed_cons_input')
                 }
                 
               }
               
  )
  
  a <- reactiveValues(
    logOutput = "Starting Monte Carlo simulation..."
  )
  
  test1 <- eventReactive(input$run_input, as.numeric(input$iteration_input))
  rv <- reactiveValues(n = 0,
                       meal_plan = NULL,
                       nutrient_targets_wk = NULL,
                       food_groups_wk = NULL,
                       min_serve_size_difference = NULL,
                       linked_high_1 = NULL,
                       linked_low_1 = NULL,
                       linked_high_2 = NULL,
                       linked_low_2 = NULL,
                       hash_list = NULL,
                       nutrients_plan = NULL,
                       nutrients_diff = NULL,
                       serves_plan = NULL,
                       serves_diff = NULL,
                       linked_sum_1 = NULL,
                       linked_sum_2 = NULL,
                       iterations_constraints = NULL,
                       iterations_fg = NULL,
                       iterations_lk = NULL,
                       runLoop = FALSE)
  
  observeEvent(input$run_input, {
    rv$n <- 1
    rv$meal_plan <-data.frame(df6())
    rv$nutrient_targets_wk <- data.frame(df4())
    rv$food_groups_wk <- data.frame(df3())
    rv$min_serve_size_difference <- input$difference_input
    rv$linked_high_1 <- linked_1_high()
    rv$linked_low_1 <- linked_1_low()
    rv$linked_high_2 <- linked_2_high()
    rv$linked_low_2 <- linked_2_low()
    rv$hash_list <- list()
    rv$nutrients_plan <- calculateNutrientsTotal(data.frame(df6()), data.frame(df4()))
    rv$nutrients_diff <- calculateNutrientsDiff(calculateNutrientsTotal(data.frame(df6()), data.frame(df4())), data.frame(df4()))
    rv$serves_plan <- calculateServes(data.frame(df6()))
    rv$serves_diff <- calculateServesDiff(data.frame(df3()), calculateServes(data.frame(df6())))
    rv$linked_sum_1 <- calculateLinkedSum(df = data.frame(df6()), low = linked_1_low(), high = linked_1_high())
    rv$linked_sum_2 <- calculateLinkedSum(df = data.frame(df6()), low = linked_2_low(), high = linked_2_high())
    rv$iterations_constraints <- data.frame(nutrient = calculateNutrientsDiff(calculateNutrientsTotal(data.frame(df6()), data.frame(df4())), data.frame(df4())) %>% pull(nutrient),
                                            high = integer(nrow(calculateNutrientsDiff(calculateNutrientsTotal(data.frame(df6()), data.frame(df4())), data.frame(df4())))),
                                            low = integer(nrow(calculateNutrientsDiff(calculateNutrientsTotal(data.frame(df6()), data.frame(df4())), data.frame(df4())))))
    
    rv$iterations_fg <- data.frame(food_group = calculateServesDiff(data.frame(df3()), calculateServes(data.frame(df6()))) %>% pull(food_group),
                                   high = integer(nrow(calculateServesDiff(data.frame(df3()), calculateServes(data.frame(df6()))))),
                                   low = integer(nrow(calculateServesDiff(data.frame(df3()), calculateServes(data.frame(df6()))))))
    
    rv$iterations_lk <- data.frame(link = c('pair_1','pair_2'),
                                   high = integer(2),
                                   low = integer(2))
    rv$runLoop <- TRUE
  })
  
  #output$log <- renderText({a$logOutput})
  
  updateLog <- function(text){
    a$logOutput <- paste(a$logOutput, text, sep = "\n")
  }
  
  report <-reactiveValues(results = list(path_dir = NULL,
                                         iterations = NULL,
                                         meals_created = NULL,
                                         last_meal = NULL,
                                         iterations_constraints = NULL,
                                         iterations_fg = NULL,
                                         iterations_lk = NULL,
                                         nutrients_diff = NULL,
                                         serves_diff = NULL,
                                         nutrient_targets_wk = NULL,
                                         food_groups_wk = NULL))
  
  observe({
    if(rv$runLoop) invalidateLater(0, session) else return();
    meal_plan <- rv$meal_plan
    nutrient_targets_wk <- rv$nutrient_targets_wk
    food_groups_wk <- rv$food_groups_wk
    min_serve_size_difference <- rv$min_serve_size_difference
    linked_high_1 <- rv$linked_high_1
    linked_low_1 <- rv$linked_low_1
    linked_high_2 <- rv$linked_high_2
    linked_low_2 <- rv$linked_low_2
    hash_list <- rv$hash_list
    nutrients_plan <- rv$nutrients_plan
    nutrients_diff <- rv$nutrients_diff
    serves_plan <- rv$serves_plan
    serves_diff <- rv$serves_diff
    off_measures <- NULL
    off_food_groups <- NULL
    off_linked_foods_low <- NULL
    off_linked_foods_high <- NULL
    target_measure <- NULL
    target_fg <- NULL
    serve_range <- NULL
    food <- NULL
    
    linked_sum_1 <- rv$linked_sum_1
    linked_sum_2 <- rv$linked_sum_2
    iterations_constraints = rv$iterations_constraints
    
    iterations_fg <- rv$iterations_fg
    
    iterations_lk <- rv$iterations_lk
    isolate({
      
      if (rv$n > test1()) {
        report$results$path_dir <- file.path(path_csv$dir_path)
        report$results$iterations <- as.numeric(input$iteration_input)
        report$results$meals_created <- length(hash_list)
        report$results$last_meal <- meal_plan
        report$results$iterations_constraints <- iterations_constraints
        report$results$iterations_fg <- iterations_fg
        report$results$iterations_lk <- iterations_lk
        report$results$nutrients_diff <- nutrients_diff
        report$results$nutrient_targets_wk <- nutrient_targets_wk
        report$results$food_groups_wk <- food_groups_wk
        rv$runLoop <- FALSE
        if(as.numeric(report$results$meals_created)>0){
          showModal(
            modalDialog(
              title = 'Diets formed!',
              p("Please select the confidence interval in which the results will be estimated.",style ="text-align: justify;", style = "color: black;", style = "font-size:18px;", style = 'padding-left:15px;', style = 'padding-right:15px;'),
              fluidRow(column(width = 2),
                       column(width = 9,
                              tags$div(class = "slider-custom",
                                       sliderInput(inputId = 'confidence_interval_input', label = 'Confidence interval (%)',
                                                   min = 90, max = 99, value = 95, step = 1))),
                       column(width = 1))
              
              
            )
          )
        }
        
        
        return()
      } else {
        if(isTRUE(checkZeroDiff(nutrients_diff))){
          if(isTRUE(checkZeroDiff(serves_diff))){
            if((is.null(linked_low_1) && is.null(linked_high_1) && is.null(linked_low_2) && is.null(linked_high_2))||(!is.null(linked_low_1) && !is.null(linked_high_1) && is.null(linked_low_2) && is.null(linked_high_2) && linked_sum_1 >=0)||(!is.null(linked_low_1) && !is.null(linked_high_1) && !is.null(linked_low_2) && !is.null(linked_high_2) && linked_sum_1 >=0 && linked_sum_2 >=0)){
              hash_diet <- hash(meal_plan)
              if(!(hash_diet %in% hash_list)){
                hash_list[[length(hash_list)+1]] <- hash_diet
                text <- paste0('Iteration: ', rv$n,'. Hit! Unique diet formed!')
                
                meal_plan <- priceEmissionData(calculateNutrientsRandomMeal(meal_plan, nutrient_colnames), emission_cols)
                file_name <- paste0('meal_plan_',rv$n,'.csv')
                write.csv(meal_plan, file.path(path_csv$dir_path, file_name), row.names=FALSE)
              } else{
                text <- paste0('Iteration: ', rv$n,'. Hit! Diet already logged in!')
              }
              food = sample_safe(meal_plan$food_id)
              serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
            }else{
              if(!is.null(linked_low_1) && !is.null(linked_high_1) && is.null(linked_low_2) && is.null(linked_high_2) && linked_sum_1 < 0){
                off_linked_foods_low <- linked_low_1
                off_linked_foods_high <- linked_high_1
              } else{
                if(linked_sum_1 < 0 && linked_sum_2 < 0){
                  off_linked_foods_low <- c(linked_low_1, linked_low_2)
                  off_linked_foods_high <- c(linked_high_1, linked_high_2)
                } else if(linked_sum_1 < 0 && linked_sum_2 >= 0){
                  off_linked_foods_low <- linked_low_1
                  off_linked_foods_high <- linked_high_1
                } else if(linked_sum_1 >= 0 && linked_sum_2 < 0){
                  off_linked_foods_low <- linked_low_2
                  off_linked_foods_high <- linked_high_2
                }
              }
              if(length(off_linked_foods_low) > 0 && length(off_linked_foods_high) > 0){
                direction_choices <- c('<', '>')
                direction <- sample_safe(direction_choices)
                if(direction == '<'){
                  tmp <- meal_plan$food_id[meal_plan$food_id %in% off_linked_foods_low]
                  food <- sample_safe(tmp)
                  if(food %in% linked_low_1){
                    fl <- 'pair_1'
                  } else{
                    fl <- 'pair_2'
                  }
                  serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$intake[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
                  text <- paste0(c(paste0('Iteration: ', rv$n,'. Food link ', fl, ' is off. ', meal_plan$food_name[meal_plan$food_id == food], ' affects it at lower half. Current intake is ',meal_plan$intake[meal_plan$food_id == food], ' and it must be between ', meal_plan$min[meal_plan$food_id == food], ' and ', meal_plan$max[meal_plan$food_id == food],'. Options: '),serve_range), collapse = " ")
                  iterations_lk$low[iterations_lk$link == fl] <- iterations_lk$low[iterations_lk$link == fl] + 1
                } else{
                  tmp <- meal_plan$food_id[meal_plan$food_id %in% off_linked_foods_high]
                  food <- sample_safe(tmp)
                  if(food %in% linked_high_1){
                    fl <- 'pair_1'
                  } else{
                    fl <- 'pair_2'
                  }
                  serve_range <- sort(seq(meal_plan$intake[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
                  text <- paste0(c(paste0('Iteration: ', rv$n,'. Food link ', fl, ' is off. ', meal_plan$food_name[meal_plan$food_id == food], ' affects it at upper half. Current intake is ',meal_plan$intake[meal_plan$food_id == food], ' and it must be between ', meal_plan$min[meal_plan$food_id == food], ' and ', meal_plan$max[meal_plan$food_id == food],'. Options: '),serve_range), collapse = " ")
                  iterations_lk$high[iterations_lk$link == fl] <- iterations_lk$high[iterations_lk$link == fl] + 1
                }
              }
              
            }
          }else{
            off_food_groups <- serves_diff[serves_diff$value != 0,]
            target_fg <- sample_safe(off_food_groups$food_group)
            
            foods_impacted <- meal_plan$food_id[meal_plan$food_group == target_fg]
            if(length(foods_impacted) == 0){
              text <- (paste('Iteration: ', rv$n,'. No food impact group: ',target_fg))
              rv$n <- rv$n + 1
              rv$meal_plan <- meal_plan
              rv$hash_list <- hash_list
              rv$nutrients_plan <- nutrients_plan
              rv$nutrients_diff <- nutrients_diff
              rv$serves_plan <- serves_plan
              rv$serves_diff <- serves_diff
              rv$linked_sum_1 -> linked_sum_1
              rv$linked_sum_2 <- linked_sum_2
              rv$iterations_constraints <- iterations_constraints
              rv$iterations_fg <- iterations_fg
              rv$iterations_lk <- iterations_lk
              updateLog(text)
            }
            food <- sample_safe(foods_impacted)
            fg <- meal_plan$food_group[meal_plan$food_id == food]
            
            if(off_food_groups$value[off_food_groups$food_group == target_fg] > 0){
              text_1 <- paste0('Iteration: ', rv$n,'. Food group ', fg, ' has too many serves. Current: ', serves_plan$value[serves_plan$food_group == target_fg],'. Max: ', food_groups_wk$max[food_groups_wk$food_group == target_fg])
              serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$intake[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
              iterations_fg$high[iterations_fg$food_group == target_fg] <- iterations_fg$high[iterations_fg$food_group == target_fg] + 1
            } else{
              text_1 <- paste0('Iteration: ', rv$n,'. Food group ', fg, ' has too few serves. Current: ', serves_plan$value[serves_plan$food_group == target_fg],'. Min: ', food_groups_wk$min[food_groups_wk$food_group == target_fg])
              serve_range <- sort(seq(meal_plan$intake[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
              iterations_fg$low[iterations_fg$food_group == target_fg] <- iterations_fg$low[iterations_fg$food_group == target_fg] + 1
            }
            text <- paste0(c(paste0(text_1,'. ', meal_plan$food_name[meal_plan$food_id == food]," has current intake of ",meal_plan$intake[meal_plan$food_id == food]," and it must be between ",meal_plan$min[meal_plan$food_id == food]," and ",meal_plan$max[meal_plan$food_id == food],". Options: "),serve_range), collapse = " ")
            
            
          }
        }else{
          off_measures <- nutrients_diff[nutrients_diff$value != 0,]
          target_measure <- sample_safe(off_measures$nutrient)
          col <- target_measure
          if(col == 'discretionary_perc'){
            foods_impacted <- meal_plan %>% filter(food_group == 'Discretionary foods') %>% pull(food_id)
          } else if(col == 'alcohol_perc'){
            foods_impacted <- meal_plan %>% filter(food_group == 'Alcohol') %>% pull(food_id)
          } else if(col == 'takeaway_perc'){
            foods_impacted <- meal_plan %>% filter(food_group == 'Takeaway') %>% pull(food_id)
          } else if(col == 'redmeat_g'){
            foods_impacted <- meal_plan %>% filter(food_id %in% redmeat_ids) %>% pull(food_id)
          } else{
            if(grepl('perc', col)){
              col <- unlist(strsplit(col, '_perc',1))
              col <- paste0(col, '_g')
            }
            foods_impacted <- meal_plan %>% filter(UQ(sym(col)) > 0) %>% pull(food_id)
          }
          if(length(foods_impacted) == 0){
            text <- paste('Iteration: ', rv$n,'. No food impact measure: ',target_measure)
            rv$n <- rv$n + 1
            rv$meal_plan <- meal_plan
            rv$hash_list <- hash_list
            rv$nutrients_plan <- nutrients_plan
            rv$nutrients_diff <- nutrients_diff
            rv$serves_plan <- serves_plan
            rv$serves_diff <- serves_diff
            rv$linked_sum_1 -> linked_sum_1
            rv$linked_sum_2 <- linked_sum_2
            rv$iterations_constraints <- iterations_constraints
            rv$iterations_fg <- iterations_fg
            rv$iterations_lk <- iterations_lk
            updateLog(text)
          }
          food <- sample_safe(foods_impacted)
          if(off_measures$value[off_measures$nutrient == target_measure] > 0){
            iterations_constraints$high[iterations_constraints$nutrient == target_measure] <- iterations_constraints$high[iterations_constraints$nutrient == target_measure] + 1
            text_1 <- paste0('Iteration: ', rv$n,'. We are too high on ',target_measure,'. Current: ',nutrients_plan$value[nutrients_plan$nutrient == target_measure],'. Max: ',nutrient_targets_wk$max[nutrient_targets_wk$nutrient == target_measure])
            serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$intake[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
            if(length(serve_range) > 10){
              serve_range <- tail(serve_range, 10)
            }
            
          } else{
            iterations_constraints$low[iterations_constraints$nutrient == target_measure] <- iterations_constraints$high[iterations_constraints$nutrient == target_measure] + 1
            text_1 <- paste0('Iteration: ', rv$n,'. We are too low on ',target_measure,'. Current: ',nutrients_plan$value[nutrients_plan$nutrient == target_measure],'. Min: ',nutrient_targets_wk$min[nutrient_targets_wk$nutrient == target_measure])
            serve_range <- sort(seq(meal_plan$intake[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$size[meal_plan$food_id == food]*min_serve_size_difference))
            if(length(serve_range) > 10){
              serve_range <- head(serve_range, 10)
            }
            
          }
          text <- paste0(paste0(c(paste0(text_1,'. ', meal_plan$food_name[meal_plan$food_id == food]," impacts ", target_measure, " and intake must be between ", meal_plan$min[meal_plan$food_id == food]," and ",meal_plan$max[meal_plan$food_id == food],". Options:"),serve_range), collapse = " "),". Current: ",meal_plan$intake[meal_plan$food_id == food])
        }
        if(!is.null(serve_range)){
          new_intake <- sample_safe(serve_range)
          text <- paste0(text, '. Changing ', meal_plan$food_name[meal_plan$food_id == food],' intake from ',meal_plan$intake[meal_plan$food_id == food],' to ',new_intake)
          meal_plan$intake[meal_plan$food_id == food] <- new_intake
        }
        updateLog(text)
        rv$n <- rv$n + 1
        rv$meal_plan <- meal_plan
        rv$hash_list <- hash_list
        rv$nutrients_plan <- nutrients_plan
        rv$nutrients_diff <- nutrients_diff
        rv$serves_plan <- serves_plan
        rv$serves_diff <- serves_diff
        rv$linked_sum_1 -> linked_sum_1
        rv$linked_sum_2 <- linked_sum_2
        rv$iterations_constraints <- iterations_constraints
        rv$iterations_fg <- iterations_fg
        rv$iterations_lk <- iterations_lk
      }
      
      
    })
    
  })
  
  output$exportButtons <- renderUI({
    if(rv$n >= test1()){
      column(width = 12,
             div(style = "display: inline-block; position:relative; left:calc(38.5%);",
                 actionButton('reset_app_input', label = 'Reset app')),
             
             
             div(style = "display: inline-block; position:relative; left:calc(41.5%);",
                 downloadButton(
                   "report_data",
                   label = "Download report",
                   style = "color: #fff; background-color: #222222; border-color: #fff;"
                 ))
             
      )
    }
  })
  
  
  observeEvent(input$reset_app_input, {session$reload()})
  
  output$report_data <- downloadHandler(
    filename = function() {
      paste0('results_monte_carlo_', format(Sys.time(), "%Y%m%d%H%M"), '.xlsx')
    },
    content = function(file) {
      df_data <- data.frame(
        'Folder path' = report$results$path_dir,
        'Iterations' = report$results$iterations,
        'Meals created' = report$results$meals_created
      )
      
      file_content <- list(
        `General` = df_data,
        `Last meal` = data.frame(report$results$last_meal),
        `Nutrient constraints` = data.frame(report$results$iterations_constraints),
        `Group constraints` = data.frame(report$results$iterations_fg),
        `Linked foods` = data.frame(report$results$iterations_lk),
        `Nutrients diff` = data.frame(report$results$nutrients_diff),
        `Groups diff` = data.frame(report$results$nutrient_targets_wk),
        `Nutrient targets` = data.frame(report$results$nutrient_targets_wk),
        `Group targets` = data.frame(report$results$food_groups_wk)
      )
      
      if (report$results$meals_created > 0) {
        files <- list.files(path = report$results$path_dir, pattern = "meal_plan_", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        df <- data.frame(item = names(df4()))
        confidence_interval <- 1 - ((input$confidence_interval_input/100)/2)
        for(file in files){
          meal_df <- read.csv(file.path(report$results$path_dir, file))
          col <- as.character(strsplit(file,'.csv')[1])
          df[,col] <- double(nrow(df))
          
          for(i in 1:nrow(df)){
            switch(df$item[i],
                   'energy_kj_g' = {df[i, col] <- sum(meal_df$energy_kj_g, na.rm = TRUE)},
                   'fat_g' = {df[i, col] <- sum(meal_df$fat_g, na.rm = TRUE)},
                   'sat_fat_g' = {df[i, col] <- sum(meal_df$sat_fat_g, na.rm = TRUE)},
                   'CHO_g' = {df[i, col] <- sum(meal_df$CHO_g, na.rm = TRUE)},
                   'sugars_g' = {df[i, col] <- sum(meal_df$sugars_g, na.rm = TRUE)},
                   'protein_g' = {df[i, col] <- sum(meal_df$protein_g, na.rm = TRUE)},
                   'fat_perc' = {df[i, col] <- (sum(meal_df$fat_g, na.rm = TRUE)*f1)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
                   'sat_fat_perc' = {df[i, col] <- (sum(meal_df$sat_fat_g, na.rm = TRUE)*f1)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
                   'CHO_perc' = {df[i, col] <- (sum(meal_df$CHO_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
                   'sugars_perc' = {df[i, col] <- (sum(meal_df$sugars_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
                   'fibre_g' = {df[i, col] <- sum(meal_df$fibre_g, na.rm = TRUE)},
                   'protein_perc' = {df[i, col] <- (sum(meal_df$protein_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
                   'red_meat_g' = {df[i, col] <- sum(meal_df$intake[meal_df$food_group == 'red meat'], na.rm = TRUE)},
                   'sodium_mg' = {df[i, col] <- sum(meal_df$sodium_mg, na.rm = TRUE)},
                   'fruit_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Fruit']), na.rm = TRUE)},
                   'vegetable_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Vegetables']), na.rm = TRUE)},
                   'grains_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Grains']), na.rm = TRUE)},
                   'dairy_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Dairy/alternatives']), na.rm = TRUE)},
                   'protein_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Protein foods: Meat, poultry, seafood, eggs, legumes, nuts, seeds']), na.rm = TRUE)},
                   'fats_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Fats & oils']), na.rm = TRUE)},
                   'sauces_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Sauces, dressings, spreads, sugars']), na.rm = TRUE)},
                   'beverages_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Beverages']), na.rm = TRUE)},
                   'ssb_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'ssb']), na.rm = TRUE)},
                   'starchy_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Starchy vegetables']), na.rm = TRUE)},
                   'red_meat_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'red meat']), na.rm = TRUE)},
                   'alcohol_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Alcohol']), na.rm = TRUE)},
                   'discretionary_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Discretionary foods']), na.rm = TRUE)},
                   'takeaway_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Takeaway']), na.rm = TRUE)},
                   'fruit_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Fruit'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'vegetable_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Vegetables'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'grains_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Grains'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'dairy_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Dairy/alternatives'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'protein_foods_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Protein foods: Meat, poultry, seafood, eggs, legumes, nuts, seeds'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'fats_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Fats & oils'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'sauces_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Sauces, dressings, spreads, sugars'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'beverages_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Beverages'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'ssb_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'ssb'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'starchy_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Starchy vegetables'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'red_meat_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'red meat'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'alcohol_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Alcohol'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'discretionary_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Discretionary foods'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'takeaway_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Takeaway'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
                   'price' = {df[i, col] <- sum(meal_df$price, na.rm = TRUE)},
                   'CF_gCO2eq' = {df[i, col] <- sum(meal_df$CF_gCO2eq, na.rm = TRUE)},
                   'WF_l' = {df[i, col] <- sum(meal_df$WF_l, na.rm = TRUE)},
                   'EF_g_m2' = {df[i, col] <- sum(meal_df$EF_g_m2, na.rm = TRUE)}
            )
            
          }
        }
        df_results <- data.frame(item = df$item)
        df_results[,c('value', 'margin')] <- double(nrow(df))
        
        for(i in 1:nrow(df)){
          n <- ncol(df) - 1
          df_results$value[i] <- as.numeric(rowMeans(df[i,2:ncol(df)]))
          s <- sd(df[i,2:ncol(df)])
          df_results$margin[i] <- ifelse(n <= 30,
                                         qt(confidence_interval,df=n-1)*s/sqrt(n),
                                         qnorm(confidence_interval)*s/sqrt(n))
          
        }
        file_content[["Results"]] <- df_results
      }
      
      write_xlsx(file_content, file)
    }
  )
  
  outputOptions(output, "alcoholSelected", suspendWhenHidden = FALSE)
  outputOptions(output, "discretionarySelected", suspendWhenHidden = FALSE)
  outputOptions(output, "takeawaySelected", suspendWhenHidden = FALSE)
  outputOptions(output, "linkedFoods1", suspendWhenHidden = FALSE)
  outputOptions(output, "linkedFoods2", suspendWhenHidden = FALSE)
  outputOptions(output, "sizeFoods", suspendWhenHidden = FALSE)
  
  
}

#App creation-------------------------------------------------------------------
shinyApp(ui = ui, server = server)

