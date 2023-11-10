############################################## CALLING LIBRARIES #################################

library(shinyFiles)
library(RColorBrewer)
library(shinyBS)
library(cartography)
library(shinyLP)
library(shinycssloaders)
library(fs)
library(readxl)
library(purrr)
library(sf)
library(crosstalk)
library(stringr)
library(shiny)
library(DT)
library(stringr)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
# library(runslam)
library(tidyr)
library(plyr)
library(dplyr)
#library(Matrix.utils)
library(RColorBrewer)
library(ggplot2)
#library(maptools)
library(rgdal)
library(RColorBrewer)
library(httpuv)
library(reshape2)
library(kableExtra)
library(scales)
library(foreign)
library(xlsx)
library(data.table)
library(rstudioapi)
library(reactable)
library(foreign)
library(leaflet.extras)

##################################################################################################


source('C:\\R_SLAM_2\\shiny\\functions.R')

##### Read control and tazx file to run the program: for choices and display #################

basedir <- "C:/R_SLAM_2/BaseLU"
taztags <- read.csv(str_c(basedir,"/ExcludeFiles/TazTags.csv"))
exComputed <- read.csv(str_c(basedir,"/ExcludeFiles/excludeComputed.csv"))
setwd(basedir)
control<-read.csv('Alt_A/input/control.csv')
control_default <- control
taz <- read.csv('Alt_A/input/TAZx.csv')
taz_default <- taz
rate_of_consumption <- read.csv('Alt_A/input/densities.csv')
rate_of_consumption_default <- rate_of_consumption
taz_shapefile <- st_read('Shapefiles', layer = "taz_to_display")
taz_data <- taz_shapefile
st_geometry(taz_data) <- NULL
taz_data <- left_join(taz_data,taztags[,c(1,9:ncol(taztags))], 
                      by = c("MODEL_TAZ" = "Model_TAZ"))

labels_of_taz <-  c("Developable Acreage","Household Compatibility Score","Retail Compatibility Score",
                    "Non-Retail Compatibility Score",
                    "Household Accessibility Score","Employment Accessibility Score","Developable Acreage",
                    "Population Index","Retail Index","Non-retail Index",
                    "Vertical HH developable acreage beyond the available area",
                    "Vertical Retail developable acreage beyond the available area",
                    "Vertical Non-Retail developable acreage beyond the available area",
                    "Household Vertical Index","Retail Vertical Index" ,
                    "Non-Retail Vertical Index","Exculde from the Model")

final_labels_of_taz <- stringr::str_c(labels_of_taz," (",colnames(taz)[4:length(taz)],")")

######################################################################################################

#################################### Functions to do things #######################################

all_ids <- c("j_name", rep(c("hh", "ret", "non_ret","pop"), times = 2), 
             "model_taz", final_labels_of_taz, "area_type")
all_ids[2:8] <- c(str_c(all_ids[2:5], "_value"), str_c(all_ids[6:8], "_den"))


running_model <- function(base_dir_input, alt_name){
  print(getwd())
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  
  if(alt_name == ""){
    alt_name <- "Alt_A"
  }
  
  runslam(base_dir_input,alt_name)
  message("*** RTC Land Use Model Complete ***\n") 
}
creating_alternative <- function(base_dir_input, alt_name ){
  if(base_dir_input == ""){
    base_dir_input <- "C:/R_SLAM_2/BaseLU"
  }
  
  if(alt_name == ""){
    base_dir_input <- "C:/R_SLAM_2/BaseLU"
  }else {
    dir.create(str_c("C:/R_SLAM_2/BaseLU/", alt_name))
    dir.create(str_c("C:/R_SLAM_2/BaseLU/",alt_name, "/", "Input"))
    dir.create(str_c("C:/R_SLAM_2/BaseLU/", alt_name,"/", "Output"))
  }
  
  #reading files from the parent folder
  parent_files_path <- list.files(paste0(base_dir_input, '/Alt_A/input'), full.names = T)
  child_files_path <- paste0(base_dir_input,"/" ,alt_name, "/input")
  file.copy(parent_files_path, child_files_path )
  
}
changing_ui <- function(file_choice){
  
  creating_ti_taz <- function(){
    # labels_of_taz <-  c("Developed Acreage","Household Compatibility Score","Retail Compatibility Score","Non-Retail Compatibility Score",
    #                     "Household Accessibility Score","Employment Accessibility Score","Developable Acreage",
    #                     "Population Index","Retail Index","Non-retail Index","Vertical value Households in acres","Vertical value Retail in acres","Vertical value Non-Retail in acres",
    #                     "Household Vertical Index","Retail Vertical Index" ,"Non-Retail Vertical Index","Exculde from the Model")
    # final_labels_of_taz <- stringr::str_c(labels_of_taz," (", name_columns[4:length(taz)],")")
    
    inputs_taz <- lapply(final_labels_of_taz, function(x){
      textInput(inputId = x, label = x, placeholder = "Enter value to not use defaults")
    })
    return(inputs_taz)
  }
  
  jurisdiction <- pickerInput("j_name", "Select Jurisdiction", choices = unique(control$Jname), selected = "Richmond",
                              options = list(title = paste0( unique(control$Jname), collapse = ", ")))
  if(file_choice == 1){
    list(
      jurisdiction,
      textInput("pop_value", "Enter Total population", placeholder = "Enter value to not use defaults"),
      textInput("hh_value", "Enter the number of Households", placeholder = "Enter value to not use defaults"),
      textInput("ret_value", "Enter the number of Retail Jobs", placeholder = "Enter value to not use defaults"),
      textInput("non_ret_value", "Enter the number of Non-retail Jobs", placeholder = "Enter value to not use defaults"))
  } else if(file_choice == 2){
    list(
      jurisdiction,
      pickerInput("area_type", "Select Area type", choices = c("Urban" = 1, "Semi-Urban" = 2, "Rural" = 3), 
                  options = list(title =  paste0(c("Urban", "Semi-Urban", "Rural"), collapse = ", "))),
      textInput("hh_den", "Enter the density of HH per acre", placeholder = "Enter value to not use defaults"),
      textInput("ret_den", "Enter the density of jobs per Acre (Retail Employment)", placeholder = "Enter value to not use defaults"),
      textInput("non_ret_den", "Enter the density of jobs per Acre (Non-retail Employment)", placeholder = "Enter value to not use defaults"))
    
  } else  if(file_choice == 3){
    list(
      jurisdiction,
      pickerInput("model_taz", "Select TAZ", choices = c(), options = list(title = "1, 2, 3, 4, 5 ..."), multiple = T),
      creating_ti_taz())
    
    
  }
}
changing_columns <- function(name_df, col_to_change, data_to_replace, row_number, choice = 1){
  if(choice == 1){
    j <- 1
    for( i in col_to_change){ #directly using the name of the column in i 
      print(i)
      name_df[[i]][row_number] <- as.numeric(data_to_replace[j]) 
      #print(x[j])
      j <- j + 1
    }
    return(name_df)
  } else if(choice == 2) {
    for( i in col_to_change){ #directly using the name of the column in i 
      print(i)
      name_df[[i]][row_number] <- as.numeric(data_to_replace[[i]][row_number]) 
    }
    return(name_df)
  }
}

changing_input_file <- function(data_by_juris, file_choice, taz_group_input = ""){
  
  file_choice <- as.numeric(file_choice)
  x <- unlist(strsplit(data_by_juris, split = ",")) # "JNAME", "POP",  "HH","RET_JOBS", "NON_RET_JOBS"
  
  if(file_choice == 1) {
    
    indices <- which(control$Jname == x[1])
    rows_to_change <- indices[1:4]
    jur_name <- x[1]
    
    if(x[2] == "" & x[5] !="" ){
      x[2] <- round(as.numeric(x[5]) / as.numeric(control_default$Ratio[rows_to_change[1]]))
      control[rows_to_change[4], "CNTL_TOT"] <<- as.numeric(x[5]) - as.numeric(control$CNTL_TOT[control$Jname == jur_name][5])
    } else if( x[2] != "" & x[5] !="" ) {
      control[rows_to_change[4], "CNTL_TOT"] <<- as.numeric(x[5]) - as.numeric(control$CNTL_TOT[control$Jname == jur_name][5])
    }
    
    x <- x[-1]
    
    if(x[4] != ""){
      x <- x[-4]
      rows_to_change <- rows_to_change[1:3]
    }
    
    empty_indices <- grepl(pattern = "\\S", x )  #T, F, T
    
    control$CNTL_TOT[ rows_to_change[empty_indices] ] <<- as.numeric(x[empty_indices])
    control$CNTL_TOT[ rows_to_change[!empty_indices] ] <<- as.numeric(control_default$CNTL_TOT[rows_to_change[!empty_indices]])
    return(control)
    
  } else if (file_choice == 3) {
    taz_to_change <- as.numeric(taz_group_input)
    index <- unlist(lapply(taz_to_change, function(x){
      which(taz$MODEL_TAZ == x)
    }))
    empty_indices <- grepl(pattern = "\\S", x ) #### columns to change from user input using it on data by jurisdiction
    columns_to_change <- colnames(taz)[4:length(taz)]
    columns_to_change_from_user_input <- columns_to_change[empty_indices] #### columns to change from user input
    columns_to_change_from_default <- columns_to_change[!empty_indices] #### columns to change from default
    
    taz <<- changing_columns(taz, columns_to_change_from_user_input, x[empty_indices], index)
    taz <<- changing_columns(taz, columns_to_change_from_default, data_to_replace = taz_default,index, choice = 2)
    return(taz)
    as
  } else if (file_choice == 2) {
    x <- unlist(strsplit(data_by_juris, split = ",")) # "JNAME", "HH","RET_JOBS", "NON_RET_JOBS"
    filtered_by_juris <- which(rate_of_consumption$JUR == x[1] & rate_of_consumption$MTP_AT == as.numeric(x[5]))
    x <- x[ -c(1,5)]
    empty_indices <- grepl(pattern = "\\S", x )  #T, F, T
    columns_to_change <-     colnames(rate_of_consumption)[4:6]
    cols_to_change_user_input <- columns_to_change[empty_indices]
    cols_to_change_default <- columns_to_change[!empty_indices]
    
    rate_of_consumption <<- changing_columns(name_df = rate_of_consumption, 
                                             col_to_change =  cols_to_change_user_input, 
                                             data_to_replace = x[empty_indices], row_number = filtered_by_juris)
    rate_of_consumption <<- changing_columns(rate_of_consumption, cols_to_change_default,
                                             row_number = filtered_by_juris, choice = 2,
                                             data_to_replace = rate_of_consumption_default)
    return(rate_of_consumption)
  }
}



#writing_file(input$m_file, input$base_dir, input$alt_name)
writing_file <- function(choice, base_dir_input, alt_name){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- ""
  }
  
  if(choice == 1) {
    return(str_c(base_dir_input,"/",alt_name,"/Input/","control.csv"))
  } else if(choice == 2){
    return(str_c(base_dir_input,"/",alt_name,"/Input/","densities.csv"))
  } else if(choice == 3){
    return(str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"))
  }
}
reading_file <- function(choice, base_dir_input, alt_name){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- ""
  }
  
  if(choice == 1) {
    a <- read.csv(str_c(base_dir_input,"/",alt_name,"/Input/","control.csv"))
    return(a)
  } else if(choice == 2){
    b <- read.csv(str_c(base_dir_input,"/",alt_name,"/Input/","densities.csv"))
    return(b)
  } else if(choice == 3){
    c <- read.csv(str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"))
    return(c)
  }
}
reading_output_of_alternative <- function(base_dir_input, alt_name, choice = ''){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  
  if(alt_name == ""){
    message("No Alternative name")
  }
  if(choice == '1') {
    pathOfLUOutput <- str_c(base_dir_input,"/",alt_name,"/Output/RSlam_output_revisedAr_",alt_name , ".csv" )
    message(pathOfLUOutput)
    main_output <- read.csv(str_c(base_dir_input,"/",alt_name,"/Output/RSlam_output_revisedAr_",alt_name , ".csv" ))
   
  } else {
    output_file_path <- list.files(str_c(base_dir_input,"/",alt_name,"/Output"), pattern = ".dbf$", full.names = T)
    main_output <- read.dbf(output_file_path)
  }
  return(main_output)

}
reading_output_file <- function(base_dir_input, alt_name, name_column){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  
  if(alt_name == ""){
    message("No Alternative name")
  }
  output_file_path <- list.files(str_c(base_dir_input,"/",alt_name,"/Output"), pattern = ".xlsx$", full.names = T) 
  
  output_data <- output_file_path %>%
    excel_sheets() %>%
    set_names() %>%
    map(read_excel,
        path = output_file_path)
  
  return(output_data[[name_column]])
}
reading_taz_output <- function(base_dir_input, alt_name, type = "model"){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- "Alt_A"
  }
  taz_visualize <- taz_shapefile[,1]
  #base_data <- list.files(str_c(basedir,"/",alt_name,"/Input"), pattern = ".dbf$", full.names = T)
  if(type == "model") {
    
    output_file_path <- list.files(str_c(basedir,"/",alt_name,"/Output"), pattern = ".dbf$", full.names = T)
    main_output <- read.dbf(output_file_path)
    final_data <- left_join(taz_visualize, main_output, by = c("MODEL_TAZ" = "ZONE"))
  } else if( type == "base") {
    
    base_file<- read.dbf(str_c(basedir,"/",alt_name,"/Input/RTC_LANDUSE_2017.dbf"))
    final_data <- left_join(taz_visualize, base_file, by = c("MODEL_TAZ" = "ZONE"))
  }  else if( type == "horizon") {
    horizon_file <- read.dbf(str_c(basedir,"/",alt_name,"/Input/RTC_LANDUSE_2045.dbf"))
    final_data <- left_join(taz_visualize, horizon_file, by = c("MODEL_TAZ" = "ZONE"))
  }
  return(final_data)
}
write_exclude <- function(choice, base_dir_input, alt_name){
  
  message(choice)
  message(alt_name)
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- ""
  }
  
  fileName <- read.csv(str_c(base_dir_input,"/",alt_name, "/Input/TAZx.csv"))
  
  selectionExclude <- as.data.frame(cbind(exComputed[["Model_TAZ"]], exComputed[[choice]]))
  colnames(selectionExclude) <- c("MODEL_TAZ", "ExcludeThis")
  x <- left_join(fileName, selectionExclude, by ="MODEL_TAZ")
  x$EXCLUDE <- x$ExcludeThis
  
  write.csv(x[,1:20], 
            str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"),
            row.names = F)
  
  
}
create_scen_map <- function(base_dir_input, alt_name, type=1){

  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- "Alt_A"
  }
  
  message(base_dir_input)
  message(alt_name)
  taz_sf <- st_read(str_c(basedir,"/",alt_name, "/Output"),
                    layer = "tazx")
  exclude_tazs <- read.csv(str_c(basedir,"/",alt_name,
                                 "/Output/Exclude.csv"))
  tdm_file <- read.dbf(str_c(basedir,"/",alt_name,
                             "/Output/RSlam_output_",alt_name,".dbf"))
  
  base_file <- read.dbf(str_c(basedir,"/",alt_name,
                              "/Input/RTC_LANDUSE_2045.dbf"))
  
  x <- tdm_file[!(tdm_file$N %in% exclude_tazs$MODEL_TAZ),] # taking out TAZs that are in excluded csv
  x_f <- x[x$N %in% taz_sf$MODEL_TAZ,]
  
  y_baseline <- left_join(x_f[,c("N","JUR","TOT_POP","HH","TOT_EMP")], 
                          base_file[,c("N","TOT_POP","HH","TOT_EMP")], 
                          by = "N")
  
  y_baseline$POP_Delta <- y_baseline$TOT_POP.x - y_baseline$TOT_POP.y
  y_baseline$HH_Delta <- y_baseline$HH.x - y_baseline$HH.y
  y_baseline$EMP_Delta <- y_baseline$TOT_EMP.x - y_baseline$TOT_EMP.y
  
  
  z <- left_join(taz_sf[taz_sf$MODEL_TAZ %in% y_baseline$N,1], 
                 y_baseline, by = c("MODEL_TAZ" = "N") )
  return(z)
  
}


#########################################################################################################

ui <- navbarPage(theme = shinytheme("flatly"),title = "Land-Allocation Model", id = "nav", 
                 tabPanel("About", div(class = "rvalogo",img(src='TPO_Logo_road.png',height="20%", width="20%", align = "right")), 
                          tags$style(".rvalogo{margin-bottom:-20px; padding-top : 15px; padding-right: 50px}"),
                          jumbotron("RSLAM", "Richmond Simplified Land Use Allocation Model (RSLAM) was developed to support the modeling and 
                  planning effort, and to facilitate scenario planning. It is a data-driven growth allocation model that creates 
                  a zonal data file suitable for input to and use by the RRTPO regional travel demand model. The model begins with
                  a zonal data file from the base year, and regional future year control totals for each zonal data item, and 
                  allocates the growth specified in the control totals to individual zones, thereby creating a future year zonal 
                  data file. It handles the eight jurisdictions in the region separately. It uses local data to guide the way that 
                  growth is allocated and was customized for Richmond. It is a fast R application, and has been calibrated using 
                  local data to replicate current forecasts. Besides the future year zonal data file, the model produces tables and 
                  maps to illustrate model assumptions and results and is controlled by a graphical user interface.",
                  buttonLabel = "Watch Video"),
                  fluidRow( column(6, 
                                   panel_div(class_type = "primary", panel_title = "Directions",
                                             content = HTML(" <a href='https://docs.google.com/document/d/14geGbfDAZhPOGPn5x2zHBvEsyPADUkvMELttNEHTm_4/edit?usp=sharing'>How to use the app</a>"))),   
                            column(6, panel_div("success", "Application Maintainers",
                                                HTML("Email: <a href='mailto:SAryal@planrva.org?Subject=Shiny%20Help' target='_top'> Sulabh Aryal</a>")))
                  ),  # end of fluidRow
                  fluidRow(
                    column(6, panel_div("info", "App Status", "RSLAM version 2.1")),
                    column(6, panel_div("danger", "Security and License", "Copyright 2020")), # End of Fluidrow
                    
                    #### FAVICON TAGS SECTION ####
                    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                    bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
                            p("Please watch the full video to learn how to run the Model"),
                            iframe(width = "560", height = "315", url_link = "https://www.youtube.com/watch?v=30oPT43Y4-I"))
                  ), # end of Fluidrow
                  
                  fluidRow(
                    column(6, panel_div( class_type = "primary",panel_title = "Created by :  ",
                                         content = list( div(class = "creators", 
                                                             h5(HTML("<a href = 'https://www.linkedin.com/in/ken-kaltenbach-a699149/'>Ken Kaltenbach </a>, 
                                   <a href = 'https://www.linkedin.com/in/parag-gupta-29413214a/'>Parag Gupta </a>, 
                                  <a href = 'https://www.linkedin.com/in/srinivas-srin-varanasi-9a538a2a/'>Srinivas Varanasi</a>"))),
                                  tags$style(".creators{color:; margin-bottom:-5px; margin-top:-12px}"),
                                  tags$style(".corradino{margin-bottom:-8px; margin-top:px;}"),
                                  div(class = "corradino",img(src='logo1.png', height = "47%", width = "47%"))
                                  
                                         ))))), # end of tabPanel
                 
                 tabPanel("Input", useSweetAlert(),
                          titlePanel("Input Modifiication"),
                          sidebarLayout(
                            sidebarPanel(
                              #titlePanel("Input Modifiication"),
                              # textOutput("value"),
                              textInput("base_dir", label = "Type the path of selected directory", placeholder = basedir),
                              textInput("alt_name", label = "Specify the name of the Alternative", 
                                        placeholder =  "Type the name of Alternative"),
                              actionButton("create_folder", label = "Create Alternative"), 
                              shinyDirButton('folder', 'Existing Alternative', 'Please select a folder', FALSE),
                              actionButton("changeName", label = "Read Data"),
                              pickerInput("m_file", "Which file you want to modify?",
                                          choices = c("Control Total" = 1, "Density (Rate of Consumption of land)" = 2, 
                                                      "TAZ" = 3),
                                          selected = 1, options = list(title = "Select from the choices")),
                              uiOutput("more_controls"),
                              actionButton("change_input", "Change Input"),
                              actionButton("csv", "Write  CSV"),
                              br(),
                              actionButton("run_model", "Run Model"),
                            ),
                            mainPanel(
                              tabsetPanel(
                              tabPanel("Baseline", reactableOutput("current_table") %>% withSpinner()),
                              tabPanel("Scenario Specific",
                                       tabsetPanel(
                                         tabPanel('Reading Data', reactableOutput("old_table") %>% withSpinner()),
                                         tabPanel('Writing CSV',  reactableOutput("new_table") %>% withSpinner())
                                                    )
                                       ),
                              tabPanel("TAZ Map", withSpinner(
                                             leafletOutput("taz_choice_map", width="100%", height= 900)) ,
                                             absolutePanel( class = "panel panel-default",
                                                            top = 150, right = 30, width = 350, fixed=TRUE,
                                                            draggable = TRUE, height = "auto",
                                                            style = "background-color: white;
                                              padding: 10px 0px 10px 10px;
                                              cursor: move;
                                            /* Fade out while not hovering */
                                              opacity: 0.75;
                                              zoom: 0.9;
                                          transition: opacity 500ms 1s;",
                                          
                                          pickerInput("taz_attribute", 
                                                      "Select TAZ attribute to see the information of :",
                                                      choices = colnames(taz_default)[4:length(taz_default)]),
                                          pickerInput("scenario", 
                                                      "Select any Alternative Geography :",
                                                      choices = colnames(taz_data)[25:ncol(taz_data)]),
                                          actionButton("writeExclude", "Write Exclude")
                                          
                                             )))
                              ) # end of main Panel
                            ) ), # end of Input Panel
                 ######################################################### OUTPUT TAB ############################################
                 tabPanel("Output",
                          tabsetPanel(tabPanel("Map",
                                               style = "position: fixed; top: 125px;left: 0; right: 0;
                                       bottom: 0; overflow: hidden; padding: 0;" ,
                                       leafletOutput("mymap", width="100%", height= 985) %>% withSpinner(),
                                       absolutePanel( class = "panel panel-default",
                                                      top = 150, right = 50, width = 230, fixed=TRUE,
                                                      draggable = TRUE, height = "auto",
                                                      style = "background-color: white;
                                              padding: 10px 0px 10px 10px;
                                              cursor: move;
                                            /* Fade out while not hovering */
                                              opacity: 0.8;
                                              zoom: 0.9;
                                          transition: opacity 500ms 1s;",
                                          selectInput("taz_col_name", label = "Select Variable to Visualize", 
                                                      choices = c("Total Population" = "TOT_POP", 
                                                                  "Households" = "HH", 
                                                                  "Reatil Jobs" = "RET_EMP",
                                                                  "Non Retail Jobs" = "NON_EMP"),               
                                                      selected = "Total Population", width = '95%'),
                                          selectInput("no_of_class", label = "Number of classes", 
                                                      choices =  c("3","4","5","6","7","8"), selected = "5",
                                                      width = '95%'),
                                          selectInput("breaks", "Method to Categorize:", width = '95%', 
                                                      choices = c("Quantile" = "quantile", 
                                                                  "Fisher" = "fisher",
                                                                  "Jenks" = "jenks",
                                                                  "K Means clustering" = "kmeans"),
                                                      selected = "jenks"),
                                          selectInput("color_palette", "Select Color Options", 
                                                      choices = row.names(brewer.pal.info), 
                                                      selected = row.names(brewer.pal.info)[35]),
                                          # actionButton("read_taz_output", "Read TAZ"),
                                          actionButton("plot", "Create Map")
                                       )),
                                      tabPanel("Table", selectInput("column_output", "Select table to choose", 
                                                                    choices = c("Output Summary","Input Summary","New Zonal Data","Growth Allocation","Growth Summary",  
                                                                                "Input scores","Input Controls" ,"Working Data", "ZDATA_in")),
                                               actionButton("read_data", "Output Data"),
                                               DT::dataTableOutput("alt_output") %>% withSpinner()),
                                      tabPanel("Main Output", selectInput("finalAll_output", "Select Model Output to display", 
                                                                          selected = '1',
                                                                          choices = c("Land Use"='1', "Travel Demand (SE Data)"='2' )),
                                               actionButton("read_model_data", "Read Data"),
                                               reactableOutput("model_file") %>% withSpinner()),
                                      tabPanel("Scenario Map", actionButton("read_Scen_data", "Read Scenario Data"),
                                               leafletOutput("scenMap", width="100%", height = "75vh") %>% withSpinner())
                                      
                          )))

##################################################################################################################

server <- function(input, output, session) {
  

  
  ############### EXISTING DIRECTORY CONTROL #########################################
  shinyDirChoose(input, 'folder', roots=c(wd='C:/'), filetypes=c('', 'txt'))
  
  # # file <- reactive(parseFilePaths(roots, input$file))
  # output$file <- renderPrint(input$folder[[1]][[2]])
  folder_name <- eventReactive(input$changeName,{
    input$folder[[1]][[length(input$folder[[1]])]]
  })
  
  observeEvent(input$changeName,{
    updateTextInput(session, "alt_name", value = folder_name())
  })
  ########################################################################################   
  
  ####################### CREATE NEW ALTERNATIVE #################################
  
  observeEvent(input$create_folder, { 
    do_this <- function(){
      creating_alternative(input$base_dir, input$alt_name)
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success"
      )}
    do_this()
  })
  
  ###########################################################################
  
  
  ############################## RUN MODEL #####################################
  observeEvent(input$run_model, 
               ask_confirmation(
                 inputId = "myconfirmation",
                 title = "Want to confirm ?"
               ))
  
  
  observeEvent(input$myconfirmation, {
    if (isTRUE(input$myconfirmation)) {
      
      showModal(modalDialog(
        title = "RSLAM Model",
        "RSLAM model in progress....",
        footer = NULL
      ))
      running_model(input$base_dir, input$alt_name)
      removeModal()
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success"
      )
      
    } else {
    }
  }, ignoreNULL = TRUE)
  
  ######################################################################################  
  #dynamic UI
  output$more_controls <- renderUI(
    changing_ui(input$m_file)
  )
  
  ###updating taz choices
  observeEvent(input$j_name, {
    updatePickerInput(session, "model_taz", choices = taz$MODEL_TAZ[taz$JUR %in% input$j_name])
  })
  
  #displaying the table to modify
  current_table_display <- eventReactive(input$m_file, {
    if(input$m_file == 1){
      return(control_default)
    }else if(input$m_file == 2){
      return(rate_of_consumption_default)
    } else if(input$m_file == 3){
      return(taz_default)
    }
  })
  output$current_table <- renderReactable(
    reactable(current_table_display(),  
              compact = T, filterable = T, defaultPageSize = 50, 
              bordered = T, highlight = T,
              showPageSizeOptions = TRUE, height = 730,
              defaultColDef = colDef(align = "center")))
  
  output$value <- renderText(string_to_display())
  
  group_taz_input <- eventReactive(input$change_input, 
                                   switch(input$m_file, "3" = input$model_taz ))  
  string_to_display <- eventReactive(input$change_input,
                                     #switch(input$m_file, str_c(past0(input[[all_ids[1:3]]], collapse = ","), ","),"bye","bye from 3")
                                     switch(input$m_file, 
                                            "1" = str_c(paste0(unlist(lapply(all_ids[1:5], 
                                                                             function(x) input[[x]])), collapse = ","), ","),
                                            "2" = str_c(paste0(unlist(lapply(all_ids[c(1,6:8, length(all_ids))], 
                                                                             function(x) input[[x]])), collapse = ","), ","),
                                            "3" = str_c(paste0(unlist(lapply(all_ids[12:length(all_ids) - 1], 
                                                                             function(x) input[[x]])), collapse = ","), ",")
                                     ))
  
  
  observeEvent(input$csv, { 
    write.csv(changing_input_file(string_to_display(),
                                  file_choice = input$m_file, 
                                  taz_group_input = group_taz_input()), 
              file = writing_file(input$m_file, input$base_dir, input$alt_name), 
              row.names =  F)
  })
  
  new_file <- eventReactive(input$csv, {
               changing_input_file(string_to_display(), 
               file_choice = input$m_file,
              taz_group_input = group_taz_input())
    })
    
    
  old_file <-  eventReactive(input$changeName, {
    reading_file( choice = input$m_file,
                           basedir, folder_name())
  })

  output$new_table <- renderReactable(
                  reactable(new_file(), 
              compact = T, filterable = T, 
              defaultPageSize = 50, bordered = T, highlight = T,
              showPageSizeOptions = TRUE, height = 730,
              defaultColDef = colDef(align = "center")) )
  
  output$old_table <- renderReactable(
    reactable(old_file(), 
              compact = T, filterable = T, 
              defaultPageSize = 50, bordered = T, highlight = T,
              showPageSizeOptions = TRUE, height = 730,
              defaultColDef = colDef(align = "center")) )
  
  
  ##### TAZ selection
  
  
  # output$taz_choice_table <- renderReactable(
  #   reactable(taz_data[,c(1:3,25:57)], selection = "multiple", onClick = "select",
  #             compact = T, filterable = T, defaultPageSize = 10,
  #             defaultSelected = 1:10))
  
  selected <- eventReactive(input$scenario, { taz_data[[input$scenario]] == 1})
  
  observeEvent(input$writeExclude,{
    write_exclude(choice = input$scenario, basedir, input$alt_name)
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
  
  putting_labels <- reactive( sprintf(
    "TAZ ID : %s<br/>%s : %g",
    taz_data$MODEL_TAZ[selected()], input$taz_attribute,
    taz_default[[input$taz_attribute]][selected()]) %>% lapply(htmltools::HTML))
  
  output$taz_choice_map <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>%
      # addDrawToolbar(
      #   targetGroup = "draw",
      #   editOptions = editToolbarOptions(
      #     selectedPathOptions = selectedPathOptions())) %>%
      # addProviderTiles(providers$Esri.WorldImagery ,  options = providerTileOptions(noWrap = TRUE)) %>%  
      # setView(lng = -77.43 , lat = 37.57, zoom = 10) %>% 
      addPolygons(data = st_transform(taz_shapefile[selected(),], crs = '+proj=longlat +datum=WGS84'), 
                  label = putting_labels(),
                  labelOptions = labelOptions( textsize = "14px"), 
                  fillColor = "#EC8F5E", weight = 2,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE)))
  
  #######Output
  
  data_to_display <- eventReactive(input$read_data, {
    reading_output_file(input$base_dir, input$alt_name, input$column_output)}) 
  
  output$alt_output <- DT::renderDataTable(data_to_display())
  main_model_output <-  eventReactive(input$read_model_data, 
                                      reading_output_of_alternative(input$base_dir, 
                                                                    input$alt_name,
                                                                    choice = input$finalAll_output))
  
  
  
  output$model_file <-  renderReactable(
    reactable(main_model_output(), 
              compact = T, filterable = T, 
              defaultPageSize = 50, bordered = T, highlight = T,
              showPageSizeOptions = TRUE, height = 730,
              defaultColDef = colDef(align = "center")) )
  
  tazs <- eventReactive(input$plot, 
                        reading_taz_output(input$base_dir, 
                                           input$alt_name, type = "model"))
  
  base_tazs <- eventReactive(input$plot, 
                             reading_taz_output(input$base_dir, 
                                                input$alt_name, type = "base"))
  
  future_tazs <- eventReactive(input$plot, 
                               reading_taz_output(input$base_dir, 
                                                  input$alt_name, type = "horizon"))
  
  scen_tazs <- eventReactive(input$read_Scen_data, 
                             create_scen_map(input$base_dir, 
                                             input$alt_name))
  
  
  output$scenMap <- renderLeaflet({
    leaflet() %>% 
      clearShapes() %>%
      clearControls() %>%
      addTiles() %>%
      setView(lng = -77.43 , lat = 37.54, zoom = 13)
  })
  
  observeEvent(input$read_Scen_data,{
    labels <- sprintf(
      "<strong>MODEL TAZ</strong> : %s<br/><strong>County </strong>: %s<br/>
  <strong>Scenario TotPop </strong>: %s<br/>
  <strong>Baseline Pop</strong> : %s<br/><strong>Scenario HH </strong>: %s<br/>
  <strong>Baseline HH</strong> : %s<br/><strong>Scenario TotEmp </strong>: %s<br/>
  <strong>Baseline TotEmp</strong> : %s<br/>",
  scen_tazs()$MODEL_TAZ, scen_tazs()$JUR,scen_tazs()$TOT_POP.x, 
  scen_tazs()$TOT_POP.y, scen_tazs()$HH.x, scen_tazs()$HH.y,
  scen_tazs()$TOT_EMP.x, scen_tazs()$TOT_EMP.y ) %>%
      lapply(htmltools::HTML)
    
    # getting breaks and coloring
    
    bks <- getBreaks(v = scen_tazs()$TOT_POP.x ,
                     nclass = 5,
                     method = "jenks")
    
    pal <- colorBin("YlOrRd", domain = scen_tazs()$TOT_POP.x, bins = bks)
    
    leafletProxy("scenMap") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = st_transform(scen_tazs(),4326),
                  weight = 2 , 
                  label = labels,
                  fillColor =  ~pal(scen_tazs()$TOT_POP.x),
                  fillOpacity = 0.7,
                  color = "black",
                  dashArray = "3",
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.4,
                    bringToFront = TRUE)) %>%
      addLegend(pal = pal, values =scen_tazs()$TOT_POP.x,
                opacity = 0.7, title = NULL, 
                position = "topright") %>%
      addScaleBar()
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      clearShapes() %>%
      clearControls() %>%
      addTiles() %>%
      setView(lng = -77.43 , lat = 37.54, zoom = 13) #%>%
    # addPolygons(data = st_transform(tazs(), crs = '+proj=longlat +datum=WGS84'),weight = 3, dashArray = 6,
    #             color = "white",
    #             fillColor = "black",
    #             fillOpacity = 0.2,  
    #             highlightOptions = highlightOptions(color = "black", weight = 2,bringToFront = TRUE))
    
  })
  
  bks2 <- eventReactive(input$plot, {
    as.numeric(getBreaks(v = switch(input$taz_col_name,
                                    "TOT_POP" = as.numeric(tazs()$TOT_POP),
                                    "HH" = as.numeric(tazs()$HH),
                                    "RET_EMP" = as.numeric(tazs()$RET_EMP),
                                    "NON_EMP" = as.numeric(tazs()$NON_EMP)) ,
                         nclass = as.numeric(input$no_of_class),
                         method = input$breaks))
    
  })
  
  bks <- eventReactive(input$plot, {
    as.numeric(getBreaks(v = switch(input$taz_col_name,
                                    "TOT_POP" = as.numeric(base_tazs()$TOT_POP ),
                                    "HH" = as.numeric(base_tazs()$HH),
                                    "RET_EMP" = as.numeric(base_tazs()$RET_EMP),
                                    "NON_EMP" = as.numeric(base_tazs()$NON_EMP)) ,
                         nclass = as.numeric(input$no_of_class),
                         method = input$breaks))
    
  })
  
  bks_horizon <- eventReactive(input$plot, {
    as.numeric(getBreaks(v = switch(input$taz_col_name,
                                    "TOT_POP" = as.numeric(future_tazs()$TOT_POP ),
                                    "HH" = as.numeric(future_tazs()$HH),
                                    "RET_EMP" = as.numeric(future_tazs()$RET_EMP),
                                    "NON_EMP" = as.numeric(future_tazs()$NON_EMP)) ,
                         nclass = as.numeric(input$no_of_class),
                         method = input$breaks))
    
  })
  
  
  observeEvent(input$plot , {
    pal <- colorBin(input$color_palette, domain = as.numeric(tazs()[[input$taz_col_name]]), bins = as.numeric(bks2()))
    pal_base <- colorBin(input$color_palette, domain = as.numeric(base_tazs()[[input$taz_col_name]]), bins = as.numeric(bks()))
    pal_horizon <- colorBin(input$color_palette, domain = as.numeric(future_tazs()[[input$taz_col_name]]), bins = as.numeric(bks_horizon()))
    labels = sprintf(
      "<strong>TAZ ID : %s</strong><br>%s : %g",
      tazs()$N, input$taz_col_name, tazs()[[input$taz_col_name]]) %>% lapply(htmltools::HTML)
    label_base = sprintf(
      "<strong>TAZ ID : %s</strong><br>%s : %g",
      base_tazs()$N, input$taz_col_name, base_tazs()[[input$taz_col_name]]) %>% lapply(htmltools::HTML)
    label_horizon = sprintf(
      "<strong>TAZ ID : %s</strong><br>%s : %g",
      future_tazs()$N, input$taz_col_name, future_tazs()[[input$taz_col_name]]) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("mymap") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons( data = st_transform(tazs(), crs = '+proj=longlat +datum=WGS84') ,
                   weight = 1, dashArray = 3,
                   fillColor = ~pal(tazs()[[input$taz_col_name]]),
                   fillOpacity = 0.7,
                   label = labels,
                   highlight = highlightOptions(
                     weight = 2, color = "green",fillOpacity = 0.5,bringToFront = TRUE, dashArray = ""),
                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                               textsize = "15px",direction = "auto"), group = "Model") %>%
      addLegend(pal = pal, values = tazs()[[input$taz_col_name]], 
                opacity = 0.7, title = NULL, position = "topleft", group = "Model") %>%
      addPolygons(data = st_transform(base_tazs(), crs = '+proj=longlat +datum=WGS84') ,
                  weight = 1, dashArray = 3,
                  fillColor = ~pal_base(base_tazs()[[input$taz_col_name]]),
                  fillOpacity = 0.7,
                  label = label_base,
                  highlight = highlightOptions(weight = 2, color = "green",fillOpacity = 0.5,bringToFront = TRUE, dashArray = ""),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",direction = "auto"), group = "2017") %>% 
      addLegend(pal = pal_base, values = base_tazs()[[input$taz_col_name]], 
                opacity = 0.7, title = NULL, position = "topleft", group = "2017") %>% 
      addPolygons( data = st_transform(future_tazs(), crs = '+proj=longlat +datum=WGS84') ,
                   weight = 1, dashArray = 3,
                   fillColor = ~pal_horizon(future_tazs()[[input$taz_col_name]]),
                   fillOpacity = 0.7,
                   label = label_horizon,
                   highlight = highlightOptions(
                     weight = 2, color = "green",fillOpacity = 0.5,bringToFront = TRUE, dashArray = ""),
                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                               textsize = "15px",direction = "auto"), group = "2050") %>%
      addLegend(pal = pal_horizon, values = future_tazs()[[input$taz_col_name]], 
                opacity = 0.7, title = NULL, position = "topleft", group = "2050") %>% 
      addLayersControl(  overlayGroups = c("2017","2050", "Model"), 
                         options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("2017","2050")) %>%
      showGroup("Model")
  })
}

shinyApp(ui, server)