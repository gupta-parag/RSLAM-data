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
                              awesomeCheckbox(
                                inputId = "Id005",
                                label = "Parent Scenario", 
                                value = FALSE,
                                status = "danger"
                              ),
                              uiOutput("parent_scenario"),
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
  
  output$parent_scenario <- renderUI(
    createParentScenario(choice = input$Id005)
  )
  
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
  
  observeEvent(input$create_folder,{
    do_this <- function(){
      # creating_alternative(input$base_dir, input$alt_name, choice=input$Id005,
      #                      parent_path = input$parentScenrio)
      creating_alternative(base_dir_input = '', alt_name = "alt_DerPrim", choice=input$Id005,
                           parent_path = input$parentScenrio)
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