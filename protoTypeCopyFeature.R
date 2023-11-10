library(shiny)
library(shinyWidgets)
library(foreign)


createParentScenario <- function(choice = FALSE){
  if(choice) {
    return(textInput("parentScenrio", 
                     label = "Type the Name of the Parent Scenario", 
                     placeholder = 'Alt_Name'))
  }
}
creating_alternative <- function(base_dir_input, alt_name, choice='', parent_path='' ){
  if(base_dir_input == ""){
    base_dir_input <- "C:/R_SLAM_2/BaseLU"
  }
  
  if(alt_name == ""){
    base_dir_input <- "C:/R_SLAM_2/BaseLU"
  }
  
  # Create directory files 
  dir.create(str_c("C:/R_SLAM_2/BaseLU/", alt_name))
  dir.create(str_c("C:/R_SLAM_2/BaseLU/",alt_name, "/", "Input"))
  dir.create(str_c("C:/R_SLAM_2/BaseLU/", alt_name,"/", "Output"))
  
  if(choice){
    message('I am copying files from Parent Scenario')
    parent_files_path <- list.files(paste0(base_dir_input, '/', parent_path,'/Input'), full.names = T)
    child_files_path <- paste0(base_dir_input,"/" ,alt_name, "/Input")
    file.copy(parent_files_path, child_files_path)
    
    landUseOutput <- read.csv(paste0(base_dir_input, '/', parent_path,
                          '/Output/RSlam_output_revisedAr_',parent_path,'.csv'))
    tdmOutput <- read.dbf(paste0(base_dir_input, '/', parent_path,
                                 '/Output/RSlam_output_',parent_path,'.dbf'))
    write.csv(landUseOutput, row.names = F, file = paste0(base_dir_input, '/', alt_name,
                                                       '/Input/input_2050.csv'))
    write.dbf(tdmOutput, , file = paste0(base_dir_input, '/', alt_name,
                                                       '/Input/RTC_LANDUSE_2045.dbf'))
    
    
  } else {
    parent_files_path <- list.files(paste0(base_dir_input, '/Alt_A/input'), full.names = T)
    child_files_path <- paste0(base_dir_input,"/" ,alt_name, "/input")
    file.copy(parent_files_path, child_files_path )
  }
  
  #reading files from the parent folder

  
}


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "label[for=Id005] {
                    font-size: 16px;
                }",
      )
    )
  ),
  
  awesomeCheckbox(
    inputId = "Id005",
    label = "Parent Scenario", 
    value = FALSE,
    status = "danger"
  ),
  uiOutput("parent_scenario"),
  actionButton("create_folder", label = "Create Alternative")
)

server <- function(input, output, session) {
  output$parent_scenario <- renderUI(
    createParentScenario(choice = input$Id005)
    )
  
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
  
}

shinyApp(ui, server)