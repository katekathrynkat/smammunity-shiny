# Load necessary packages

library(shiny)
library(tidyverse)

# Text chunks

text_fire <- 'In September-October 2014, a large human-ignited fire known as the King Fire scorched 39,545 ha in the northern Sierra Nevada, earning the distinction “mega-fire” due to its scope and intensity. Small mammal communities shifted drastically across the fire severity gradient, providing an interesting system to explore the role that these animals play in different habitats.'

text_smamm <- 'Smamms are fuzzy and cute! But watch out, they bite!'

##### USER INTERFACE #####

ui <- navbarPage('Big Fires, Small Mammals',

                 #### Tab 1 ####
                                  
                 tabPanel(
                   
                   'The \'King\' of Mega-fires',
                   
                   titlePanel('The \'King\' of Mega-fires'),
                   
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons('button_severity',
                                    'Select Severity',
                                    c('Unburned' = 'unb',
                                      'Low-Moderate Severity' = 'mod',
                                      'High Severity' = 'high'))
                     ),
                     
                     mainPanel(
                       text_fire,
                       imageOutput('photo_severity')
                     )
                   )  
                 ),
                 
                 #### Tab 2 ####
                 
                 tabPanel(
                   
                   'Meet the Mammals!',
                   
                   titlePanel('Meet the Mammals!'),
                   
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons('button_smamm',
                                    'Choose a smamm!',
                                    c('North American deer mouse' = 'PEMA',
                                      'Yellow pine chipmunk' = 'TAAM'))
                     ),
                     
                     mainPanel(
                       text_smamm,
                       imageOutput('photo_smamm')
                     )
                   )
                 ),
                 
                 #### Tab 3 ####
                 
                 tabPanel('Measuring Diversity')
   

)



##### SERVER #####

server <- function(input, output) {
  
  #### Tab 1 ####
  
  # Site photo
  
  output$photo_severity <- renderImage({
    if(input$button_severity == 'unb') {
      return(list(
        src = 'www/site_unb.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_severity == 'mod') {
      return(list(
        src = 'www/site_mod.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_severity == 'high') {
      return(list(
        src = 'www/site_high.png', contentType = 'image/png', width = 300
      ))
    }
  }, deleteFile = FALSE)

  #### Tab 2 ####
  
  # Mammal photo
  
  output$photo_smamm <- renderImage({
    if(input$button_smamm == 'PEMA') {
      return(list(
        src = 'www/PEMA.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'TAAM') {
      return(list(
        src = 'www/TAAM.png', contentType = 'image/png', width = 300
      ))
    }
  }, deleteFile = FALSE)
  
  #### Tab 3 ####
  
}



# Run the application 
shinyApp(ui = ui, server = server)

