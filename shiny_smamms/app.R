# Load necessary packages

library(shiny)
library(tidyverse)

##### USER INTERFACE #####

text_fire <- 'a long long time ago there was a very big fire!'

ui <- navbarPage('Big Fires, Small Mammals',
                 
                 tabPanel('The \'King\' of Mega-fires',
                          
                          titlePanel('The \'King\' of Mega-fires'),
                          
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('button_severity',
                                           'Select Severity',
                                           c('Unburned' = 'unb', 'Low-Moderate Severity' = 'mod', 'High Severity' = 'high'))
                            ),
                            
                            mainPanel(text_fire)
                          )  
                          
                          ),
                 tabPanel('Meet the Mammals!'),
                 tabPanel('Measuring Diversity')
   

)



##### SERVER #####

server <- function(input, output) {}



# Run the application 
shinyApp(ui = ui, server = server)

