# Load necessary packages

library(shiny)
library(tidyverse)
library(kableExtra)

# Load necessary data

bio <- read_csv('./data/smamm_bio.csv') # small mammal biographies

traits <- read_csv('./data/trait.csv') # small mammal functional traits

##### DATA WRANGLING ######

diet <- traits %>% 
  gather('diet_item', 'prop', 4:9) %>% 
  select(code, diet_item, prop)

##### PLOT ELEMENTS ######

colors <- c('darkgoldenrod3', 'cadetblue4', 'darkolivegreen4', 'navajowhite3', 'olivedrab3', 'coral3')
names(colors) <- as.character(unique(diet$diet_item))
labs <- c('Seeds', 'Fruits', 'Vegetation', 'Fungi', 'Invertebrates', 'Vertebrates')
names(labs) <- as.character(unique(diet$diet_item))

##### TEXT CHUNKS #####

text_fire <- 'In September-October 2014, a large human-ignited fire known as the King Fire scorched 39,545 ha in the northern Sierra Nevada, earning the distinction “mega-fire” due to its scope and intensity. Small mammal communities shifted drastically across the fire severity gradient, providing an interesting system to explore the role that these animals play in different habitats.'

text_smamm <- 'Smamms are fuzzy and cute! But watch out, they bite!'

##### USER INTERFACE #####

ui <- navbarPage('Big Fires, Small Mammals',

                 #### Tab 1 - The "King" of Mega-Fires####
                                  
                 tabPanel(
                   
                   'The \"King\" of Mega-fires',
                   
                   titlePanel('The \"King\" of Mega-fires'),
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       # Widget for selection of severity
                       
                       radioButtons('button_severity',
                                    'Select Severity',
                                    c('Unburned' = 'unb',
                                      'Low-Moderate Severity' = 'mod',
                                      'High Severity' = 'high'))
                     ),
                     
                     mainPanel(
                       text_fire,
                       
                       # Site photo
                       
                       imageOutput('photo_severity')
                     )
                   )  
                 ),
                 
                 #### Tab 2 - Meet the Mammals!####
                 
                 tabPanel(
                   
                   'Meet the Mammals!',
                   
                   titlePanel('Meet the Mammals!'),
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       # Widget for selection of smamm
                       
                       radioButtons('button_smamm',
                                    'Choose a smamm!',
                                    c('North American deer mouse' = 'PEMA',
                                      'Trowbridge\'s shrew' = 'SOTR', 
                                      'California ground squirrel' = 'SPBE',
                                      'Brush mouse' = 'PEBO', 
                                      'Long-eared chipmunk' = 'TAQU',
                                      'Shadow chipmunk' = 'TASE',
                                      'Dusky-footed woodrat' = 'NEFU',
                                      'Yellow pine chipmunk' = 'TAAM',
                                      'Northern flying squirrel' = 'GLSA',
                                      'Pinyon mouse' = 'PETR',
                                      'Western harvest mouse' = 'REME'))
                     ),
                     
                     mainPanel(
                       text_smamm,
                       
                       # Photo of smamm
                       
                       imageOutput('photo_smamm',
                                   hover = hoverOpts(
                                     id = 'license_hover'
                                   )),
                       
                       # Table of smamm info
                       
                       tableOutput('smamm_table'),
                       
                       # Pie chart of diet
                       
                       plotOutput('smamm_diet')
                     )
                   )
                 ),
                 
                 #### Tab 3 - Measuring Diversity####
                 
                 tabPanel('Measuring Diversity')
   

)

##### SERVER #####

server <- function(input, output) {
  
  #### Tab 1 - The "King" of Mega-Fires####
  
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

  #### Tab 2 - Meet the Mammals!####
  
  # Photo of smamm
  
  output$photo_smamm <- renderImage({
    if(input$button_smamm == 'PEMA') {
      return(list(
        src = 'www/PEMA.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'SPBE') {
      return(list(
        src = 'www/SPBE.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'TASE') {
      return(list(
        src = 'www/TASE.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'TAAM') {
      return(list(
        src = 'www/TAAM.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'GLSA') {
      return(list(
        src = 'www/GLSA.png', contentType = 'image/png', width = 300
      ))
    }
  }, deleteFile = FALSE)
  
  output$license_hover <- renderText({
    cat('TEST TSTTTTTTTAHIOAWg')
  })

  # Table of smamm info
  
  output$smamm_table <- function() {
    req(input$button_smamm)
    bio %>% 
      filter(code == input$button_smamm) %>% # FILTER FOR SMAMM
      gather(info, value, 2:7) %>% 
      select(-code) %>% 
      mutate(value = cell_spec(value, italic = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))) %>% 
      kable(escape = FALSE,
            col.names = c('','')) %>% 
      kable_styling(bootstrap_options = c('condensed', 'hover'),
                    full_width = FALSE) %>% 
      column_spec(1, bold = TRUE, width = '5cm') %>% 
      column_spec(2, width = '6cm')
  }
  
  # Pie chart of diet
  
  output$smamm_diet <- renderPlot({
    diet %>% 
      filter(code == input$button_smamm,
             prop != 0) %>% 
      arrange(-prop) %>% 
      mutate(diet_item = factor(diet_item, levels = diet_item)) %>% 
      arrange(prop) %>% 
      ggplot(aes(x = code, y = prop)) +
      geom_bar(aes(fill = diet_item),
               stat = 'identity',
               color = 'white',
               lwd = 3) +
      geom_text(aes(x = 1.6,
                    label = paste0(prop, '%')),
                position = position_stack(vjust = 0.5),
                size = 4.3, fontface = 'bold') +
      scale_fill_manual('Diet Item',
                        values = colors,
                        labels = labs) +
      coord_polar(theta = 'y', start = 1) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  })
  
  #### Tab 3 - Measuring Diversity####
  
}



# Run the application 
shinyApp(ui = ui, server = server)

