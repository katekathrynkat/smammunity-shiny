# Load necessary packages

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyBS) # tooltips
library(gt) # making pretty tables
library(ape) # phylogenetic analysis
library(ggtree) # ggplot phylogenetic trees
library(ggwaffle) # ggplot waffle plots
library(emojifont) # emojis for waffle plot icons

# Load necessary data

smamms <- read_csv('./data/unique_smamms.csv') # dataset with one row per unique individual

bio <- read_csv('./data/smamm_bio.csv') # small mammal biographies

traits <- read_csv('./data/trait.csv') # small mammal functional traits

pruned <- read.nexus('./data/pruned_tree.nex') # phylogenetic tree

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

#### USER INTERFACE ####

ui <- navbarPage('Big Fires, Small Mammals',
                 theme = shinytheme("sandstone"),
                 
                 #### Tab 1 - About ####
                 
                 tabPanel(
                   
                   'About',
                   
                   titlePanel('About')
                   
                 ),

                 #### Tab 2 - The "King" of Mega-Fires ####
                                  
                 tabPanel(
                   
                   'The \"King\" of Mega-fires',
                   
                   titlePanel('The \"King\" of Mega-fires'),
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       # Widget for selection of severity
                       
                       radioButtons('button_severity',
                                    'Where do you want to visit?',
                                    c('Unburned sites' = 'unb',
                                      'Low-moderate severity sites' = 'mod',
                                      'High severity sites' = 'high'))
                     ),
                     
                     mainPanel(
                       text_fire,
                       
                       # Site photo
                       
                       imageOutput('photo_severity')
                     )
                   )  
                 ),
                 
                 #### Tab 3 - Meet the Mammals! ####
                 
                 tabPanel(
                   
                   'Meet the Mammals!',
                   
                   titlePanel('Meet the Mammals!'),
                   
                   sidebarLayout(
                     sidebarPanel(width = 4,
                       
                       # Widget for selection of smamm
                       
                       radioButtons(
                         
                         'button_smamm',
                         'Who do you want to meet?',
                         
                         c('Pinyon mouse' = 'PETR',
                           'Brush mouse' = 'PEBO', 
                           'North American deer mouse' = 'PEMA',
                           'Western harvest mouse' = 'REME',
                           'Dusky-footed woodrat' = 'NEFU',
                           'Yellow pine chipmunk' = 'TAAM',
                           'Shadow chipmunk' = 'TASE',
                           'Long-eared chipmunk' = 'TAQU',
                           'California ground squirrel' = 'SPBE',
                           'Northern flying squirrel' = 'GLSA',
                           'Trowbridge\'s shrew' = 'SOTR')
                         
                       )
                     ),
                     
                     mainPanel(
                       text_smamm,
                       
                       # Photo of smamm
                       
                       imageOutput('photo_smamm'),
                       
                       # Table of smamm info
                       
                       gt_output('smamm_table'),
                       
                       # Pie chart of diet
                       
                       plotOutput('smamm_diet'),
                       
                       # Phylogenetic tree
                       
                       plotOutput('phylo_tree'),
                       
                       # Pictograph of habitat preference
                       
                       plotOutput('smamm_pictograph')
                     )
                   )
                 ),
                 
                 #### Tab 4 - Measuring Diversity ####
                 
                 tabPanel(
                   
                   'Measuring Diversity',
                   
                   titlePanel('Measuring Diversity'),
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       div(
                         
                         style = "display:inline-block; vertical-align:top; width:150px;",
                         
                         sliderInput('slider_PETR',
                                     'Pinyon mouse',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),
                         
                         sliderInput('slider_PEBO',
                                     'Brush mouse',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_PEMA',
                                     'North American deer mouse',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_REME',
                                     'Western harvest mouse',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_NEFU',
                                     'Dusky-footed woodrat',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_TAAM',
                                     'Yellow-pine chipmunk',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE)
                         
                       ),
                       
                       div(
                         
                         style="display:inline-block; vertical-align:top; width:20px;",
                         
                         HTML("<br>")
                         
                         ),
                       
                       div(
                         
                         style = "display:inline-block; vertical-align:top; width:150px;",
                         
                         sliderInput('slider_TASE',
                                     'Shadow chipmunk',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),
                         
                         sliderInput('slider_TAQU',
                                     'Long-eared chipmunk',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_SPBE',
                                     'California ground squirrel',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_GLSA',
                                     'Northern flying squirrel',
                                     min = 0, max = 10,
                                     value = 0, step = 1,
                                     round = TRUE, ticks = FALSE),

                         sliderInput('slider_SOTR',
                                     'Trowbridge\'s shrew',
                                     min = 0, max = 10, value = 0, step = 1,
                                     round = TRUE, ticks = FALSE)
                         
                       )
                      
                     ),
                     
                     mainPanel(
                       
                     )
                   )
                   
                   )
   

)

#### SERVER ####

server <- function(input, output, session) {
  
  #### Tab 2 - The "King" of Mega-Fires ####
  
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

  #### Tab 3 - Meet the Mammals! ####
  
  # Photo of smamm
  
  output$photo_smamm <- renderImage({
    if(input$button_smamm == 'PETR') {
      return(list(
        src = 'www/PETR.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'PEBO') {
      return(list(
        src = 'www/PEBO.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'PEMA') {
      return(list(
        src = 'www/PEMA.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'REME') {
      return(list(
        src = 'www/REME.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'NEFU') {
      return(list(
        src = 'www/NEFU.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'TAAM') {
      return(list(
        src = 'www/TAAM.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'TASE') {
      return(list(
        src = 'www/TASE.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'TAQU') {
      return(list(
        src = 'www/TAQU.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'SPBE') {
      return(list(
        src = 'www/SPBE.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'GLSA') {
      return(list(
        src = 'www/GLSA.png', contentType = 'image/png', width = 300
      ))
    }
    if(input$button_smamm == 'SOTR') {
      return(list(
        src = 'www/SOTR.png', contentType = 'image/png', width = 300
      ))
    }
  }, deleteFile = FALSE)

  # Table of smamm info
  
  output$smamm_table <- render_gt({
    req(input$button_smamm)
    bio %>% 
      filter(code == input$button_smamm) %>% # FILTER FOR SMAMM
      gather(info, value, 2:8) %>% 
      select(-code) %>% 
      gt() %>% 
      tab_header(
        title = md('**Mammal Information**')
      ) %>% 
      cols_label(info = '', value = '') %>% 
      tab_style(
        style = cells_styles(text_weight = 'bold'),
        locations = cells_data(
          columns = vars(info)
        )
      ) %>% 
      tab_style(
        style = cells_styles(text_style = 'italic'),
        locations = cells_data(
          columns = vars(value),
          rows = 4
        )
      ) %>% 
      tab_options(
        table.width = px(500)
      )
  })
  
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
  
  # Phylogenetic tree
  
  output$phylo_tree <- renderPlot({
    
    pruned_grouped <- groupClade(pruned, c(25,26,35))
    
    phylo_labels <- data.frame(label = pruned_grouped$tip.label,
                               name = c('Virgina opossum', 'Mountain lion', 'Bobcat', 'Gray fox', 'Coyote', 'Spotted skunk', 'Striped skunk', 'American black bear', 'Mule deer', 'Trowbridge\'s shrew', 'Human', 'California ground squirrel', 'Shadow chipmunk', 'Yellow pine chipmunk', 'Long-eared chipmunk', 'Northern flying squirrel', 'Dusky-footed woodrat', 'Brush mouse', 'Pinyon mouse', 'North American deer mouse', 'Western harvest mouse', 'White-tailed jackrabbit'),
                               code = c('', '', '', '', '', '', '', '', '', 'SOTR', '', 'SPBE', 'TASE', 'TAAM', 'TAQU', 'GLSA', 'NEFU', 'PEBO', 'PETR', 'PEMA', 'REME', '')) %>% # data frame for labels
      mutate(match = grepl(input$button_smamm, code)) # FILTER FOR SMAMM
    
    ggtree(pruned_grouped, aes(color = group),
           size = 1.4) %<+% phylo_labels +
      scale_color_manual(values = c('grey60', 'black', 'grey60', 'black')) +
      
      geom_tiplab(aes(label = name,
                      alpha = factor(match)),
                  offset = 2,
                  geom = 'label',
                  fill = 'coral2', color = NA) +
      scale_alpha_manual(values = c(0, 1)) +
      geom_tiplab(aes(label = name), offset = 3) +
      geom_rootedge(rootedge = 10, size = 1.4) +
      xlim(-10,400)
  })
  
  # Pictograph of habitat preference
  
  output$smamm_pictograph <- renderPlot({
  
    batter <- smamms %>% 
      mutate(
        rows = case_when(
          species == 'PEMA' ~ 10,
          TRUE ~ 5
        ),
        color = case_when(
          severity == 'high' ~ 'red',
          severity == 'mod' ~ 'orange',
          severity == 'unb' ~ 'darkgreen'
        )
      ) %>% 
      filter(species == input$button_smamm) # FILTER FOR SMAMM
    
    waffled <- waffle_iron(batter, aes_d(group = color), rows = unique(batter$rows))
    
    ggplot() +
      geom_emoji('mouse2', x = waffled$x, y = waffled$y,
                 color = waffled$group,
                 size = 5) +
      coord_equal() +
      ggplot2::xlim(c(-1,43)) +
      ggplot2::ylim(c(0,11)) +
      labs(x = '', y = '') +
      theme_waffle()
  })
  
  #### Tab 4 - Measuring Diversity ####
  
}



# Run the application 
shinyApp(ui = ui, server = server)

