##### Load necessary packages ##### 

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyBS) # tooltips
library(gt) # making pretty tables
library(ape) # phylogenetic analysis
library(ggtree) # ggplot phylogenetic trees
library(ggwaffle) # ggplot waffle plots
library(emojifont) # emojis for waffle plot icons
library(vegan) # community ecology analysis
library(lefse) # weighted Faith's PD
library(FD) # functional diversity analysis
library(gridExtra) # combine plots in a grid

##### Load necessary data ##### 

smamms <- read_csv('./data/unique_smamms.csv') %>% # dataset with one row per unique individual
  mutate(severity = factor(severity, levels = c('unb', 'mod', 'high', 'fake')))

bio <- read_csv('./data/smamm_bio.csv') # small mammal biographies

traits <- read_csv('./data/trait.csv') # small mammal traits for diet

fn_traits <- read_csv('./data/trait2.csv') # data set with complete functional trait data

spp_matrix <- read_csv('./data/spp_matrix.csv') # species matrix

pruned <- read.nexus('./data/pruned_tree.nex') # phylogenetic tree


##### DATA WRANGLING ######

diet <- traits %>% 
  gather('diet_item', 'prop', 4:9) %>% 
  select(code, diet_item, prop)

sevs <- smamms %>% 
  dplyr::group_by(site, severity) %>% 
  summarize(length(species))

##### PLOT ELEMENTS ######

colors <- c('darkgoldenrod3', 'cadetblue4', 'darkolivegreen4', 'navajowhite3', 'olivedrab3', 'coral3')
names(colors) <- as.character(unique(diet$diet_item))
labs <- c('Seeds', 'Fruits', 'Vegetation', 'Fungi', 'Invertebrates', 'Vertebrates')
names(labs) <- as.character(unique(diet$diet_item))


#### USER INTERFACE ####

ui <- navbarPage('Big Fires, Small Mammals',
                 theme = shinytheme("sandstone"),
                 
                 #### Tab 1 - About ####
                 
                 tabPanel(
                   
                   'About',
                   
                   img(src = 'cover_photo.png', height = '400px'),
                   
                   h4(strong('What is this?')),
                   
                   p('This app is learning companion for an article published in', tags$em('Some Fancy Journal'), '.  Access the full article', tags$a('here.', href = 'https://www.disturbedsystems.com/')),
                   p(strong('Abstract:'), 'Fire regimes in North American forests have shifted drastically over the past century, with sweeping effects on small mammal communities. In particular, “mega-fires”—wildfires with extraordinary scope and intensity—have become more frequent. During fall 2014, a mega-fire known as the King Fire tore through Eldorado National Forest in the northern Sierra Nevada, burning a swath of 97,717 acres. Habitat near the geographic center of the King Fire burned at a high severity and tree mortality reached 100%, whereas habitat near the edges of the fire burned in heterogeneous mixed-severity patches. Three years after the fire, we established 27 sites across this gradient (9 high-severity, 9 mixed-severity, and 9 unburned) in order to investigate post-fire small mammal community recovery. At each 1-hectare site, we sampled small mammals using 100-m^2^ Sherman trapping grids. We used a spatial capture-recapture model to estimate small mammal density at each site, and calculated standard diversity indices. In burned sites, the overall density of small mammals increased while diversity decreased, with a shift towards communities overwhelmingly dominated by deer mice (Peromyscus maniculatus) and a loss of rare species such as shrews and woodrats. Although we found that small mammals were more abundant after a mega-fire, the loss in diversity signifies a decrease of species that might be providing essential ecosystem services.'),
                   
                   br(),
                   
                   h4(strong('How do I use this app?')),
                   
                   p('Click on the tabs to learn more about the 2014 King Fire and the response of the small mammal community.'),
                   
                   tags$ul(tags$li(strong('The \"King\" of Megafires'), '- use this tab to learn more about different areas of the King Fire'),
                           tags$li(strong('Meet the Mammals!'), '- use this tab to learn more about the eleven mammal species that we trapped during the summer of 2017'),
                           tags$li(strong('Measuring Diversity'), '- use this tab to learn about how altering species abundances change the species diversity')),
                   
                   br(),
                   
                   h4(strong('How was the data collected?')),
                   
                   p(strong('Small mammal community:'), 
                     'We sampled small mammal communities within a grid of 100 traps during one session at each site. Each session consisted of three continuous trap nights (maximum of 300 available traps per site). Captured small mammals were identified to species and marked with unique ear tags in order to track individuals across the 3-day period. Each mammal’s mass, sex, age, and reproductive status were recorded. Trap condition was also noted as a measure of the number of traps actually available for capturing small mammals. The full dataset has 1466 observations, with an separate observation for each capture or sprung/broken trap. In total, we captured 545 individuals of 11 species of mammals.'),
                   
                   p(strong('Habitat measurements:'), 
                     'Vegetation cover and volume were surveyed along two 50-m transects at each of the 27 sites. Percent cover for several plant and substrate types (dominant forb species, all other forbs, grasses, shrub species, tree species, woody litter, char, soft loose litter, soft rooted litter, rocks, bare ground, and other) was estimated within 1-m^2^ quadrats located every 5 m along the transect (10 quadrats per transect). For each cover type, we also recorded the median and maximum height in order to calculate an estimate of volume and biomass. The species and dimensions of trees, shrubs, and coarse woody debris were also surveyed within bands along the same 50-m vegetation transects. These data are in several disparate datasets, but I\'ve calculated several useful metrics for each of the 27 sites and compiled them into a single data frame: tree mortality, vegetation biomass (for trees, shrubs, grasses, and forbs), vegetation ground cover, volume of coarse woody debris, vegetation diversity (for trees, shrubs, and dominant forbs).'),
                   
                   p(strong('Small mammal functional traits:'), 
                     'This dataset has quantifies four ecologically relevant traits for each of the 11 species: body size, diet, foraging mode, and activity time. Body size measurements were calculated as the average mass from field measurements, but the rest of the data were collated from two field guides as well as species accounts from the American Society of Mammalogists. Diet is split into six columns with the proportion for each of six diet items (seeds, fruits, other vegetation, fungi, invertebrates, and vertebrates). Activity time is a categorical variable (nocturnal, diurnal, or cathemeral). Foraging mode is split into two variables indicating to what extent the mammal is a) arboreal and b) fossorial. Data from the National Audubon Society Field Guide to North American Mammals, ', tags$sup('1'), 'Peterson Reference Guide to Behavior of North American Mammals, ', tags$sup('2,3'), 'and species accounts from the American Society of Mammalogists.', tags$sup('4–13')),
                   
                   br(),
                   
                   h4(strong('Who collected the data?')),
                   
                   p('Data collection was a collorative effort between researchers at the US Forest Service and University of California (Davis and Santa Barbara campuses).'),
                   
                   br(),
                   
                   h4(strong('Who did the analyses?')),
                   
                   p('These analyses were run by Kate Culhane, a graduate student at University of California, Santa Barbara. Check out her work ', tags$a('here!', href = 'https://www.disturbedsystems.com/')),
                   
                   br(),
                   
                   h4(strong('References')),
                   
                   tags$ol(tags$li('Whitaker, J. O. National Audubon Society Field Guide to North American Mammals. (Alfred A. Knopf Inc., 1996).'),
                           tags$li('Elbroch, M. & Rinehart, K. Peterson Reference Guide to Behavior of North American Mammals. (Houghton Mifflin Harcourt, 2011).'),
                           tags$li('Reid, F. Peterson field guide to mammals of North America. (Houghton Mifflin Harcourt, 2006).'),
                           tags$li('Clawson, R. G., Clawson, J. A. & Best, T. L. Tamias quadrimaculatus. Mamm. Species 7 (1994).'),
                           tags$li('Gannon, W. L. & Forbes, R. B. Tamias senex. Mamm. Species (1995).'),
                           tags$li('Sutton, D. A. Tamias amoenus. Mamm. Species (1992).'),
                           tags$li('George, S. B. Sorex trowbridgii. Mamm. Species 1 (1989). doi:10.2307/3504159'),
                           tags$li('Smith, J. E., Long, D. J., Russell, I. D., Newcomb, K. L. & Muñoz, V. D. Otospermophilus beecheyi. Mamm. Species 48, 91–108 (2016).'),
                           tags$li('Webster, W. D. & Jones, J. K. Reithrodontomys megalotis. Mamm. Species 1 (1982). doi:10.2307/3504020'),
                           tags$li('Hoffmeister, D. F. Peromyscus truei. Mamm. Species 1 (1981). doi:10.2307/3503851'),
                           tags$li('Kalcounis-Rueppell, M. C. & Spoon, T. R. Peromyscus boylii (Rodentia: Cricetidae). Mamm. Species 838, 1–14 (2009).'),
                           tags$li('Carraway, L. N. & Verts, B. J. Neotoma fuscipes. Mamm. Species 1 (1991). doi:10.2307/3504130'),
                           tags$li('Wells-Gosling, N. & Heaney, L. R. Glaucomys sabrinus. Mamm. Species 1 (1984). doi:10.2307/3503926'))

                 ),

                 #### Tab 2 - The "King" of Mega-Fires ####
                                  
                 tabPanel(
                   
                   'The \"King\" of Mega-fires',
                   
                   titlePanel('The \"King\" of Mega-fires'),
                   
                   sidebarLayout(
                     sidebarPanel(width = 3,
                       
                       # Widget for selection of severity
                       
                       radioButtons('button_severity',
                                    'Where do you want to visit?',
                                    c('Unburned sites' = 'unb',
                                      'Low-moderate severity sites' = 'mod',
                                      'High severity sites' = 'high'))
                     ),
                     
                     mainPanel(
                       
                       'In September-October 2014, a large human-ignited fire known as the King Fire scorched 39,545 ha in the northern Sierra Nevada, earning the distinction “mega-fire” due to its scope and intensity. Small mammal communities shifted drastically across the fire severity gradient, providing an interesting system to explore the role that these animals play in different habitats.',

                       # Site photo
                       
                       imageOutput('photo_severity')
                     )
                   )  
                 ),
                 
                 #### Tab 3 - Meet the Mammals! ####
                 
                 tabPanel(
                   
                   'Meet the Mammals!',
                   
                   titlePanel('Meet the Mammals!'),
                   
                   # Radio buttons to select smamm
                   
                   sidebarPanel(width = 3,
                     
                     p('We caught eleven species of small mammals at our sites. Choose a mammal and click on the tabs to learn more!'),
                     
                     br(),
                     
                     radioButtons('button_smamm',
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
                                    'Trowbridge\'s shrew' = 'SOTR'))
                     ),
                   
                   mainPanel(

                     tabsetPanel(
                       
                       tabPanel('General Information',
                         fluidRow(
                           
                           br(),
                           
                           # Photo of smamm
                           
                           column(3, br(), br(), imageOutput('photo_smamm')),
                           
                           # Table of smamm info
                           
                           column(9, gt_output('smamm_table'))
                           
                         ),
                         
                         fluidRow(
                           
                           # Pictograph of habitat preference
                           
                           plotOutput('smamm_pictograph')
                           
                         )
                         ),
                       
                       tabPanel('Functional Traits',
                         
                         br(),
                         
                         p('Small mammals serve a wide range of functions in their habitat, including seed dispersal, soil bioturbation, regulation of insect populations, and food resources for predators. Many of these functions are tied to aspects of the mammal\'s ecology: for example, diet is a good indicator of a mammal\'s role as a seed disperser. We can calculate the functional diversity of an animal community by quantifying many possible functions, including diet, and combining hem across an entire community. Click on the \"Measuring Diversity\" tab to learn about how the functional diversity of the King Fire small mammal communities changed.'),
                         
                         fluidRow(
                           
                           # Pie chart of diet
                           
                           column(12, plotOutput('smamm_diet'))
                           
                         )
                       ),
                       
                       tabPanel('Phylogeny',
                         
                         br(),
                         
                         p('This phylogenetic tree includes all mammals observed at our sites. Small mammals included in our analyses are in black, and larger mammals are in grey. Most of the small mammals we trapped are grouped together in order Rodentia - the rodents. The shrew (a tiny insectivore) was the only non-rodent we caught in our traps. We can calulate the phylogenetic diversity of an animal community by adding up the branch lengths of every organism in the community. Click on the \"Measuring Diversity\" tab to learn about the phylogenetic diversity of the King Fire small mammal communities changed.'),
                         
                         fluidRow(
                           
                           # Phylogenetic tree
                           
                           column(12, plotOutput('phylo_tree'))
                           
                         )
                       )
                     )
                   )
                 ),
                 
                 #### Tab 4 - Measuring Diversity ####
                 
                 tabPanel(
                   
                   'Measuring Diversity',
                   
                   titlePanel('Measuring Diversity'),
                   
                   p('There are many different ways to calculate diversity. Three of the commonly reported metrics are:',
                     tags$ul(tags$li(strong('Species Diversity'), '- how many species are there, and how evenly are individuals distributed between them?'),
                             tags$li(strong('Functional Diversity'), '- how diverse are the functions provided by the community?'),
                             tags$li(strong('Phylogenetic Diversity'), '- how diverse are the taxonomic groups of the community?')),
                     'Use the sliders below to create your own small mammal community, and see how the calculated diversity metrics compare to actual data from the King Fire! Adjusting a species\' slider changes its abundance in the diversity calculations. Your new mammal community will be displayed as a purple dot in the plots below.'),
                   
                   fluidRow(
                     column(12,
                       wellPanel(
                         fluidRow(
                           
                           column(3,
                                  
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
                                         round = TRUE, ticks = FALSE)
                           ),
                           
                           column(3,
                                  
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
                           
                           column(3,
                             
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
                                         round = TRUE, ticks = FALSE)
                           ),
                           
                           column(3,
                             
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
                         )
                       )
                     )
                   ),
                   
                   fluidRow(img(src = 'legend.png', height = '50px'), align = 'center'),
                   
                   fluidRow(
                     
                     # Jitter plot of Shannon diversity
                     
                     column(3,
                            h4(strong('Species Diversity')),
                            plotOutput('H_plot'),
                            align = 'center'),

                     # Jitter plot of functional diversity
                     
                     column(6,
                            h4(strong('Functional Diversity')), 
                            plotOutput('FD_plot'),
                            align = 'center'),
                     
                     # Jitter plot of phylogenetic diversity
                     
                     column(3,
                            h4(strong('Phylogenetic Diversity')),
                            plotOutput('PD_plot'),
                            align = 'center')
                     
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
        src = 'www/site_unb.png', contentType = 'image/png', width = 500
      ))
    }
    if(input$button_severity == 'mod') {
      return(list(
        src = 'www/site_mod.png', contentType = 'image/png', width = 500
      ))
    }
    if(input$button_severity == 'high') {
      return(list(
        src = 'www/site_high.png', contentType = 'image/png', width = 500
      ))
    }
  }, deleteFile = FALSE)

  #### Tab 3 - Meet the Mammals! ####
  
  # Photo of smamm
  
  output$photo_smamm <- renderImage({
    if(input$button_smamm == 'PETR') {
      return(list(
        src = 'www/PETR.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'PEBO') {
      return(list(
        src = 'www/PEBO.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'PEMA') {
      return(list(
        src = 'www/PEMA.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'REME') {
      return(list(
        src = 'www/REME.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'NEFU') {
      return(list(
        src = 'www/NEFU.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'TAAM') {
      return(list(
        src = 'www/TAAM.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'TASE') {
      return(list(
        src = 'www/TASE.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'TAQU') {
      return(list(
        src = 'www/TAQU.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'SPBE') {
      return(list(
        src = 'www/SPBE.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'GLSA') {
      return(list(
        src = 'www/GLSA.png', contentType = 'image/png', width = 200
      ))
    }
    if(input$button_smamm == 'SOTR') {
      return(list(
        src = 'www/SOTR.png', contentType = 'image/png', width = 200
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
                size = 5, fontface = 'bold') +
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
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12))
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
  
  # Jitter plot of Shannon diversity
  
  output$H_plot <- renderPlot(height = 300, {
    
    smammunity <- c(input$slider_PETR,
                    input$slider_PEBO,
                    input$slider_PEMA,
                    input$slider_REME,
                    input$slider_NEFU,
                    input$slider_TAAM,
                    input$slider_TASE,
                    input$slider_TAQU,
                    input$slider_SPBE,
                    input$slider_GLSA,
                    input$slider_SOTR)
    
    H <- data.frame(spp_matrix[,1], diversity(spp_matrix[,-1]), sevs[,2]) %>% 
      rename(H = diversity.spp_matrix....1..,
             site = X1)
    
    new_H <- data.frame(H = diversity(smammunity),
                        severity = 'fake') %>% 
      mutate(severity = factor(severity, levels = c('unb', 'mod', 'high', 'fake')))
    
    colors <- c('darkgreen', 'orange', 'red', 'purple')
    xlabs <- c('Unburned', 'Moderate\nSeverity', 'High\nSeverity', 'YOUR\nNEW\nCOMMUNITY')
    
    ggplot2::ggplot() +
      geom_jitter(data = H,
                  aes(x = severity, y = H, color = severity),
                  width = 0.15, size = 2) +
      geom_point(data = new_H,
                 aes(x = severity, y = H, color = severity),
                 size = 5) +
      scale_x_discrete(drop = FALSE,
                       labels = xlabs) +
      scale_color_manual(values = rev(colors)) +
      theme_classic() +
      labs(x = 'Fire Severity',
           y = 'Shannon Diversity (H)') +
      theme(legend.position = 'NA')
    
  })
  
  # Jitter plot of functional diversity
  
  output$FD_plot <- renderPlot(height = 500, {
    
    fn_traits2 <- data.frame(fn_traits) %>% 
      arrange(X1)
    rownames(fn_traits2) <- fn_traits2$X1
    fn_traits2 <- fn_traits2 %>% 
      select(-X1)
    
    sites <- spp_matrix[,1]
    spp_matrix_mtx <- data.matrix(spp_matrix)
    rownames(spp_matrix_mtx) <- sites$X1
    spp_matrix_mtx <- spp_matrix_mtx[,-1]
    
    weight <- c(0.25, # weight for body mass
                0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.04166667, # weight for diet categories
                0.125, 0.125, # weight for foraging mode attributes
                0.05, 0.05, 0.05, 0.05, 0.05) # weight for weediness attributes
    
    FD_indices <- dbFD(fn_traits2, spp_matrix_mtx, weight)
    
    FD <- data_frame(sites$X1, FD_indices$FRic, FD_indices$FDiv, FD_indices$FEve, FD_indices$FDis) %>% 
      rename(site = 'sites$X1',
             FRic = 'FD_indices$FRic',
             FDiv = 'FD_indices$FDiv',
             FEve = 'FD_indices$FEve',
             FDis = 'FD_indices$FDis') %>% 
      full_join(sevs)
 
    # Make a graph
    
    colors <- c('red', 'orange', 'darkgreen')
    xlabs <- c('Unburned', 'Moderate\nSeverity', 'High\nSeverity', 'YOUR\nNEW\nCOMMUNITY')
    
    FD1 <- ggplot2::ggplot() +
      geom_jitter(data = FD,
                  aes(x = severity, y = FRic, color = severity),
                  width = 0.15, size = 2) +
      scale_x_discrete(drop = FALSE,
                       labels = xlabs) +
      scale_color_manual(values = rev(colors)) +
      theme_classic() +
      labs(x = 'Fire Severity',
           y = 'Functional Richness (Fric)') +
      theme(legend.position = 'NA')
    FD2 <- ggplot2::ggplot() +
      geom_jitter(data = FD,
                  aes(x = severity, y = FDiv, color = severity),
                  width = 0.15, size = 2) +
      scale_x_discrete(drop = FALSE,
                       labels = xlabs) +
      scale_color_manual(values = rev(colors)) +
      theme_classic() +
      labs(x = 'Fire Severity',
           y = 'Functional Divergence (FDiv)') +
      theme(legend.position = 'NA')
    FD3 <- ggplot2::ggplot() +
      geom_jitter(data = FD,
                  aes(x = severity, y = FEve, color = severity),
                  width = 0.15, size = 2) +
      scale_x_discrete(drop = FALSE,
                       labels = xlabs) +
      scale_color_manual(values = rev(colors)) +
      theme_classic() +
      labs(x = 'Fire Severity',
           y = 'Functional Evenness (FEve)') +
      theme(legend.position = 'NA')
    FD4 <- ggplot2::ggplot() +
      geom_jitter(data = FD,
                  aes(x = severity, y = FDis, color = severity),
                  width = 0.15, size = 2) +
      scale_x_discrete(drop = FALSE,
                       labels = xlabs) +
      scale_color_manual(values = rev(colors)) +
      theme_classic() +
      labs(x = 'Fire Severity',
           y = 'Functional Dispersion (FDis)') +
      theme(legend.position = 'NA')
    
    grid.arrange(FD1, FD2, FD3, FD4, ncol=2)
    
  })
  
  # Jitter plot of phylogenetic diversity
  
  output$PD_plot <- renderPlot(height = 300, {
    
    smammunity <- c(input$slider_PETR,
                    input$slider_PEBO,
                    input$slider_PEMA,
                    input$slider_REME,
                    input$slider_NEFU,
                    input$slider_TAAM,
                    input$slider_TASE,
                    input$slider_TAQU,
                    input$slider_SPBE,
                    input$slider_GLSA,
                    input$slider_SOTR)
    
    # Calculation for site
    
    spp_matrix2 <- spp_matrix
    spp_matrix2$num_spp <- rowSums(spp_matrix2[2:12]!=0)
    spp_matrix2 <- spp_matrix2 %>% 
      filter(num_spp != 1)
    sites <- spp_matrix2$X1
    spp_matrix2 <- spp_matrix2 %>% 
      select(-X1, -num_spp)
    rownames(spp_matrix2) <- sites
    spp_tree <- c('Glaucomys_sabrinus',
                  'Neotoma_fuscipes',
                  'Peromyscus_boylii',
                  'Peromyscus_maniculatus',
                  'Peromyscus_truei',
                  'Reithrodontomys_megalotis',
                  'Sorex_trowbridgii',
                  'Otospermophilus_beecheyi',
                  'Neotamias_amoenus',
                  'Neotamias_quadrimaculatus',
                  'Neotamias_senex')
    
    colnames(spp_matrix2) <- spp_tree
    
    PD <- data.frame(weighted.faith(pruned, spp_matrix2)) %>%
      mutate(site = rownames(.),
             PD = weighted.faith.pruned..spp_matrix2.) %>% 
      full_join(sevs) %>% 
      select(site, severity, PD) %>% 
      mutate(PD = replace_na(PD, 13.97496))
    
    new_matrix <- data.frame(t(smammunity))
    spp_tree2 <- c('Peromyscus_truei',
                   'Peromyscus_boylii',
                   'Peromyscus_maniculatus',
                   'Reithrodontomys_megalotis',
                   'Neotoma_fuscipes',
                   'Neotamias_amoenus',
                   'Neotamias_senex',
                   'Neotamias_quadrimaculatus',
                   'Otospermophilus_beecheyi',
                   'Glaucomys_sabrinus',
                   'Sorex_trowbridgii')

    colnames(new_matrix) <- spp_tree2
    
    new_PD_value <- tryCatch(
      weighted.faith(pruned, new_matrix),
      error = function(e) {print(''); 13.97496}
    )

    new_PD <- data.frame(PD = new_PD_value,
                         severity = 'fake') %>%
      mutate(severity = factor(severity, levels = c('unb', 'mod', 'high', 'fake')))
    
    colors <- c('darkgreen', 'orange', 'red', 'purple')
    xlabs <- c('Unburned', 'Moderate\nSeverity', 'High\nSeverity', 'YOUR\nNEW\nCOMMUNITY')
    
    # Graph
    
    ggplot2::ggplot() +
      geom_jitter(data = PD,
                  aes(x = severity, y = PD, color = severity),
                  width = 0.15, size = 2) +
      geom_point(data = new_PD,
                 aes(x = severity, y = PD, color = severity),
                 size = 5) +
      scale_x_discrete(drop = FALSE,
                       labels = xlabs) +
      scale_color_manual(values = rev(colors)) +
      theme_classic() +
      labs(x = 'Fire Severity',
           y = 'Weighted Faith\'s Phylogenetic Diversity (PD)') +
      theme(legend.position = 'NA')
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

