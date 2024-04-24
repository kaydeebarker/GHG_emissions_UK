# Load libraries ####
##Create groups of libraries
datalibraries <- c("dplyr", "tidyverse", "janitor", "readxl", "readODS") #data reading and  manipulation
vislibraries <- c("ggplot2", "ggthemes", "RColorBrewer", "scales") #pretty plots
applibraries <- c("shiny", "plotly", "gridlayout", "bslib", "DT") #libraries for app

##Install packages
#lapply(datalibraries, install.packages, character.only = TRUE)
#lapply(vislibraries, install.packages, character.only = TRUE)
#lapply(applibraries, install.packages, character.only = TRUE)
#devtools::install_github("rstudio/gridlayout") #install from github for this version of R


##Load libraries
lapply(datalibraries, library, character.only = TRUE)
lapply(vislibraries, library, character.only = TRUE)
lapply(applibraries, library, character.only = TRUE)


# Load data ####

##Map loop to download UK 2022 data from the UK Department for Energy Security and Net Zero
downloaded <- file.exists("UKGHG_2022.ods") #checks if file is downloaded in working directory
if(downloaded != T){ #if downloaded is not true
  map2("https://assets.publishing.service.gov.uk/media/65c0d54663a23d000dc821ca/final-greenhouse-gas-emissions-2022-by-source-dataset.ods", #update this link when new data available
       "UKGHG_2022.ods", download.file)} #else{print('data downloaded')} #name and download or print

##Read in ods file
GHG_UK22 <- read_ods(
  path = "UKGHG_2022.ods",
  sheet = 1, #define tab/sheet to read
  col_names = TRUE, #use header row for column names
  col_types = NULL, #guess data types
  na = "", #treat blank cells as NA
  skip = 0, #don't skip rows
  formula_as_formula = FALSE, #values only
  range = NULL, 
  row_names = FALSE, #no row names
  strings_as_factors = TRUE) %>% #use factors
  clean_names() %>% #clean column names to lowercase, with underscores
  separate_wider_delim(source, delim = " - ", #separate source, define delimiter as " - "
                       names = c("source_1", "source_2", "source_3"), #name new columns
                       too_few = "align_start", too_many = "merge", #what to do with fewer or more than 3
                       cols_remove = FALSE) %>% #keep original column
  separate_wider_delim(tes_category, delim = " - ", #separate category, define delimiter as " - "
                       names = c("category_1", "category_2"), #name new columns
                       too_few = "align_start", too_many = "merge", #what to do with fewer or more than 2
                       cols_remove = FALSE) %>% #keep original column
  separate_wider_delim(activity, delim = " - ", #separate activity, define delimiter as " - "
                       names = c("activity_1", "activity_2"), #name new columns
                       too_few = "align_start", too_many = "merge", #what to do with fewer or more than 2
                       cols_remove = FALSE) %>% #keep original column
  replace(is.na(.), "") #get rid of NA values, input blank


# Subset data ####

##Positive values only
GHG_UK22pos <- GHG_UK22 %>%
  filter(emissions_mt_co2e >= "0") #subset filter only positive values

##Negative values only
GHG_UK22neg <- GHG_UK22 %>%
  filter(emissions_mt_co2e <= "0") #subset filter only negative values

##Simplify
GHG_simp <- GHG_UK22 %>%
  filter(year == 2022) %>%
  group_by(tes_sector, tes_subsector, category_2, category_1, activity) %>%
  summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
  replace(is.na(.), "") %>%
  filter(Temissions >= "0") #subset filter only positive values

##Pivot - rows are greenhouse gas groups
GHG_wide <- GHG_UK22 %>%
  pivot_wider(names_from = ghg_grouped, values_from=emissions_mt_co2e) %>%
  replace(is.na(.), 0)

# Define filters ####

##Years, gases for user to select from
year <- unique(GHG_UK22$year)
GHG <- unique(GHG_UK22$ghg_grouped)

# Define colors ####

##Define custom color palette
colors <- c(
  "#3CBD9C", #transport
           "#F0A7CA", #buildings
           "#F0C954", #industry
           "#C24841", #electricity
           "#96BC61", #ag green
           "#5D7ED1", #lulucf blue
           "#D38744", #fuel
           "#A780BB"  #waste purple
)


colors_3lvls <- c(
  "#C24841", #CH4
           "#3CBD9C", #CO2
           "#F0C954"  #N2O
)



# Build UI dashboard frame with grid layout ####
ui <- grid_page(
  layout = c(
    "header     header     header   ",
    "filter    timeseries timeseries ",
    "emissions  emissions removals ",
    "emissions  emissions removals "
  ),
  row_sizes = c(
    "45px",
    "1.8fr",
    "0.2fr",
    "1fr"
  ),
  col_sizes = c(
    "200px",
    "0.8fr",
    "1.2fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "filter",
    card_header("Filters"
    ),
    card_body(
              selectInput(
                inputId = "Select_year",
                label = "Year",
                selected = max(year), #default select most recent
                choices = year)
    ),
    card_body(
              selectInput(
                inputId = "Select_GHG",
                label = "GHG group",
                selected = GHG, #default include all
                choices = GHG)
  ),
  ),
  grid_card_text(
    area = "header",
    content = "UK Greenhouse Gas Inventory",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "timeseries",
    full_screen = TRUE,
    card_header("Time Series - Net Emissions by Sector"),
    card_body(plotlyOutput(outputId = "t_plot"))
  ),
  grid_card(
    area = "removals",
    full_screen = TRUE,
    card_header("Negative Emissions (Removals)"),
    card_body(plotlyOutput(outputId = "sb_neg_plot"))
  ),
  grid_card(
    area = "emissions",
    full_screen = TRUE,
    card_header("Greenhouse Gas Emissions"),
    card_body(plotlyOutput(outputId = "sb_plot"))
  )
)

# Server define inputs outputs ####

server <- function(input, output, session) {
  
  produce_timeplot <- reactive({
    
  ## Time Series ####
  
  #Plot time series
  Sect_time <- GHG_UK22 %>%
    filter(ghg_grouped %in% input$select_GHG) %>%
    group_by(year, tes_sector) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
    rename("Sector" = "tes_sector") %>% #rename variable
    ggplot(aes(x=as.numeric(year), y=Temissions, color=Sector)) +
    geom_line() +
    xlab("") + ylab("Greenhouse Gas Emissions (Megatonnes CO2 eq.)") +
    ggtitle("Greenhouse Gas Emissions of the UK per Sector Over Time") +
    theme_few() +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = pretty(c(1990:2020), n=6)) +
    scale_color_brewer(palette = "Set2") +
    theme(legend.position="bottom")
  
  #Sect_time #view ggplot 
  
  #Make interactive with Plotly
  t_int <- plotly::ggplotly(Sect_time) %>% #convert to interactive graph
    layout(legend = list(
      orientation = "h")) %>%
    style(t_int, hovertemplate="Year: %{x:,.r} #define what hover label says, round x values
          CO2eq: %{y:,.4r} kt") #round y values to 4 sig. figs
  })
  
  produce_eplot <- reactive({
  
  ## Emissions data format for sunburst ####
  
  ##Separate out variables summaries - all GHGs combined
  sectors <- GHG_UK22pos %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_sector) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE))
  
  subsectors <- GHG_UK22pos %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_sector, tes_subsector) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE))
  
  cat_2 <- GHG_UK22pos %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    filter(category_2 != "") %>%
    group_by(tes_sector, tes_subsector, category_2) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
    unite('parents', c(tes_sector, tes_subsector), remove = TRUE, sep = " - ") #combine higher categories into parents
  
  cat_1 <- GHG_UK22pos %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_sector, tes_subsector, category_2, category_1) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
    mutate(parents = case_when(
      category_2 != "" ~str_c(tes_sector,tes_subsector,category_2, sep = " - "),
      category_2 == "" ~str_c(tes_sector,tes_subsector, sep = " - ")))
  
  act_1 <- GHG_UK22pos %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_sector, tes_subsector, category_2, category_1, activity_1) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
    mutate(parents = case_when(
      category_2 != "" ~str_c(tes_sector,tes_subsector,category_2, category_1, sep = " - "),
      category_2 == "" ~str_c(tes_sector,tes_subsector, category_1, sep = " - ")
    ))
  
  act_2 <- GHG_UK22pos %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    filter(activity_2 != "") %>%
    group_by(tes_sector, tes_subsector, category_2, category_1, activity_1, activity_2) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
    mutate(parents = case_when(
      category_2 != "" ~str_c(tes_sector,tes_subsector,category_2, category_1, activity_1, sep = " - "),
      category_2 == "" ~str_c(tes_sector,tes_subsector, category_1, activity_1, sep = " - ")
    ))
  
  
  ##Now paste together distinct instances into dataframe with hierarchical structure - start simple, set root
  GHGsb1 <- data.frame(
    parents =c(rep("", 8)),
    labels =c(paste0(sectors$tes_sector)),
    values =as.numeric(c(paste0(sectors$Temissions)))
  )
  
  ##Add id column
  GHGsb1 <- GHGsb1 %>%
    mutate(ids = labels) 
  
  ##Now paste together distinct instances into dataframe with hierarchical structure - add on
  GHGsb2 <- data.frame(
    parents =c(paste0(subsectors$tes_sector), paste0(cat_2$parents), paste0(cat_1$parents), 
               paste0(act_1$parents), paste0(act_2$parents)),
    labels =c(paste0(subsectors$tes_subsector), paste0(cat_2$category_2), paste0(cat_1$category_1), 
              paste0(act_1$activity_1), paste0(act_2$activity_2)),
    values =as.numeric(c(paste0(subsectors$Temissions), paste0(cat_2$Temissions), paste0(cat_1$Temissions), paste0(act_1$Temissions), paste0(act_2$Temissions)))
  )
  
  ##Add id column                    
  GHGsb2 <- GHGsb2 %>%
    unite('ids', c(parents, labels), remove = FALSE, sep = " - ") #combine columns for id
  
  ##Combine into one dataframe
  GHGsb <- rbind(GHGsb1, GHGsb2) 
  
  
  ## Sunburst Emissions with Plotly ####
  sb <- plot_ly(GHGsb,
                type = 'sunburst',
                ids = ~ids,
                labels = ~labels,
                parents = ~parents,
                values = ~values,
                branchvalues = 'total', #parents are totals of children
                insidetextorientation='radial',
                maxdepth = 2, #how many levels to show
                level = "",
                sort = TRUE #sort by size
                #width = 800, height = 800 #plot dimensions
  ) %>%
    layout(
      margin = list(l = 10, r = 10, b = 10, t = 10),
      sunburstcolorway = colors,
      extendsunburstcolors = TRUE
    ) %>%
    style(sb, hovertemplate="%{label} 
      %{value:,.3r} Mt CO2 equivalents
      %{percentParent:.1%} of %{parent} emissions
      <extra></extra>")
  
  })
  
  produce_rplot <- reactive({
  
  ## Removals data format for sunburst ####
  
  ##Replace empty source 2 values with source 1
  GHG_UK22neg$source_2[GHG_UK22neg$source_2 == ""] <- NA #also works to replace blanks or any value defined here with NA across a dataframe
  GHG_UK22neg$source_2[is.na(GHG_UK22neg$source_2)] <- GHG_UK22neg$source_1[is.na(GHG_UK22neg$source_2)]
  
  ##Separate out variables summaries - all GHGs combined
  subsectors_neg <- GHG_UK22neg %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_subsector) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE))
  
  cat_neg <- GHG_UK22neg %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_subsector, tes_category) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) 
  
  src_neg <- GHG_UK22neg %>%
    filter(year == input$select_Year & ghg_grouped %in% input$select_GHG) %>%
    group_by(tes_subsector, tes_category, source_2) %>%
    summarize(Temissions = sum(emissions_mt_co2e, na.rm = TRUE)) %>%
    unite('parents', c(tes_subsector, tes_category), remove = TRUE, sep = " - ") #combine higher categories into parents
  
  
  ##Now paste together distinct instances into dataframe with hierarchical structure - start simple, set root
  GHGsb1_neg <- data.frame(
    parents =c(rep("", 6)),
    labels =c(paste0(subsectors_neg$tes_subsector)),
    values =as.numeric(c(paste0(subsectors_neg$Temissions)))
  )
  
  ##Add id column
  GHGsb1_neg <- GHGsb1_neg %>%
    mutate(ids = labels) 
  
  ##Now paste together distinct instances into dataframe with hierarchical structure - add on
  GHGsb2_neg <- data.frame(
    parents =c(paste0(cat_neg$tes_subsector), paste0(src_neg$parents)),
    labels =c(paste0(cat_neg$tes_category), paste0(src_neg$source_2)),
    values =as.numeric(c(paste0(cat_neg$Temissions), paste0(src_neg$Temissions)))
  )
  
  ##Add id column                    
  GHGsb2_neg <- GHGsb2_neg %>%
    unite('ids', c(parents, labels), remove = FALSE, sep = " - ") #combine columns for id
  
  ##Combine into one dataframe
  GHGsb_neg <- rbind(GHGsb1_neg, GHGsb2_neg) 
  
  ##Convert negative values to absolute (positive) values
  GHGsb_neg$values <- abs(GHGsb_neg$values)
  
  ## Sunburst Removals with Plotly ####
  sb_neg <- plot_ly(GHGsb_neg,
                    type = 'sunburst',
                    ids = ~ids,
                    labels = ~labels,
                    parents = ~parents,
                    values = ~values,
                    branchvalues = 'total', #parents are totals of children
                    insidetextorientation='radial',
                    maxdepth = 2, #how many levels to show
                    level = "",
                    sort = TRUE #sort by size
                    #width = 800, height = 800 #plot dimensions
  ) %>%
    layout(
      #margin = list(l = 10, r = 10, b = 10, t = 10),
      sunburstcolorway = c(
        "#3CBD9C", #forestry
                 "#F0C954", #grassland
                 "#5D7ED1", #peatland
                 "#C24841", #other
                 "#96BC61",
                 "#F0A7CA", 
                 "#D38744", 
                 "#A780BB"
      ),
      extendsunburstcolors = TRUE
    ) %>%
    style(sb_neg, hovertemplate="%{label} 
      %{value:,.3r} Mt CO2 equivalents
      %{percentParent:.1%} of %{parent} removals
      <extra></extra>")
  })
  

  output$t_plot <- renderPlotly({produce_timeplot})
  output$sb_plot <- renderPlotly({produce_eplot})
  output$sb_neg_plot <- renderPlotly({produce_rplot})
  
  
}

# Run app ####
shinyApp(ui, server)

