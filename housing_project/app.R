#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
#library(stringr)


#library(sf)
#library(raster)
#library(spData)
#library(spDataLarge)
#library(tmap)    # for static and interactive maps
#library(leaflet) # for interactive maps

hvi <- read_csv(file = 'assets/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv', col_types =c('d'))
zori <- read_csv(file = 'assets/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.csv', col_types =c('d'))
merged <- full_join(hvi, zori, by='RegionID')
merged <- rename(merged, rent = '2022-05')
merged <- rename(merged, home_value ='2022-05-31')
merged$rtv <- as.numeric(merged$rent) / as.numeric(merged$home_value)*100



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rent to Value"),

    # Sidebar with a slider input for number of bins 
        mainPanel(
          plotOutput(outputId = "scatterplot"),
          tableOutput(outputId = "top25")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterplot <- renderPlot({
      merged %>% dplyr::filter(home_value < 500000) %>% ggplot(., aes(x = home_value, y = rent, label=RegionName.x, color=rtv)) + geom_point() + geom_text(hjust=0, vjust=0)
      
  })
    
    output$top25 <- renderTable({
      
      merged %>%  
      #dplyr::filter(home_value < 500000) %>%  
      #dplyr::filter(City == 'Detroit') %>%  
      dplyr::select(RegionName.x, City, State, home_value, rent, rtv)  %>% 
       drop_na() %>% arrange(-rtv) %>% top_n(1000)
      
    }, digits=2)
}

# Run the application 
shinyApp(ui = ui, server = server)
