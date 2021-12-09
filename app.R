library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
data(data)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("slate"),
    titlePanel("Movies on Streaming Platforms"),
    
    tabsetPanel(
        tabPanel("Ratings by Genre", 
                 #sidebar
                 sidebarLayout(
                     
                     #sidebarPanel 
                     sidebarPanel(
                         
                         p("The boxplots generated here are created with the Movies on Streaming Platforms Dataset,
                           obtained on", a("Kaggle.", href="https://www.kaggle.com/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
                           "These data contain information on movies available on streaming platforms in the United States,
                           and availability of these U.S. movies in other countries. The data contain ratings scraped from the 
                           IMDb and Rotten Tomatoes websites. Use the dropdown below to examine IMDb or Rotten Tomatoes ratings
                           for the different genres in the data.", strong("Note"), ": a single movie can be listed under multiple genres in the data."
                             
                         ),
                         
                         br(),
                         
                         # Dropdown menu that allows the user to choose rating type
                         selectInput(inputId = "rating", label = "Choose a Rating Type",
                                     choices = c(IMDb ="IMDb", `Rotten Tomatoes`="rotten_tom")),
                     ),
                     
                     # Main panel
                     mainPanel( 
                         
                         # Plot
                         plotOutput("boxplot")))),
        
        tabPanel("Ratings by Year and Target Audience Age",
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         p("The boxplots generated here are created with the Movies on Streaming Platforms Dataset,
                           obtained on", a("Kaggle.", href="https://www.kaggle.com/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
                           "These boxplots are grouped by the intended age group
                           for the movie's audience (ex: 17+ indicates the movie was intended for viewers 17 years and older).
                           Use the slider below to examine the boxplots of IMDb ratings by movie release year.",
                           strong("Note"), ": the dataset contains movies with release years as old as 1914, but the data on intended age group 
                           are sparse up until the late 1960s."
                             
                         ),
                         
                         br(),
                         
                         sliderInput("year", "Movie Release Year:",
                                     min = 1968, max = 2021,
                                     value = 1968, 
                                     step = 1,
                                     sep = "",       
                                     ticks = FALSE,  
                                     animate = TRUE)
                     ),
                     
                     mainPanel(
                         plotOutput("boxplot2")
                     )
                 )
        )
    )
)


server <- function(input, output) {
    output$boxplot = renderPlot({
        data %>% 
            select(input$rating, genres) %>%
            ggplot(aes_string(x = "genres", y = input$rating, group="genres")) + 
            geom_boxplot() +
            scale_y_continuous() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            xlab("Genre") +
            ylab("Ratings") + labs(title="Movie Ratings by Genre")
    })
    
    output$boxplot2 = renderPlot({
        data %>% 
            filter(Year %in% input$year & !is.na(Age)) %>%
            select(IMDb, Age) %>% 
        ggplot(aes(x=Age, y=IMDb, group=Age)) +
            geom_boxplot() + 
            xlab("Intended Age Group") + 
            ylab("IMDb Ratings") +
            labs(title="Movie Ratings by Target Audience Age, for Selected Release Year")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
