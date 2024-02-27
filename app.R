#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(gganimate)
library(shiny)
library(tidyverse)
library(cowplot)
library(ggridges)
library(repr)


#setwd("C:/Users/owenb/OneDrive/Documents/Sports Analytics 23-24/SAL 602/Module 8/Final Project")

plays <- read_csv("data/plays.csv")
games <- read_csv("data/games.csv")
players <- read_csv("data/players.csv")
tackles <- read_csv("data/tackles.csv")

missedTackles <- tackles %>% 
  filter(pff_missedTackle == 1) %>% 
  left_join(players, by = "nflId")
missedTacklesLionsEagles <- missedTackles %>% 
  filter(gameId == 2022091104)

playsLionsEagles <- plays %>% 
  filter(gameId %in% missedTacklesLionsEagles$gameId, 
         playId %in% missedTacklesLionsEagles$playId,
         !grepl("No Play.", playDescription))

possiblePlayIds <- playsLionsEagles$playId
names(possiblePlayIds) <- playsLionsEagles$playDescription

trackingWeek1 <- read_csv("data/tracking_week_1.csv")
trackingLionsEagles <- trackingWeek1 %>% 
  filter(gameId == 2022091104, 
         playId %in% playsLionsEagles$playId)

plays <- plays %>% left_join(missedTackles, by = c("gameId", "playId"))

plays <- plays %>% 
  filter(pff_missedTackle == 1) %>% 
  mutate(absoluteYardlineNumber = absoluteYardlineNumber - 10)

plays$down <- as.factor(plays$down)
plays$quarter <- as.factor(plays$quarter)


#turning off warnings
options(warn=-1)

#setting plot width and height
options(repr.plot.width = 15, repr.plot.height = 10)

#loading command to make NFL field in ggplot (credit to Marschall Furman)
source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')

#upright dimensions
uprightLength = 18.5/3
uprightYardline = 120
uprightAccrossFieldLocation = 160/6
uprightColor = "#E8DE35"
uprightlineWidth = 2
uprightShape = 21
uprightSize = 4
uprightOutlineColor = 'black'

#attributes used for plot. first is away, second is football, third is home.
cols_fill <- c("dodgerblue1", "#663300", "firebrick1")
cols_col <- c("#000000", "#663300", "#000000")
size_vals <- c(6, 4, 6)
shape_vals <- c(21, 16, 21)

trackingAnimationPlot <- ggplot(NULL) +
  
  
  #creating field underlay
  gg_field(yardmin = -2, yardmax = 122) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = 'forestgreen',
                                        color = 'forestgreen'),
        panel.grid = element_blank()) +
  
  
  #adding field goal uprights
  annotate(geom = 'segment',
           x = uprightYardline,
           xend = uprightYardline,
           y = uprightAccrossFieldLocation + uprightLength/2,
           yend = uprightAccrossFieldLocation - uprightLength/2,
           color = uprightColor,
           lwd = uprightlineWidth) +
  
  annotate(geom = 'point',
           x = uprightYardline,
           y = uprightAccrossFieldLocation + uprightLength/2,
           size = uprightSize,
           shape = uprightShape,
           fill = uprightColor,
           color = uprightOutlineColor) +
  
  annotate(geom = 'point',
           x = uprightYardline,
           y = uprightAccrossFieldLocation - uprightLength/2,
           size = uprightSize,
           shape = uprightShape,
           fill = uprightColor,
           color = uprightOutlineColor) +
  
  annotate(geom = 'segment',
           x = 0,
           xend = 0,
           y = uprightAccrossFieldLocation + uprightLength/2,
           yend = uprightAccrossFieldLocation - uprightLength/2,
           color = uprightColor,
           lwd = uprightlineWidth) +
  
  annotate(geom = 'point',
           x = 0,
           y = uprightAccrossFieldLocation + uprightLength/2,
           size = uprightSize,
           shape = uprightShape,
           fill = uprightColor,
           color = uprightOutlineColor) +
  
  annotate(geom = 'point',
           x = 0,
           y = uprightAccrossFieldLocation - uprightLength/2,
           size = uprightSize,
           shape = uprightShape,
           fill = uprightColor,
           color = uprightOutlineColor) +
  
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = FALSE) + 
  scale_shape_manual(values = shape_vals, guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE)

library(tidyverse)
library(rvest)


#Web Scraping

scrapeMissedTackles <- function(season) {
  url <- paste("https://www.pro-football-reference.com/years/",season,"/opp.htm", sep = "")
  webpage <- read_html(url)
  webpage %>% html_table() -> tables
  return(tables[[2]])
}

#Pro Football Reference Scraping Total Missed Tackles Last 2 Years
totalTacklesScraped <- tibble()

  for (season in 2022:2023) {
    Sys.sleep(runif(1)*10)
    tacklesScraped <- scrapeMissedTackles(season)
    totalTacklesScraped <- totalTacklesScraped %>% bind_rows(tacklesScraped)
}

write_csv(totalTacklesScraped, "totalTacklesScraped.csv")

totalTacklesScraped <- totalTacklesScraped %>% 
  group_by(Tm) %>% 
  summarise("Total Missed Tackles Last Two Seasons" = sum(MTkl))

missedTacklesLeaderboard <- totalTacklesScraped %>% arrange(`Total Missed Tackles Last Two Seasons`)



ui <- fluidPage(

  
    titlePanel("Grading Missed Tackles: An Exploration"),
    
    h3("2022 Week 1: Eagles at Lions: Missed Tackle Animations"),

    sidebarLayout(
      sidebarPanel(
      selectInput("selectPlay", 
                  h3("Select Play"),
                  choices = possiblePlayIds, 
                  selected = 86)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           imageOutput(outputId = "animation")
        )
    ),
    
    h3("Situational Scatterplot"),
    
    sidebarLayout(
      sidebarPanel(
        
        checkboxGroupInput("selectQuarter",
                           h4("Quarter"),
                           choices = list("1" = "1",
                                          "2" = "2",
                                          "3" = "3",
                                          "4" = "4",
                                          "OT" = "5"),
                           selected = c("1","2","3","4","5")),
        
        checkboxGroupInput("selectDown",
                           h4("Down"),
                           choices = list("1" = "1",
                                          "2" = "2",
                                          "3" = "3",
                                          "4" = "4"),
                           selected = c("1","2","3","4"))),
      
      
      mainPanel(
        plotOutput(outputId = "whenChart")
        
      )
    ),
    
    h3("Distance from End Zone Histogram"),
    
        sidebarLayout(
          sidebarPanel(
            sliderInput(inputId = "bins",
                        label = "Number of Bins",
                        min = 5,
                        max = 35,
                        value = 10)),
            
      mainPanel(
        plotOutput(outputId = "whereHistogram")
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        h3("Missed Tackles Leaderboard: 2022 and 2023")),
      mainPanel(
        tableOutput("table")
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  SelectedPlayReactive <- reactive({
    trackingLionsEagles %>% filter(playId == input$selectPlay)
  })
  
  
    output$animation <- renderImage({
      
        SelectedPlayReactive() -> example_play 
        
        outfile <- tempfile(fileext='.gif')
        
        p <- trackingAnimationPlot %+% example_play +
          
        geom_point(data = example_play, aes(x = x, 
                       y = y, 
                       shape = club, 
                       fill = club, 
                       group = nflId, 
                       size = club, 
                       colour = club), 
                   alpha = 0.7) +  
        
        #adding jersey numbers
        geom_text(data = example_play, aes(x = x, y = y, label = jerseyNumber),
                  colour = "white", 
                  vjust = 0.36, size = 3.5) + 
        
        labs(title = example_play$playDescription) +
        
        transition_time(frameId)  +
        ease_aes('linear')
        
        anim_save("outfile.gif", animate(p))
        
        list(src = "outfile.gif",
             contentType = 'image/gif')})
    
    output$whenChart <- renderPlot({
      
      plays %>% filter(quarter == input$selectQuarter, 
                       down == input$selectDown) %>% 
        ggplot() +
        geom_point(aes(x = yardsToGo, y = playResult, color = down)) + 
        scale_color_manual(values = c("green","blue","orange","pink")) +
        theme(legend.position = "bottom",
              legend.box = "vertical") +
        geom_abline(intercept = 0, slope = 1) +
        xlab("Yards to Go") + ylab("Yards Gained")
      
      
    })

    output$whereHistogram <- renderPlot({
      
      plays %>% filter(quarter == input$selectQuarter, 
                       down == input$selectDown) %>% 
        ggplot(aes(x = absoluteYardlineNumber, color = down)) +
        scale_color_manual(values = c("green","blue","orange","pink")) +
        theme(legend.position = "bottom",
              legend.box = "vertical") +
        geom_histogram(bins = input$bins) + 
        xlab("Distance from End Zone") + ylab("n")
    })
    
    output$table <- renderTable(totalTacklesScraped)
}

# Run the application 
shinyApp(ui = ui, server = server)
