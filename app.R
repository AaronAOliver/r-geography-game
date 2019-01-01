# apps.R: Single file script for a geography game
# Written by Aaron Oliver, 12/29/2018
# version date: 12/31/18

# Use package pacman to import other packages when running as a script locally on new machines
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, ggmap, maps, mapdata, plotly, shiny, dplyr, rnaturalearth, rnaturalearthdata, randomcoloR, rsconnect)

# Import packages for drawing maps
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(plotly)
library(shiny)
library(dplyr)
library(tools)
library(rnaturalearth)
library(rnaturalearthdata)
library(randomcoloR)
library(rsconnect)

# Why is this not the default
options(stringsAsFactors = FALSE)

## Read in local intermediate files and database files
# for drawing single states
statenames = as.data.frame(read.table("data/lower48.tab", sep="\t"))
states = map_data("state")

# for drawing single countries
worldmap = rnaturalearth::countries110
worldcountries = as.data.frame(read.table("data/world_countries.tab", sep="\t"))

# for drawing capitals
data(us.cities)
capitals <- subset(us.cities, capital == 2)
capitolnames <- capitals[!(capitals$country.etc %in% "AK" | capitals$country.etc %in% "HI"), ]

## Initialize global variables
map_color = "aquamarine"
statenamelist = c(NULL, NULL, NULL, NULL)
correctCount = 0
totalCount = -1
correctPos = -1
correctPlace = ""
NUM_ANSWER_BUTTONS = 4
CITY_SUFFIX_LENGTH = 3

# Flags to keep track of current mode
useStates = FALSE
useNations = FALSE
useCapitols = FALSE

# Function to randomly select a province or city
# Inputs
# regionnames (list): list of locations to use
# Output
# a single location name
generate_region = function(regionnames) {
  if (useCapitols) {
    # Use RNG to select a position in the given list of locations
    max = length(regionnames)
    randnumber = sample(1:max, 1, replace=F)
    statename = regionnames[randnumber[1]]
    return(statename)
  }
  
  else {
    # Use RNG to select a position in the given list of locations
    max = nrow(regionnames)
    randnumber = sample(1:max, 1, replace=F)
    statename = regionnames[randnumber[1], 1]
    return(statename)
  }
}

# Function to randomly select a list of locations
# Inputs
# regionnames (list): a list of locations to choose from
# length (integer): how long the output list should be
# Output
# a list of randomly selected locations
generate_region_list = function(regionnames, length) {
  current_list = c()
  while (length(current_list) < length) {
    #Ensure that no two buttons will have the same text as possible answers
    current_list = unique(c(current_list, generate_region(regionnames)), incomparables = FALSE)
  }
  
  return(current_list)
}

# Function to modify values after a button is pressed
# Inputs
# session (session): the current session of the user
update_buttons = function(session) {
  #Use different databases based on global flags, also need to modify text differently for user
  if (useStates) {
    statenamelist <<- generate_region_list(statenames, NUM_ANSWER_BUTTONS)
    
    for (buttonpos in 1:NUM_ANSWER_BUTTONS) {
      updateActionButton(session, paste0("state", buttonpos), label = toTitleCase(statenamelist[buttonpos]))
    } 
  }
  
  else if (useNations) {
    statenamelist <<- generate_region_list(worldcountries, NUM_ANSWER_BUTTONS)
    
    for (buttonpos in 1:NUM_ANSWER_BUTTONS) {
      updateActionButton(session, paste0("state", buttonpos), label = toTitleCase(statenamelist[buttonpos]))
    } 
  }
  
  else if (useCapitols) {
    cities = as.matrix(capitolnames$name)
    statenamelist <<- generate_region_list(cities, NUM_ANSWER_BUTTONS)
    
    for (buttonpos in 1:NUM_ANSWER_BUTTONS) {
      updateActionButton(session, paste0("state", buttonpos), label = toTitleCase(substring(statenamelist[buttonpos], 1, nchar(statenamelist[buttonpos]) - CITY_SUFFIX_LENGTH)))
    } 
  }
}

# Function that modifies the current score when an answer is given
# Input
# buttonnum (integer): position of the button that was pressed
update_score = function(buttonnum) {
  if (buttonnum == correctPos) {
    correctCount <<- correctCount + 1
  }
  
  totalCount <<- totalCount + 1
}

# Function that returns the program to its initial state
# Input
# session (session): the current session of the user
reset = function(session) {
  # Return global variables to initial values
  statenamelist <<- c(NULL, NULL, NULL, NULL)
  correctCount <<- 0
  totalCount <<- -1
  correctPos <<- -1
  correctPlace <<- ""
  
  # Give buttons menu labels and properties
  updateActionButton(session, "state1", label = "Nation Silhouettes")
  updateActionButton(session, "state2", label = "State Silhouettes")
  updateActionButton(session, "state3", label = "State Capitals")
  updateActionButton(session, "state4", label = "Randomize Map Color")
  
  # Set all global flags to false so user can change gamemode
  useStates <<- FALSE
  useNations <<- FALSE
  useCapitols <<- FALSE
}

# Define UI using R to HTML function
ui = fluidPage(
  
  tags$head(
    tags$style(HTML("
      
      h1 {
        font-family: 'Times New Roman', Times, serif;
        font-weight: 500;
        line-height: 1.1;
        color: #29559b;
      }

    "))
  ),

  headerPanel("States and Nations: A Geography Game"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("state1", label = "Nation Silhouettes", style = "font-family: serif; color: #29559b; font-size: 20px", width = "100%"),
      br(),
      actionButton("state2", label = "State Silhouettes", style = "font-family: serif; color: #29559b; font-size: 20px", width = "100%"),
      br(),
      actionButton("state3", label = "State Capitals", style = paste0("font-family: serif; color: #29559b; font-size: 20px"), width = "100%"),
      br(),
      actionButton("state4", label = "Randomize Map Color", style = "font-family: serif; color: #29559b; font-size: 20px", width = "100%")
    ),
    
    mainPanel(
      plotOutput("map"),
      textOutput("score"),
      actionButton("reset", label = "Reset", style = "font-family: sans-serif; color : #000000, font-size: 24 px;"),
      br(),
      br(),
      textOutput("desc")
    ),
    
    position = "right"
  )
)

# Define server logic
# Runs once initially, then again after any button press
server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    reset(session)
  })
  
  observeEvent(input$state1, {
    # Flag differentiates between menu function and answer selection
    if(!useStates && !useNations && !useCapitols && totalCount <= 0) {
      useNations <<- TRUE
    }
    update_buttons(session)
    update_score(1)
  })
  
  observeEvent(input$state2, {
    # Flag differentiates between menu function and answer selection
    if(!useStates && !useNations && !useCapitols && totalCount <= 0) {
      useStates <<- TRUE
    }
    update_buttons(session)
    update_score(2)
  })
  
  observeEvent(input$state3, {
    # Flag differentiates between menu function and answer selection
    if(!useStates && !useNations && !useCapitols && totalCount <= 0) {
      useCapitols <<- TRUE
    }
      update_buttons(session)
      update_score(3)
  })
  
  observeEvent(input$state4, {
    # Flag differentiates between menu function and answer selection
    if(useStates || useNations || useCapitols) {
      update_buttons(session)
      update_score(4)
    }
    
    else {
      map_color <<- randomColor()
    }
  })
  
  output$score = renderText({
    # Reference buttons so text will update when one is pressed
    relevant_buttons = c(input$state1, input$state2, input$state3, input$state4, input$reset)
    
    # Only write text when an answer is provided after a question is asked
    if (totalCount > 0) {
      paste0("The correct answer was ", toTitleCase(correctPlace), ". Your current score is ", correctCount, " out of ", totalCount, ", or ", format(round((100*correctCount)/as.double(totalCount), 2)), "%.") 
    }
  })
  
  output$desc = renderText({
    paste0("Welcome! This game was written in R to practice GIS and the use of geographical data. Thanks for stoping by!")
  })
  
  output$map = renderPlot({
    # Reference buttons so plot will update when one is pressed
    relevant_buttons = c(input$state1, input$state2, input$state3, input$state4, input$reset)
    
    # Use map_data for state information, ggplot to draw
    if ((!is.null(statenamelist)) && useStates) {
      # Randomly select which button option to draw 
      randnumber = sample(1:NUM_ANSWER_BUTTONS, 1, replace=F)
      correctPos <<- randnumber
      correctPlace <<- statenamelist[randnumber]
      
      # Draw using ggplot
      state_subset = subset(states, region == correctPlace)
      ggplot() + geom_polygon(data = state_subset, aes(x = long, y = lat, group = group), col = "grey", fill = map_color) +
        coord_fixed(1.3)  + theme_nothing()
    }
    
    # Use rnaturalearth for country information and to draw
    else if ((!is.null(statenamelist)) && useNations) {
      # Randomly select which button option to draw 
      randnumber = sample(1:NUM_ANSWER_BUTTONS, 1, replace=F)
      correctPos <<- randnumber
      correctPlace <<- statenamelist[randnumber]
      
      # Draw using rnaturalearth
      sp::plot(ne_countries(country = correctPlace, scale = 50), col = map_color)
    }
    
    # Use maps for city information, ggplot to draw
    else if ((!is.null(statenamelist)) && useCapitols) {
      # Randomly select which button option to draw 
      randnumber = sample(1:NUM_ANSWER_BUTTONS, 1, replace=F)
      correctPos <<- randnumber
      
      # Convert between "City STATEABBR" format and useable "state" and "city" format
      correctPlace <<- statenamelist[randnumber]
      desired_state_id = substring(correctPlace, nchar(correctPlace) - 1, nchar(correctPlace))
      desired_state = statenames[statenames$V2 == desired_state_id, 1]
      
      # Draw using ggplot
      ggplot(states, aes(x=long ,y=lat, group = group)) + 
        geom_polygon(fill="grey", colour = "white") +
        coord_fixed(1.3) + theme_nothing() +
        geom_polygon(fill = map_color, data = filter(states, region %in% desired_state)) +
        geom_point(shape = 19, data = capitolnames[capitolnames$country.etc == desired_state_id, ], aes(x=long, y=lat), inherit.aes = FALSE)
    }
  })
}

# Run the app
# Loops until crash or user leaves
shinyApp(ui = ui, server = server)
