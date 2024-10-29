library(shiny)

#1
ui <- fluidPage(
  textInput("name", "", placeholder = "Your name")
)

#2
library(lubridate)

ui <- fluidPage(
  sliderInput("date", "When should we deliver?", 
              min = ymd("2020-09-16"), max = ymd("2020-09-23"), value = ymd("2020-09-17"), timeFormat = "%F")
)

#3
ui <- fluidPage(
  sliderInput("value", "Select a number", 
              min = 0, max = 100, value = 0, step = 5, animate = T)
)

#4
ui <- fluidPage(
  selectInput("option", "Choice", 
              choices = list(`A` = list('A1', 'A2', 'A3'),
                             `B` = list('B1', 'B2', 'B3')))
)
