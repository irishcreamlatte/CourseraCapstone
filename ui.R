library(shiny)
library(tidyverse)
library(quanteda)
library(qdap)
library(data.table) 

LoadToEnvironment <- function(RData, env = new.env()){
    load("data.RData", env)
    return(env)
}


shinyUI(fluidPage(theme = "bootstrap.css",
    
    tags$head(
        tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
    ),
    
    h1("Next Text Prediction App", 
               style = "font-family: 'Lobster', cursive;
        font-weight: 500; line-height: 1.1; 
        color: white;"),
    sidebarLayout(
        sidebarPanel(
            helpText("Using the Kneser-Ney algorithm, this App aims
                     to predict the next likely word in a three- or two-word
                     sequence. It also shows 5 other possible word continuations and their probabilities."), 
            helpText("Enter two words below:"),
            textInput("word1", "First Word:"),
            textInput("word2", "Second Word:"),
        ),
        mainPanel(
            h2("The predicted next word is:", style = "font-family: 'Lobster';
        font-weight: 500; line-height: 1.1; color: white;"),
            h3(textOutput("predicted"), style = "font-family: 'Cinzel';
        font-weight: 500; line-height: 1.1; 
        color: white;"),
            br(), br(),
            tableOutput("probabilities")
        )
        )
))