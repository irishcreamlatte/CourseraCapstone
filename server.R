library(shiny)
library(tidyverse)
library(quanteda)
library(qdap)
library(data.table)

LoadToEnvironment <- function(RData, env = new.env()){
    load("data.RData", env)
    return(env)
}

uni_df <- readRDS("./data/uni_df.RDS")
bi_df <- readRDS("./data/bi_df.RDS")
tri_df <- readRDS("./data/tri_df.RDS")

model <- function(x, y){
    if(x == "" & y == ""){
        predict = uni_df[sample(nrow(uni_df), 1), ] %>% 
            select(word)
    } else if(x %in% tri_df$word1 & 
              y %in% tri_df$word2){
        predict = tri_df %>% 
            filter(word1 %in% x &
                       word2 %in% y) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word3)
    }  else if(y %in% bi_df$word1){
        predict = bi_df %>% 
            filter(bi_df$word1 %in% y) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word2)
    } else if(x %in% tri_df$word2 & 
              y == ""){
        predict = tri_df %>% 
            filter(tri_df$word2 %in% x) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word3)
    } else if(x %in% bi_df$word1 & y == ""){
        predict = bi_df %>% 
            filter(bi_df$word1 %in% x) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word2)
    } else {
        predict = uni_df %>% 
            arrange(desc(Probability)) %>% 
            top_n(50, Probability) %>% 
            sample_n(size = 1, replace = TRUE) %>% 
            select(word)
    }
    return(as.character(predict))
}

shinyServer(function(input,output){
    
    modelpred <- function(){
        model(as.character(input$word1), 
              as.character(input$word2))
    }
    
    output$predicted <- renderText({modelpred()})
     
    model2 <- reactive({
        if(input$word1 %in% tri_df$word1 & 
           input$word2 %in% tri_df$word2){
            predict = tri_df %>% 
                filter(word1 %in% input$word1 &
                           word2 %in% input$word2) %>% 
                arrange(desc(Probability)) %>% 
                top_n(5, Probability) %>% 
                select(Word = word3, Probability)
        }  else if(input$word2 %in% bi_df$word1){
            predict = bi_df %>% 
                filter(bi_df$word1 %in% input$word2) %>% 
                arrange(desc(Probability)) %>% 
                top_n(5, Probability) %>% 
                select(Word = word2, Probability)
        } else if(input$word1 %in% tri_df$word2 & 
                  input$word2 == ""){
                predict = tri_df %>% 
                    filter(tri_df$word2 %in% input$word1) %>% 
                    arrange(desc(Probability)) %>% 
                    top_n(5, Probability) %>% 
                    select(Word = word3, Probability)
        } else if(input$word1 %in% bi_df$word1 & input$word2 == ""){
                predict = bi_df %>% 
                    filter(bi_df$word1 %in% input$word1) %>% 
                    arrange(desc(Probability)) %>% 
                    top_n(5, Probability) %>% 
                    select(Word = word2, Probability)
        } else {
            predict = uni_df %>% 
                arrange(desc(Probability)) %>% 
                top_n(5, Probability) %>% 
                select(Word = word, Probability)
        }
        return(predict)
    })
    
    
    output$probabilities <- renderTable({model2()})
    
})

