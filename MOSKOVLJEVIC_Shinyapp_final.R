##Created by: Andrea Moskovljevic
##Project: Deep Dive Into Shark Tank

###############
library(devtools)
library(shiny)
library(shinyjqui)
library(DT)
library(shinyjs)
library(shinycustomloader)
library(readxl)
library(devtools)
library(dashboardthemes)
library(viridis)
library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(tableHTML)
library(wesanderson)
library(shinydashboardPlus)
library(ggalt)


###############
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(gridExtra)


Shark_tank = read.csv("shark_tank_final.csv", stringsAsFactors = FALSE)

Shark_tank <- Shark_tank %>%
  group_by(category) %>% 
  mutate(popularity = sum(deal)) %>% 
  ungroup() %>% 
  mutate(depV = case_when(deal == "TRUE" ~ 1,
                          TRUE ~ 0))


Shark_tank$season = as.factor(Shark_tank$season)
Shark_tank$episode = as.factor(Shark_tank$episode)

Shark_tank$valuation = as.numeric(Shark_tank$valuation)

ui <- dashboardPage(
    dashboardHeader(title = "EDA Shark Tank"),
    dashboardSidebar(
        sidebarMenu(
          menuItem("Purpose of the App ", tabName = "purpose", icon = icon("info")),
          menuItem("Key Information About Deals ", tabName = "key_info_tab", icon = icon("dollar-sign")),
            menuItem("Deal Insights", tabName = "deal_insights_tab", icon = icon("chart-bar")),
            menuItem("Deal Trends", tabName = "deal_trends_tab", icon = icon("chart-line"))
        )
    ),
    ## BODY CONTENT
    dashboardBody(shinyDashboardThemes(
      theme = "blue_gradient"), ###Change of theme
        
        tags$head(tags$style(HTML(
            '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: black;
      }
    '))),
        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Total Number of Pitches : 495 </span>\');
      })
     ')),
    
        
        
        
        tabItems(
          
          #INTRO TAB
         
      
       tabItem(tabName ="purpose",
               
               widgetUserBox(
                 width = 12,
                 height = 300,
                 title = "Hey there! Welcome to SharkTank Analytics!",
                 subtitle = "Click on the tabs on your left to start",
                 type = NULL,
                 src = "https://png.pngtree.com/svg/20170224/shark_13492.png",
                 background = TRUE,
                 backgroundUrl = "https://d3specialists.com/wp-content/uploads/2019/02/Every-Impression-Counts-Are-Your-Digital-Advertising-and-Branding-Tactics-Delivering-ROI.jpg",
                 closable = FALSE,
                 footer = "Starting a business is difficult. Gaining the recognition for the business idea and getting the initial investment can be equally, or even more challenging. 
                 
                The purpose of the app is to support entrepreneurs, like you, who are interested in starting their new ventures, by providing basic insights on which variables play a key role when it comes to getting an investment. It focuses on exploring the data from the popular U.S. based TV Show “Shark Tank”, where entrepreneurs pitch business ideas to a panel of seasoned investors (the sharks). I believe that the challenges and triumphs of the entrepreneurs featured on Shark Tank can be relatable to other business owners, thus I wanted to perform the analysis of the business pitches and their corresponding variables that appear when getting the deal"),
               
               hr(),
               hr()),
                 
                 
          
            #FIRST TAB CONTENT
            tabItem(tabName = "key_info_tab", 
                    column( width = 3,
                            uiOutput("key_info_Location_input_ui"),
                            uiOutput("key_info_Category_input_ui"),
                            uiOutput("key_info_Entrepreneurs_input_ui"),
                            uiOutput("key_info_Season_input_ui"),
                            uiOutput("key_info_Episode_input_ui")),
                    
                    box(h3(tags$b("Ask For")), width = 3, h3(htmlOutput("key_info_text_output_askFor")), height = "300px"),
                    
                    box(h3(tags$b("Valuation")), width = 3, h3(htmlOutput("key_info_text_output_Valuation")), height = "300px",
                        background = "teal"),
                    
                    box(h3(tags$b("Exchange Stake")), width = 3, h3(htmlOutput("key_info_text_output_Stake")), height = "300px",
                        background = "lime")
                    
                    ##########################
                    
            ),
            
            #SECOND TAB CONTENT
            tabItem(tabName = "deal_insights_tab", 
                    fluidRow(
                    column(2,uiOutput("deal_insights_Location_input_ui")),
                    column(2,uiOutput("deal_insights_Category_input_ui")),
                    column(2,uiOutput("deal_insights_Entrepreneurs_input_ui")),
                    column(2,uiOutput("deal_insights_Season_input_ui")),
                    column(2,uiOutput("deal_insights_Episode_input_ui"))),
                    
                    fluidRow(
                    
                    tabsetPanel(
                      tabPanel("Deal-Season", 
                              box(h3("Shark Tank Deals: Season Breakdown"), width = 12, plotlyOutput("deal_insights_plot_season_deal"))), 
                      
                      tabPanel("Deal-Entrepreneurs", 
                              box(h3("Shark Tank Deals: Team Breakdown"), width = 12, plotlyOutput("deal_insights_plot_entrepreneurs_deal"))),
                      
                      tabPanel("Deal-Category", 
                              box(h3("Shark Tank Deals: Business Category Breakdown"), width = 12, plotlyOutput("deal_insights_plot_categories_deal"))), 
                      
                      tabPanel("Deal-Location", 
                              box(h3("Shark Tank Deals: Location Breakdown"), width = 12, plotlyOutput("deal_insights_plot_location_deal")))
                                
                    )
                    
                    
                    ##########################
                    
            ),
            
           
           fluidRow(box(                               ## Data table
            dataTableOutput("table",height = 320),width = 12))
          
            ),
            
            #THIRD TAB CONTENT
            tabItem(tabName = "deal_trends_tab", 
                    fluidRow(
                        column(3,uiOutput("deal_trends_Location_input_ui")),
                        column(3,uiOutput("deal_trends_Category_input_ui")),
                        column(3,uiOutput("deal_trends_Entrepreneurs_input_ui")),
                        column(3,uiOutput("deal_trends_Season_input_ui"))),
                    br(), 
                    fluidRow(
                        column(3,uiOutput("deal_trends_Episode_input_ui")),
                        column(3,uiOutput("deal_trends_Valuation_input_ui")),
                        column(3,uiOutput("deal_trends_Exchange_input_ui")),
                        column(3,uiOutput("deal_trends_askedFor_input_ui"))),
                    fluidRow(
                        box(width = 12, h3("Likelihood Of Getting Deal Throughout Episodes"), 
                            plotlyOutput("deal_trends_plot_season_episode_deal")),
                        box(width = 12, h3("Top 4 Business Categories Through Seasons - 10+ Deals"), 
                            plotlyOutput("deal_trends_plot_season_category_deal")),
                        box(width = 12, h3("Company Valuation Development Through Seasons"), 
                            plotlyOutput("deal_trends_plot_season_Valuation_deal")),
                        box(width = 12, h3("Exchange Stake Development Through Seasons"), 
                            plotlyOutput("deal_trends_plot_season_Exchange_deal")),
                        box(width = 12, h3("Asked For Development Through Seasons"), 
                            plotlyOutput("deal_trends_plot_season_AskedFor_deal"))
                    
                    ##########################
                    
                    )
            )
        )
    )
)

server <- function(input, output) {
    
    ####---------------------------------   KEY INFO   -----------------------------------------------####
    
    ## Location ##
    output$key_info_Location_input_ui <- renderUI({
        pickerInput("key_info_Location_input","Select Location", options = list(`actions-box` = TRUE), multiple = T,
                    choices = Shark_tank$location, selected = Shark_tank$location[1:3])
    })
    
    ## Category ##
    output$key_info_Category_input_ui <- renderUI({
        pickerInput("key_info_Category_input","Select Category", options = list(`actions-box` = TRUE), multiple = T,
                    choices = Shark_tank$category, selected = Shark_tank$category)
    })
    
    ## Entrepreneurs ##
    output$key_info_Entrepreneurs_input_ui <- renderUI({
        pickerInput("key_info_Entrepreneurs_input","Select If Multiple Entrepreneurs", options = list(`actions-box` = TRUE), multiple = T,
                    choices = c(TRUE,FALSE), selected = c(TRUE,FALSE))
    })
    
    ## Season ##
    output$key_info_Season_input_ui <- renderUI({
        pickerInput("key_info_Season_input","Select Season", options = list(`actions-box` = TRUE), multiple = T,
                    choices = levels(Shark_tank$season), selected = levels(Shark_tank$season))
    })
    
    ## Episode ##
    output$key_info_Episode_input_ui <- renderUI({
        pickerInput("key_info_Episode_input","Select Episode", options = list(`actions-box` = TRUE), multiple = T,
                    choices = levels(Shark_tank$episode), selected = levels(Shark_tank$episode) )
    })
    
    
    ## Asked For ##
    output$key_info_boxplot_ask_for =  renderPlotly({
        req(input$key_info_Location_input)
        req(input$key_info_Category_input)
        req(input$key_info_Entrepreneurs_input)
        req(input$key_info_Season_input)
        req(input$key_info_Episode_input)
        ggplot(data = Shark_tank[Shark_tank$location %in% input$key_info_Location_input & 
                                     Shark_tank$category %in% input$key_info_Category_input & 
                                     Shark_tank$Multiple.Entreprenuers %in% input$key_info_Entrepreneurs_input & 
                                     Shark_tank$season %in% input$key_info_Season_input & 
                                     Shark_tank$episode %in% input$key_info_Episode_input,])+
            geom_boxplot(aes(y = askedFor)) + labs( y = "Asked For $")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
    })
    
    output$key_info_text_output_askFor =  renderUI({
        req(input$key_info_Location_input)
        req(input$key_info_Category_input)
        req(input$key_info_Entrepreneurs_input)
        req(input$key_info_Season_input)
        req(input$key_info_Episode_input)
        temp = Shark_tank[Shark_tank$location %in% input$key_info_Location_input & 
                                     Shark_tank$category %in% input$key_info_Category_input & 
                                     Shark_tank$Multiple.Entreprenuers %in% input$key_info_Entrepreneurs_input & 
                                     Shark_tank$season %in% input$key_info_Season_input & 
                                     Shark_tank$episode %in% input$key_info_Episode_input,]
        if(NROW(temp) > 0){
        HTML(paste("Average: $", round(mean(temp$askedFor),2), "<br><br>", "Min: $", min(temp$askedFor), "<br><br>", "Max: $", max(temp$askedFor)) )
        }
        
        })
    
    
    ## Valuation ##
    output$key_info_boxplot_valuation =  renderPlotly({
        req(input$key_info_Location_input)
        req(input$key_info_Category_input)
        req(input$key_info_Entrepreneurs_input)
        req(input$key_info_Season_input)
        req(input$key_info_Episode_input)
        ggplot(data = Shark_tank[Shark_tank$location %in% input$key_info_Location_input & 
                                     Shark_tank$category %in% input$key_info_Category_input & 
                                     Shark_tank$Multiple.Entreprenuers %in% input$key_info_Entrepreneurs_input & 
                                     Shark_tank$season %in% input$key_info_Season_input & 
                                     Shark_tank$episode %in% input$key_info_Episode_input,])+
            geom_boxplot(aes(y = valuation)) + labs(y = "Valuation $")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
    })
    
    output$key_info_text_output_Valuation =  renderUI({
        req(input$key_info_Location_input)
        req(input$key_info_Category_input)
        req(input$key_info_Entrepreneurs_input)
        req(input$key_info_Season_input)
        req(input$key_info_Episode_input)
        temp = Shark_tank[Shark_tank$location %in% input$key_info_Location_input & 
                              Shark_tank$category %in% input$key_info_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$key_info_Entrepreneurs_input & 
                              Shark_tank$season %in% input$key_info_Season_input & 
                              Shark_tank$episode %in% input$key_info_Episode_input,]
        
        if(NROW(temp) > 0){
        HTML(paste("Average: $", round(mean(temp$valuation),2), "<br><br>", "Min: $", min(temp$valuation), "<br><br>", "Max: $", max(temp$valuation)) )
        }
        
    })
    
    ### Stake ##
    output$key_info_boxplot_stake =  renderPlotly({
        req(input$key_info_Location_input)
        req(input$key_info_Category_input)
        req(input$key_info_Entrepreneurs_input)
        req(input$key_info_Season_input)
        req(input$key_info_Episode_input)
        ggplot(data = Shark_tank[Shark_tank$location %in% input$key_info_Location_input & 
                                     Shark_tank$category %in% input$key_info_Category_input & 
                                     Shark_tank$Multiple.Entreprenuers %in% input$key_info_Entrepreneurs_input & 
                                     Shark_tank$season %in% input$key_info_Season_input & 
                                     Shark_tank$episode %in% input$key_info_Episode_input,])+
            geom_boxplot(aes(y = exchangeForStake)) + labs(y = "Stake %")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
    })
    
    output$key_info_text_output_Stake =  renderUI({
        req(input$key_info_Location_input)
        req(input$key_info_Category_input)
        req(input$key_info_Entrepreneurs_input)
        req(input$key_info_Season_input)
        req(input$key_info_Episode_input)
        temp = Shark_tank[Shark_tank$location %in% input$key_info_Location_input & 
                              Shark_tank$category %in% input$key_info_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$key_info_Entrepreneurs_input & 
                              Shark_tank$season %in% input$key_info_Season_input & 
                              Shark_tank$episode %in% input$key_info_Episode_input,]
        if(NROW(temp) > 0){
        
        HTML(paste("Average:", round(mean(temp$exchangeForStake),2), "%<br><br>", "Min:", min(temp$exchangeForStake), "%<br><br>", "Max:", max(temp$exchangeForStake) ,"%") )
        
        }
    })
    
    
    ####---------------------------------     DEAL INSIGHTS   -----------------------------------------------####
    
    ## Location ##
    output$deal_insights_Location_input_ui <- renderUI({
        pickerInput("deal_insights_Location_input","Select Location", options = list(`actions-box` = TRUE), multiple = T,
                    choices = Shark_tank$location, selected = Shark_tank$location)
    })
    
    ## Category ##
    output$deal_insights_Category_input_ui <- renderUI({
        pickerInput("deal_insights_Category_input","Select Category", options = list(`actions-box` = TRUE), multiple = T,
                    choices = Shark_tank$category, selected = Shark_tank$category)
    })
    
    ## Entrepreneurs ##
    output$deal_insights_Entrepreneurs_input_ui <- renderUI({
        pickerInput("deal_insights_Entrepreneurs_input","Select If Multiple Entrepreneurs", options = list(`actions-box` = TRUE), multiple = T,
                    choices = c(TRUE,FALSE), selected = c(TRUE,FALSE))
    })
    
    ## Season ##
    output$deal_insights_Season_input_ui <- renderUI({
        pickerInput("deal_insights_Season_input","Select Season", options = list(`actions-box` = TRUE), multiple = T,
                    choices = levels(Shark_tank$season), selected = levels(Shark_tank$season))
    })
    
    ## Episode ##
    output$deal_insights_Episode_input_ui <- renderUI({
        pickerInput("deal_insights_Episode_input","Select Episode", options = list(`actions-box` = TRUE), multiple = T,
                    choices = levels(Shark_tank$episode), selected = levels(Shark_tank$episode) )
    })
    
    
    ## Graph 1 seasaon - deal ##
    output$deal_insights_plot_season_deal =  renderPlotly({
        
        req(input$deal_insights_Location_input)
        req(input$deal_insights_Category_input)
        req(input$deal_insights_Entrepreneurs_input)
        req(input$deal_insights_Episode_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_insights_Location_input & 
                              Shark_tank$category %in% input$deal_insights_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_insights_Entrepreneurs_input &
                              Shark_tank$episode %in% input$deal_insights_Episode_input &
                              Shark_tank$deal == TRUE,]
        
        temp = data.frame(table(temp$season, temp$deal))[,c(1,3)]
        
        colnames(temp) = c("season", "deal")
        if(NROW(temp) > 0){
        ggplot(data = temp)+
            geom_bar(aes(x = season, y = deal), fill = "skyblue1", stat="identity",width=0.3) + labs(x = "Season", y = "# of Deals")+
            scale_fill_brewer(palette="Reds")
        }
    })
    
    
    ## Graph 2 Entrepreneurs - deal ##
    output$deal_insights_plot_entrepreneurs_deal =  renderPlotly({
        
        req(input$deal_insights_Location_input)
        req(input$deal_insights_Category_input)
        req(input$deal_insights_Season_input)
        req(input$deal_insights_Episode_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_insights_Location_input & 
                              Shark_tank$category %in% input$deal_insights_Category_input &
                              Shark_tank$season %in% input$deal_insights_Season_input & 
                              Shark_tank$episode %in% input$deal_insights_Episode_input,]
        
        temp = data.frame(prop.table(table(temp$deal, temp$Multiple.Entreprenuers)))[c(2,4),c(2,3)]
        
        colnames(temp) = c("Multiple_Entreprenuers", "Probability")
        
        if(NROW(temp) > 0){
        
        ggplot(data = temp)+
        geom_bar(aes(x = Multiple_Entreprenuers, y = Probability), fill = "turquoise4", stat="identity", width=0.3) + 
        labs(x = "Multiple Entreprenuers", y = "Probability of getting a deal")+
        scale_fill_brewer(palette="Reds")
            
          }
    })
    
    
    
    ## Graph 3 categories - deal ##
    output$deal_insights_plot_categories_deal =  renderPlotly({
        
        req(input$deal_insights_Location_input)
        req(input$deal_insights_Season_input)
        req(input$deal_insights_Entrepreneurs_input)
        req(input$deal_insights_Episode_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_insights_Location_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_insights_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_insights_Season_input & 
                              Shark_tank$episode %in% input$deal_insights_Episode_input &
                              Shark_tank$deal == TRUE,]
        
        temp = data.frame(table(temp$category, temp$deal))[,c(1,3)]
        
        colnames(temp) = c("category", "deal")
        
        temp$category = as.character(temp$category)
        
        temp = head(temp[order(temp$deal, decreasing= T),], n = 10)
        
        if(NROW(temp) > 0){
        ggplot(data = temp)+
            geom_bar(aes(x = category,y = deal, fill = category), width = 0.3, stat="identity") + 
            scale_fill_brewer(palette = "BrBG") +
            labs(x = "Category", y = "# of Deals")+
            theme(axis.text.x = element_text(angle = 90))+ 
                scale_x_discrete(label=function(x) abbreviate(x, minlength=10), limits = temp$category)
           }
        
    })
    
    
    
    ## Graph 4 location - deal ##
    output$deal_insights_plot_location_deal =  renderPlotly({
        
        req(input$deal_insights_Category_input)
        req(input$deal_insights_Season_input)
        req(input$deal_insights_Entrepreneurs_input)
        req(input$deal_insights_Episode_input)
        
        temp = Shark_tank[Shark_tank$category %in% input$deal_insights_Category_input &
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_insights_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_insights_Season_input & 
                              Shark_tank$episode %in% input$deal_insights_Episode_input &
                              Shark_tank$deal == TRUE,]
        
        temp = data.frame(table(temp$location, temp$deal))[,c(1,3)]
        
        colnames(temp) = c("location", "deal")
        
        temp$location = as.character(temp$location)
        
        temp = head(temp[order(temp$deal, decreasing= T),], n = 10)
        
        if(NROW(temp) > 0){
          ggplot(data = temp, aes(x=location, y=deal)) + 
            geom_point(color="skyblue1", size=5) + 
            geom_segment(aes(x=location, 
                             xend=location, 
                             y=0, 
                             yend=deal)) +
            labs(x = "Location of Business", y = "# of Deals")+
            scale_x_discrete(label=function(x) abbreviate(x, minlength=10), limits = temp$location)
          
        }
        
    })
    
    ## Data Table ##
    data <- reactive(
      Shark_tank%>%
        filter(location %in% input$deal_insights_Location_input) %>%
        filter(category %in% input$deal_insights_Category_input) %>%
        filter(Multiple.Entreprenuers %in% input$deal_insights_Entrepreneurs_input) %>%
        filter(episode %in% input$deal_insights_Episode_input))
    output$table <- renderDataTable(data())
    
    
    
    ####---------------------------------      DEAL TRENDS    -----------------------------------------------####
    
    ## Location ##
    output$deal_trends_Location_input_ui <- renderUI({
        pickerInput("deal_trends_Location_input","Select Location", options = list(`actions-box` = TRUE), multiple = T,
                    choices = Shark_tank$location, selected = Shark_tank$location)
    })
    
    ## Category ##
    output$deal_trends_Category_input_ui <- renderUI({
        pickerInput("deal_trends_Category_input","Select Category", options = list(`actions-box` = TRUE), multiple = T,
                    choices = Shark_tank$category, selected = Shark_tank$category)
    })
    
    ## Entrepreneurs ##
    output$deal_trends_Entrepreneurs_input_ui <- renderUI({
        pickerInput("deal_trends_Entrepreneurs_input","Select If Multiple Entrepreneurs", options = list(`actions-box` = TRUE), multiple = T,
                    choices = c(TRUE,FALSE), selected = c(TRUE,FALSE))
    })
    
    ## Season ##
    output$deal_trends_Season_input_ui <- renderUI({
        pickerInput("deal_trends_Season_input","Select Season", options = list(`actions-box` = TRUE), multiple = T,
                    choices = levels(Shark_tank$season), selected = levels(Shark_tank$season))
    })
    
    ## Episode ##
    output$deal_trends_Episode_input_ui <- renderUI({
        pickerInput("deal_trends_Episode_input","Select Episode", options = list(`actions-box` = TRUE), multiple = T,
                    choices = levels(Shark_tank$episode), selected = levels(Shark_tank$episode) )
    })
    
    ## Valuation ##
    output$deal_trends_Valuation_input_ui <- renderUI({
        sliderInput("deal_trends_Valuation_input", label = "Valuation $", min = min(Shark_tank$valuation), max = max(Shark_tank$valuation), 
                    value = c(min(Shark_tank$valuation), max(Shark_tank$valuation)))
    })
    
    ## Exchange ##
    output$deal_trends_Exchange_input_ui <- renderUI({
        sliderInput("deal_trends_Exchange_input", label = "Exchange %", 
                    min = min(Shark_tank$exchangeForStake), 
                    max = max(Shark_tank$exchangeForStake), 
                    value = c(min(Shark_tank$exchangeForStake), 
                              max(Shark_tank$exchangeForStake)))
    })
    
    ## Asked for ##
    output$deal_trends_askedFor_input_ui <- renderUI({
        sliderInput("deal_trends_askedFor_input", label = "Asked For $", 
                    min = min(Shark_tank$askedFor), 
                    max = max(Shark_tank$askedFor), 
                    value = c(min(Shark_tank$askedFor), 
                              max(Shark_tank$askedFor)))
    })
    
    
    ### Graph 5 season - episode - deal ###
    output$deal_trends_plot_season_episode_deal =  renderPlotly({
        
        req(input$deal_trends_Location_input)
        req(input$deal_trends_Category_input)
        req(input$deal_trends_Entrepreneurs_input)
        req(input$deal_trends_Season_input)
        req(input$deal_trends_Episode_input)
        req(input$deal_trends_Valuation_input)
        req(input$deal_trends_Exchange_input)
        req(input$deal_trends_askedFor_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_trends_Location_input & 
                              Shark_tank$category %in% input$deal_trends_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_trends_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_trends_Season_input &
                              Shark_tank$episode %in% input$deal_trends_Episode_input &
                              Shark_tank$valuation >= input$deal_trends_Valuation_input[1] &
                              Shark_tank$valuation <= input$deal_trends_Valuation_input[2] &
                              Shark_tank$exchangeForStake >= input$deal_trends_Exchange_input[1] &
                              Shark_tank$exchangeForStake <= input$deal_trends_Exchange_input[2] &
                              Shark_tank$askedFor >= input$deal_trends_askedFor_input[1] &
                              Shark_tank$askedFor <= input$deal_trends_askedFor_input[2],]
        
        temp = table(temp$season, temp$episode, temp$deal)
        
        temp = data.frame(temp[,,2] / (temp[,,1] + temp[,,2]))
        
        colnames(temp) = c("season", "episode", "Probability")
        
        
        
        if(NROW(temp) > 0){
        ggplot(data = temp, aes(x = episode , y = Probability, color = season, group = season))+
            geom_jitter(stat = "identity") + 
            theme_bw() +
            labs(x = "Episode", y = "Probability of getting a deal") + scale_fill_brewer(palette = "Paired")
            
        }
        
        
    })
    
    
    
    ### Graph 6 season - category - deal ###
    
    sumarized_sharktank <- reactive({
      Shark_tank %>% 
        filter(popularity > 10)%>%
        group_by(season, category) %>%
        summarise(count = n(),
                  deals = sum(deal),
                  success = deals/count) %>% 
        ungroup()
    })
    
      
    output$deal_trends_plot_season_category_deal =  renderPlotly({
        
        req(input$deal_trends_Location_input)
        req(input$deal_trends_Category_input)
        req(input$deal_trends_Entrepreneurs_input)
        req(input$deal_trends_Season_input)
        req(input$deal_trends_Episode_input)
        req(input$deal_trends_Valuation_input)
        req(input$deal_trends_Exchange_input)
        req(input$deal_trends_askedFor_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_trends_Location_input & 
                              Shark_tank$category %in% input$deal_trends_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_trends_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_trends_Season_input &
                              Shark_tank$episode %in% input$deal_trends_Episode_input &
                              Shark_tank$valuation >= input$deal_trends_Valuation_input[1] &
                              Shark_tank$valuation <= input$deal_trends_Valuation_input[2] &
                              Shark_tank$exchangeForStake >= input$deal_trends_Exchange_input[1] &
                              Shark_tank$exchangeForStake <= input$deal_trends_Exchange_input[2] &
                              Shark_tank$askedFor >= input$deal_trends_askedFor_input[1] &
                              Shark_tank$askedFor <= input$deal_trends_askedFor_input[2] &
                              Shark_tank$deal == TRUE,]
        
        temp = data.frame(table(temp$season, temp$category))
        
        colnames(temp) = c("season", "category", "Deals")
        
        if(NROW(temp) > 0){
    
          p <- ggplot(sumarized_sharktank(),
                      aes_string(x = sumarized_sharktank()$season,
                                y = sumarized_sharktank()$success,
                                color = as.factor(sumarized_sharktank()$category),
                                group = as.factor(sumarized_sharktank()$category)
          )) +
            geom_point()+
            scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#C70039")) +
            geom_line() +
            theme_bw() +
            labs(x = "Season", y = "# of deals")
          
          ggplotly(p)
          
          
             }
        
        
    })
    
    
    
    
    ### Graph 7 Deal vs Season vs Valuation vs Exchange vs Asked For ###
    
    ## Graph valuation
    
    output$deal_trends_plot_season_Valuation_deal =  renderPlotly({
        
        req(input$deal_trends_Location_input)
        req(input$deal_trends_Category_input)
        req(input$deal_trends_Entrepreneurs_input)
        req(input$deal_trends_Season_input)
        req(input$deal_trends_Episode_input)
        req(input$deal_trends_Valuation_input)
        req(input$deal_trends_Exchange_input)
        req(input$deal_trends_askedFor_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_trends_Location_input & 
                              Shark_tank$category %in% input$deal_trends_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_trends_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_trends_Season_input &
                              Shark_tank$episode %in% input$deal_trends_Episode_input &
                              Shark_tank$valuation >= input$deal_trends_Valuation_input[1] &
                              Shark_tank$valuation <= input$deal_trends_Valuation_input[2] &
                              Shark_tank$exchangeForStake >= input$deal_trends_Exchange_input[1] &
                              Shark_tank$exchangeForStake <= input$deal_trends_Exchange_input[2] &
                              Shark_tank$askedFor >= input$deal_trends_askedFor_input[1] &
                              Shark_tank$askedFor <= input$deal_trends_askedFor_input[2] &
                              Shark_tank$deal == TRUE,]
        
        if(NROW(temp) > 0){
        ggplot(data = temp)+
            geom_boxplot(aes(x = season , y = valuation), color="cyan3", fill="cyan3", alpha=0.1)+ 
           
            labs(x = "Season", y = "Deal Valuation $") 
        }
        
        
    })
    
    ## Graph AskedFor
    
    output$deal_trends_plot_season_AskedFor_deal =  renderPlotly({
        
        req(input$deal_trends_Location_input)
        req(input$deal_trends_Category_input)
        req(input$deal_trends_Entrepreneurs_input)
        req(input$deal_trends_Season_input)
        req(input$deal_trends_Episode_input)
        req(input$deal_trends_Valuation_input)
        req(input$deal_trends_Exchange_input)
        req(input$deal_trends_askedFor_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_trends_Location_input & 
                              Shark_tank$category %in% input$deal_trends_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_trends_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_trends_Season_input &
                              Shark_tank$episode %in% input$deal_trends_Episode_input &
                              Shark_tank$valuation >= input$deal_trends_Valuation_input[1] &
                              Shark_tank$valuation <= input$deal_trends_Valuation_input[2] &
                              Shark_tank$exchangeForStake >= input$deal_trends_Exchange_input[1] &
                              Shark_tank$exchangeForStake <= input$deal_trends_Exchange_input[2] &
                              Shark_tank$askedFor >= input$deal_trends_askedFor_input[1] &
                              Shark_tank$askedFor <= input$deal_trends_askedFor_input[2] &
                              Shark_tank$deal == TRUE,]
        
        if(NROW(temp) > 0){
        ggplot(data = temp)+
            geom_boxplot(aes(x = season , y = askedFor), color="cyan3", fill="cyan3", alpha=0.1) +  
            labs(x = "Season", y = "Deal Asked For $")
        }
        
        
    })
    
    
    # Graph Exchange
    output$deal_trends_plot_season_Exchange_deal =  renderPlotly({
        
        req(input$deal_trends_Location_input)
        req(input$deal_trends_Category_input)
        req(input$deal_trends_Entrepreneurs_input)
        req(input$deal_trends_Season_input)
        req(input$deal_trends_Episode_input)
        req(input$deal_trends_Valuation_input)
        req(input$deal_trends_Exchange_input)
        req(input$deal_trends_askedFor_input)
        
        temp = Shark_tank[Shark_tank$location %in% input$deal_trends_Location_input & 
                              Shark_tank$category %in% input$deal_trends_Category_input & 
                              Shark_tank$Multiple.Entreprenuers %in% input$deal_trends_Entrepreneurs_input &
                              Shark_tank$season %in% input$deal_trends_Season_input &
                              Shark_tank$episode %in% input$deal_trends_Episode_input &
                              Shark_tank$valuation >= input$deal_trends_Valuation_input[1] &
                              Shark_tank$valuation <= input$deal_trends_Valuation_input[2] &
                              Shark_tank$exchangeForStake >= input$deal_trends_Exchange_input[1] &
                              Shark_tank$exchangeForStake <= input$deal_trends_Exchange_input[2] &
                              Shark_tank$askedFor >= input$deal_trends_askedFor_input[1] &
                              Shark_tank$askedFor <= input$deal_trends_askedFor_input[2] &
                              Shark_tank$deal == TRUE,]
        
        if(NROW(temp) > 0){
        ggplot(data = temp)+
            geom_boxplot(aes(x = season , y = exchangeForStake), color="cyan3", fill="cyan3", alpha=0.1) + 
            labs(x = "Season", y = "Deal Exchange %")
        }
        
        
    })
    
    
    
}









shinyApp(ui, server)



