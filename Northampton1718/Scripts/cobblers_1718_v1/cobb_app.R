#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(ggpmisc)
library(plotrix)
library(data.table)
library(dplyr)
library(Hmisc)
library(tidyverse)
library(ggbeeswarm)

mstats<-read.csv("match_data_v2.csv")
# remove rows of dataframe where event type = NONE
mstats<- subset(mstats, event_type!="NONE")
# strip out whitespace from event_player field
mstats$event_player <- trimws(mstats$event_player)
# set timing field as integer
mstats$Timing <- as.integer(mstats$Timing)
mstats$added_time <- as.integer(mstats$added_time)
# set match_date as date field
mstats$match_date <- as.Date(mstats$match_date, format = '%d-%m-%Y')

# create and populate the min_range field, for minute of match range 
# initialise new field, min_range
mstats$min_range <- ""
for (row in 1:nrow(mstats)) {
    if ((mstats$Timing[row]>0) & (mstats$Timing[row]<=15)) {
        mstats$min_range[row]<-c("1 - 15 mins")
    }
    if ((mstats$Timing[row]>15) & (mstats$Timing[row]<=30)) {
        mstats$min_range[row]<-c("16 - 30 mins")
    }
    if ((mstats$Timing[row]>30) & (mstats$Timing[row]<=45)) {
        mstats$min_range[row]<-c("31 - 45 mins")
        if ((mstats$added_time[row]>0) & (mstats$Timing[row]==45)) {
            mstats$Timing[row]<-as.numeric(45.5)
            mstats$min_range[row]<-c("45+")
        }
    }
    if ((mstats$Timing[row]>45) & (mstats$Timing[row]<=60)) {
        mstats$min_range[row]<-c("46 - 60 mins")
    }
    if ((mstats$Timing[row]>60) & (mstats$Timing[row]<=75)) {
        mstats$min_range[row]<-c("61 - 75 mins")
    }
    if ((mstats$Timing[row]>75) & (mstats$Timing[row]<=90)) {
        mstats$min_range[row]<-c("76 - 90 mins")
        if ((mstats$added_time[row]>0) & (mstats$Timing[row]==90)) {
            mstats$Timing[row]<-as.integer(91)
            mstats$min_range[row]<-c("90+")
        }
    }
}

home_matches <- subset(mstats,H_A=="HOME")
away_matches <- subset(mstats,H_A=="AWAY")

evts_date_h <- data.frame(table(home_matches$match_date, home_matches$event_type))
evts_date_a <- data.frame(table(away_matches$match_date, away_matches$event_type))
evts_date <- data.frame(table(mstats$match_date, mstats$event_type))

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("united"),titlePanel("Northampton Town : Season 2017/18"),
                tabsetPanel(
                    tabPanel("Season Summary",
                             # Create a new row for the table.
                             sidebarPanel(
                             fluidRow(
                                 column(width = 4,
                                             selectInput("pickVenue",
                                                         "Home or Away :",
                                                         sort(c("BOTH",
                                                                unique(as.character(mstats$H_A)))),selected="GOAL COBBLERS")
                             )) ),
                             mainPanel(
                                plotOutput("hm_events"),
                                plotOutput("barevents"))
                             ),
                    tabPanel("Event Analysis",
                             sidebarPanel(
                             fluidRow(
                                     column(width = 6,
                                            selectInput("pickVenue2",
                                                        "Home or Away :",
                                                        sort(c("BOTH", 
                                                               unique(as.character(mstats$H_A)))),selected="GOAL COBBLERS")
                                     )),
                             fluidRow(
                                 column(width = 12, size=4,
                                        radioButtons("pickEvent2",
                                                    "Event Type :",
                                                    sort(c(unique(as.character(mstats$event_type)))),selected="CORNER COBBLERS")
                                 ))
                             ),
                             mainPanel(
                             plotOutput("bar_evtd"),
                             plotOutput("bees_plot"))
                    )
                ))

server <- function(input, output) {

    output$hm_events <- renderPlot({
        hm1 <- data.frame(table(mstats$event_type, mstats$H_A, mstats$min_range))
        if (input$pickVenue=="HOME") {
            mdata <- subset(hm1,Var2=="HOME")
        }
        if (input$pickVenue=="AWAY") {
            mdata <- subset(hm1,Var2=="AWAY")
        }
        if (input$pickVenue=="BOTH") {
            mdata <- hm1
        }
        ggplot(mdata, aes(x = Var3, y = Var1)) + geom_tile(aes(fill = Freq)) +
            scale_fill_gradient2(low = "white", 
                                 high = "red", 
                                 mid = "light blue",
                                 midpoint = 30) + 
            ggtitle("Event Types - Occurrence during matches") +
            xlab("Period of Match") + ylab("Event Type") +
            # theme(axis.title.y = element_blank())
        theme(plot.title = element_text(color="blue", size=18, face="bold.italic"),
                           axis.text.x = element_text(face="bold", color="black", 
                                                      size=12),
                           axis.text.y = element_text(face="bold", color="black", 
                                                      size=10),
                           legend.position = "none")
    })
    
    output$hm2 <- renderPlot({
    })
    
    
    output$barevents <- renderPlot({
        if (input$pickVenue=="HOME") {
            mdata <- subset(home_matches, H_A=="HOME")
        }
        if (input$pickVenue=="AWAY") {
            mdata <- subset(away_matches, H_A=="AWAY")
        }
        if (input$pickVenue=="BOTH") {
            mdata <- mstats
        }
        p <- ggplot(mdata, aes(x = fct_infreq(mdata$event_type),
                               fill=mdata$event_type)) + 
            geom_bar(stat = "count") +
            ggtitle("Season 2017/18 : Types of Event (Total Number for Season)") +
            xlab("Frequency") + ylab("Event Type") +
            coord_flip()
        p + theme(plot.title = element_text(color="blue", size=18, face="bold.italic"),
                  axis.text.x = element_text(face="bold", color="black", 
                                             size=9, angle=90),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=9),
                  legend.position = "none")
    })
    
    output$bar_evtd <- renderPlot({
        if (input$pickVenue2=="HOME") {
            mdata <- subset(evts_date_h, Var2==input$pickEvent2)
        }
        if (input$pickVenue2=="AWAY") {
            mdata <- subset(evts_date_a, Var2==input$pickEvent2)
        }
        if (input$pickVenue2=="BOTH") {
            mdata <- subset(evts_date, Var2==input$pickEvent2)
        }
        p <- ggplot(data = mdata, aes(x = Var1, y = Freq, group = 1)) + 
            geom_line() + 
            ggtitle("Season 2017/18 : Event Type Frequency (per match)") +
            ylab("Frequency") + xlab("Match Date")
        p + theme(plot.title = element_text(color="blue", size=18, face="bold.italic"),
                  axis.text.x = element_text(face="bold", color="black", 
                                             size=9, angle=90),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=9))
    })
    
    output$bees_plot <- renderPlot({
        if (input$pickVenue2=="HOME") {
            mdata <- subset(mstats, event_type==input$pickEvent2 & H_A=="HOME")
        }
        if (input$pickVenue2=="AWAY") {
            mdata <- subset(mstats, event_type==input$pickEvent2 & H_A=="AWAY")
        }
        if (input$pickVenue2=="BOTH") {
            mdata <- subset(mstats, event_type==input$pickEvent2)
        }
        p <- ggplot(mdata,aes(Timing, event_type, col=H_A)) + geom_beeswarm(groupOnX=FALSE,
                                                                            dodge.width=1,
                                                                            cex=1.5) + 
            ylab("Event Type") + xlab("Minute of Match") + 
            ggtitle("Swarm Plot - Event Occurrences during matches") + 
            scale_x_discrete(limits=c(0,15,30,45,60,75,90))
        p + theme(
            plot.title = element_text(color="blue", size=18, face="bold.italic"),
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold")
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

