#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(scales)
library(ggpmisc)
library(plotrix)
library(data.table)
library(dplyr)
library(Hmisc)
library(tidyverse)
library(ggbeeswarm)
library(reshape2)
library(tibble)
library(fmsb)
library(plotly)

mstats<-read.csv("match_data_v2.csv")
evt_type_data <- read.csv("event_type_data.csv")
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
    if ((mstats$Timing[row]>=46) & (mstats$Timing[row]<=60)) {
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

play_data <- subset(mstats,event_player!="NONE")

play_pivot <- data.frame(table(play_data$event_player,play_data$H_A,play_data$event_type))
play_pivot <- subset(play_pivot, substr(play_pivot$Var1,start=1,stop=6)!="SECOND")
play_pivot2 <- data.frame(table(play_data$event_player,play_data$event_type))
# play_pivot <- subset(play_pivot,Freq>0)

# Define UI for application that draws a histogram
ui <- dashboardPage(title = "Northampton Town FC - Season 2017/18",
      
        dashboardHeader(title = "Northampton Town FC: Season 2017/18", titleWidth = 408),

        dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction / Info", tabName = "info1", icon = icon("info")),
                menuItem("Season Summary", tabName = "summary1", icon = icon("bar-chart")),
                menuItem("Event Analysis", tabName = "evt_summary", icon = icon("bar-chart")),
                menuItem("Player Stats", tabName = "pstats", icon = icon("bar-chart")),
                menuItem("Data Sheet", tabName = "dsheet", icon = icon("th"))
            )),
        
        dashboardBody(
            tabItems(
                # Info tab content
                tabItem(tabName = "info1",
                        fluidRow(
                            titlePanel("Northampton Town FC - Analysis of 2017/18 League One Season "),
#                            sidebarLayout(
#                                sidebarPanel(),
                                mainPanel(
                                    h3("Introduction"),
                                    h5("The app will analyse match event data, scraped from the Sporting Life website"),
                                    h5("Below are a list of match event types and their descriptions"),
                                    h5(" "),
                                    h4("Event Types"),
                                    tableOutput('tbl')
                                )
                            )
                        #    )
                       # )
                ),
                # First tab content
                tabItem(tabName = "summary1",
                        fluidRow(
                                 column(width = 4,
                                             selectInput("pickVenue",
                                                         "Home or Away :",
                                                         sort(c("BOTH",
                                                                unique(as.character(mstats$H_A)))),selected="BOTH")
                             )),
                        fluidRow(
                            box(
                                # title = "Match Events - When and how many ?", width = 12,
                                solidHeader = TRUE, status = "primary", width = 12,
                                plotOutput("hm_events"),
                                plotOutput("barevents"))
                            )
                             ),
                # Second tab content
                tabItem(tabName = "evt_summary",  
                        fluidRow(
                                     column(width = 6,
                                            selectInput("pickVenue2",
                                                        "Home or Away :",
                                                        sort(c("BOTH", 
                                                               unique(as.character(mstats$H_A)))),selected="BOTH")),
                                     column(width = 6, size=4,
                                            selectInput("pickEvent2",
                                                    "Event Type :",
                                                    sort(c(unique(as.character(mstats$event_type)))),selected="CORNER COBBLERS"))
                             ),
                        fluidRow(),
                        fluidRow(
                             plotlyOutput("bar_evtd"),
                             plotlyOutput("bees_plot"))
                    ),
                # Third tab content
                tabItem(tabName = "pstats", 
                             # Create a new row for the table.
                        fluidRow(
#                                 column(width = 4,
#                                                    "Venue :",
#                                        selectInput("pickHorA",
#                                                           unique(as.character(play_pivot$Var2)))),selected="Both")
#                                                    sort(c("Both",
#                                 ),
                                 column(width = 4,
                                        selectInput("pickPlayer",
                                                    "Player Name :",
                                                    sort(c(unique(as.character(play_pivot$Var1)))),selected="ASH TAYLOR")
                                 ),
                                 column(width = 4,
                                        selectInput("pickEvtPl",
                                                    "Event Type :",
                                                    sort(c("All",
                                                           unique(as.character(play_data$event_type)))),selected="GOAL COBBLERS")
                                 )
                                 ),
                        fluidRow(
                                 tabBox(
                                     title = h4("Player Stats"),
                                     # The id lets us use input$tabset1 on the server to find the current tab
                                     id = "tabset1", height = "200px",
                                     tabPanel("ALL MATCHES",
                                              fluidRow(
                                                  valueBoxOutput("statsBBox",width = 3),
                                                  valueBoxOutput("statsBBox3",width = 3),
                                                  valueBoxOutput("statsBBox4",width = 3),
                                                  valueBoxOutput("statsBBox6",width = 3)
                                              ),
                                              fluidRow(
                                                  valueBoxOutput("statsBBox5",width = 3),
                                                  valueBoxOutput("statsBBox2",width = 3),
                                                  valueBoxOutput("statsBBox7",width = 3),
                                                  valueBoxOutput("statsBBox8",width = 3)
                                              )
                                     ),
                                     tabPanel("HOME MATCHES",
                                              fluidRow(
                                              valueBoxOutput("statsBox",width = 3),
                                              valueBoxOutput("statsBox3",width = 3),
                                              valueBoxOutput("statsBox4",width = 3),
                                              valueBoxOutput("statsBox6",width = 3)
                                              ),
                                              fluidRow(
                                              valueBoxOutput("statsBox5",width = 3),
                                              valueBoxOutput("statsBox2",width = 3),
                                              valueBoxOutput("statsBox7",width = 3),
                                              valueBoxOutput("statsBox8",width = 3)
                                              )
                                     ),
                                     tabPanel("AWAY MATCHES",
                                              fluidRow(
                                              valueBoxOutput("statsABox",width = 3),
                                              valueBoxOutput("statsABox3",width = 3),
                                              valueBoxOutput("statsABox4",width = 3),
                                              valueBoxOutput("statsABox6",width = 3)
                                              ),
                                              fluidRow(
                                              valueBoxOutput("statsABox5",width = 3),
                                              valueBoxOutput("statsABox2",width = 3),
                                              valueBoxOutput("statsABox7",width = 3),
                                              valueBoxOutput("statsABox8",width = 3)
                                              )
                                     )
                                 ),
                                 tabBox(
                                 box(title = "Event / Player link : Frequencies ",
                                     solidHeader = FALSE, status = "primary", width = 12,
                                     plotlyOutput("barPevt")
                                         )
                                 )
                             )
                    ),
                # Fourth tab content
                tabItem(tabName = "dsheet", 
                        fluidPage(
                                 titlePanel("Basic DataTable"),
                                 
                                 # Create a new Row in the UI for selectInputs
                                 fluidRow(
                                     column(4,
                                            selectInput("mid",
                                                        "Match ID:",
                                                        c("All",
                                                          unique(as.character(mstats$match_id))))
                                     ),
                                     column(4,
                                            selectInput("evt",
                                                        "Event Type:",
                                                        c("All",
                                                          unique(as.character(mstats$event_type))))
                                     ),
                                     column(4,
                                            selectInput("mdate",
                                                        "Date of Match:",
                                                        c("All",
                                                          unique(as.character(mstats$match_date))))
                                     )),
                                 # Create a new row for the table.
                        fluidRow(
                                     DT::dataTableOutput("mstats_tab")
                                 )
                             )
                    )
                )
            )
,skin = "red")

# function to use in the boxes section on the Player Stats Screen
# called when working out stats for a player
box_func <- function(Var2Venue,PickPlayer) {
    if (Var2Venue=="BOTH") {
        # create data frames for players
        md_p <- subset(play_pivot2, Var1==PickPlayer)
        # reshape long to wide
        md_p <- reshape(md_p, idvar = "Var1", timevar = "Var2", direction = "wide")
    }
    else {
        # create data frames for players
        md_p <- subset(play_pivot, Var1==PickPlayer & Var2==Var2Venue)
        # drop the Var2 column, home or away 
        md_p <- subset(md_p, select = -c(Var2))
        # reshape long to wide
        md_p <- reshape(md_p, idvar = "Var1", timevar = "Var3", direction = "wide")
    }
        return(md_p)
}

server <- function(input, output) {

    output$tbl <- renderTable({ evt_type_data })
    
    output$statsBBox <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.CORNER OPPOSITION'], "CORNERS CONCEEDED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox2 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.GOAL ATTEMPT COBBLERS'], "GOAL ATTEMPTS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox3 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.FOUL COBBLERS'], "FOULS CONCEEDED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox4 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        fk_tot<-as.integer(mdata_p['Freq.FREE KICK COBBLERS - ATTACKING HALF'])+
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - DEFENSIVE HALF']) +
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - RIGHT WING']) +
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - LEFT WING'])
        valueBox(
            fk_tot, "FREE KICKS GAINED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox5 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.GOAL COBBLERS'], "GOALS SCORED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox6 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.INJURY COBBLERS'], "INJURIES", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox7 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.YELLOW CARD COBBLERS'], "YELLOW CARDS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBBox8 <- renderValueBox({
        mdata_p <- box_func("BOTH",input$pickPlayer)
        valueBox(
            mdata_p['Freq.RED CARD COBBLERS'], "RED CARDS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.CORNER OPPOSITION'], "CORNERS CONCEEDED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox2 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.GOAL ATTEMPT COBBLERS'], "GOAL ATTEMPTS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox3 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.FOUL COBBLERS'], "FOULS CONCEEDED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox4 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        fk_tot<-as.integer(mdata_p['Freq.FREE KICK COBBLERS - ATTACKING HALF'])+
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - DEFENSIVE HALF']) +
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - RIGHT WING']) +
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - LEFT WING'])
        valueBox(
            fk_tot, "FREE KICKS GAINED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox5 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.GOAL COBBLERS'], "GOALS SCORED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox6 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.INJURY COBBLERS'], "INJURIES", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox7 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.YELLOW CARD COBBLERS'], "YELLOW CARDS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsBox8 <- renderValueBox({
        mdata_p <- box_func("HOME",input$pickPlayer)
        valueBox(
            mdata_p['Freq.RED CARD COBBLERS'], "RED CARDS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.CORNER OPPOSITION'], "CORNERS CONCEEDED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox2 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.GOAL ATTEMPT COBBLERS'], "GOAL ATTEMPTS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox3 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.FOUL COBBLERS'], "FOULS CONCEEDED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox4 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        fk_tot<-as.integer(mdata_p['Freq.FREE KICK COBBLERS - ATTACKING HALF'])+
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - DEFENSIVE HALF']) +
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - RIGHT WING']) +
            as.integer(mdata_p['Freq.FREE KICK COBBLERS - LEFT WING'])
        valueBox(
            fk_tot, "FREE KICKS GAINED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox5 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.GOAL COBBLERS'], "GOALS SCORED", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox6 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.INJURY COBBLERS'], "INJURIES", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox7 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.YELLOW CARD COBBLERS'], "YELLOW CARDS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$statsABox8 <- renderValueBox({
        mdata_p <- box_func("AWAY",input$pickPlayer)
        valueBox(
            mdata_p['Freq.RED CARD COBBLERS'], "RED CARDS", icon = icon("bar-chart"),
            color = "red"
        )
    })
    
    output$barPevt <- renderPlotly({
        pdata <- subset(play_pivot2, Var2==input$pickEvtPl & Freq>0)
        # rename columns to more meaningful names
        names(pdata)[1]<-"Player"
        names(pdata)[3]<-"Frequency"
        ggplot(data = pdata, aes(x = Player, y = Frequency)) + 
            geom_bar(stat = "identity", color = "#7c2e35", fill = "yellow") + 
            # ggtitle("Season 2017/18 : Event Type Frequency - Per Player") +
            ylab("Frequency") + xlab("Player") +
        theme(plot.title = element_text(color="#7c2e35", size=12, face="bold.italic"),
                  axis.text.x = element_text(face="bold", color="black", 
                                             size=6, angle=90),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=7))
    })
    
    output$hm_events <- renderPlot({
        hm1 <- data.frame(table(mstats$event_type, mstats$H_A, mstats$min_range))
        hm1 <- subset(hm1, Var1!="NONE")
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
            ggtitle("Heat Map : Event frequency during a match") +
            xlab("Period of Match") + ylab("Event Type") +
            # theme(axis.title.y = element_blank())
        theme(plot.title = element_text(color="#7c2e35", size=18, face="bold.italic"),
                           axis.text.x = element_text(face="bold", color="black", 
                                                      size=12),
                           axis.text.y = element_text(face="bold", color="black", 
                                                      size=10),
                           legend.position = "none")
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
        p + theme(plot.title = element_text(color="#7c2e35", size=18, face="bold.italic"),
                  axis.text.x = element_text(face="bold", color="black", 
                                             size=9, angle=90),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=9),
                  legend.position = "none")
    })
    
    output$bar_evtd <- renderPlotly({
        if (input$pickVenue2=="HOME") {
            mdata <- subset(evts_date_h, Var2==input$pickEvent2)
        }
        if (input$pickVenue2=="AWAY") {
            mdata <- subset(evts_date_a, Var2==input$pickEvent2)
        }
        if (input$pickVenue2=="BOTH") {
            mdata <- subset(evts_date, Var2==input$pickEvent2)
        }
        # rename columns to more meaningful names
        names(mdata)[1]<-"MatchDate"
        names(mdata)[3]<-"Frequency"
        p <- ggplot(data = mdata, aes(x = MatchDate, y = Frequency, group = 1)) + 
            geom_line() + 
            ggtitle("Season View : Event Type Frequency (per match)") +
            ylab("Frequency") + xlab("Match Date") +
            theme(plot.title = element_text(color="#7c2e35", size=14, face="bold.italic", hjust=0),
                  axis.text.x = element_text(face="bold", color="black", 
                                             size=6, angle=90),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=8))
    })

    output$bees_plot <- renderPlotly({
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
            ggtitle("Swarm Plot - Occurrences of event in matches") + 
            scale_x_discrete(limits=c(0,15,30,45,60,75,90))
        p + theme(
            plot.title = element_text(color="#7c2e35", size=14, face="bold.italic"),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold", angle=180)
        )
    })
    
    output$playevts <- renderPlot({
        if (input$pickHorA=="HOME") {
            if (input$pickEvtPl=="All") {
                mdata <- subset(play_pivot, Var2=="HOME")
            }
            else {
                mdata <- subset(play_pivot, Var2=="HOME" & Var3==input$pickEvtPl)
            }
        }
        if (input$pickHorA=="AWAY") {
            if (input$pickEvtPl=="All") {
                mdata <- subset(play_pivot, Var2=="AWAY")
            }
            else {
                mdata <- subset(play_pivot, Var2=="AWAY" & Var3==input$pickEvtPl)
            }
        }
        if (input$pickHorA=="Both") {
            if (input$pickEvtPl=="All") {
                mdata <- play_pivot
            }
            else {
                mdata <- subset(play_pivot, Var3==input$pickEvtPl)
            }
        }
        p <- ggplot(mdata, aes(x = Var1, y = Freq, group = 1)) + 
            geom_bar() +
            ggtitle("Season 2017/18 : Types of Event (Total Number for Season)") +
            xlab("Frequency") + ylab("Event Type") +
            coord_flip()
        p + theme(plot.title = element_text(color="#7c2e35", size=18, face="bold.italic"),
                  axis.text.x = element_text(face="bold", color="black", 
                                             size=9, angle=90),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=9),
                  legend.position = "none")
    })
    
    # Filter data based on selections
    output$mstats_tab <- DT::renderDataTable(DT::datatable({
        data <- mstats
        if (input$mid != "All") {
            data <- mstats[mstats$match_id == input$mid,]
        }
        if (input$evt != "All") {
            data <- mstats[mstats$event_type == input$evt,]
        }
        if (input$mdate != "All") {
            data <- mstats[mstats$match_date == input$mdate,]
        }
        data
    }))
    
}

# Run the application 
shinyApp(ui = ui, server = server)

