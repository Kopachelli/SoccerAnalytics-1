#Loading the R packages
library('rvest')
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(stringr)

# create function to classify events in a match
evt_classify <- function(evt) {
    if (grepl("FOUL BY", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        return(c("FOUL COBBLERS"))
    } else
        if (grepl("FOUL BY", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
            return(c("FOUL OPPOSITION"))
        } else
            if (grepl("CORNER", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) &
                !(grepl("GOAL", evt, fixed = TRUE)) & !(grepl("ATTEMPT", evt, fixed = TRUE))) {
                return(c("CORNER COBBLERS"))
            } else
                if (grepl("CORNER", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) &
                    !(grepl("GOAL", evt, fixed = TRUE)) & !(grepl("ATTEMPT", evt, fixed = TRUE))) {
                    return(c("CORNER OPPOSITION"))
                } else
                    if (grepl("WINS A FREE KICK", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                        return(c("FREE KICK COBBLERS"))
                    } else
                        if (grepl("WINS A FREE KICK", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                            return(c("FREE KICK OPPOSITION"))
                        } else
                            if ((grepl("ATTEMPT", evt, fixed = TRUE) | grepl("HITS THE", evt, fixed = TRUE))
                                 & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) & !(grepl("GOAL!", evt, fixed = TRUE))) {
                                return(c("GOAL ATTEMPT COBBLERS"))
                            } else
                                if ((grepl("ATTEMPT", evt, fixed = TRUE) | grepl("HITS THE", evt, fixed = TRUE))
                                    & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) & !(grepl("GOAL!", evt, fixed = TRUE))) {
                                    return(c("GOAL ATTEMPT OPPOSITION"))
                                } else
                                    if (grepl("SUBSTITUTION", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                                        return(c("SUBSTITUTION COBBLERS"))
                                    } else
                                        if (grepl("SUBSTITUTION", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                                            return(c("SUBSTITUTION OPPOSITION"))
                                        } else
                                            if (grepl("GOAL!", evt, fixed = TRUE) & (grepl("(NORTHAMPTON TOWN)", evt, fixed = TRUE))) {
                                                return(c("GOAL COBBLERS"))
                                            } else
                                                if (grepl("GOAL!", evt, fixed = TRUE) & !(grepl("(NORTHAMPTON TOWN)", evt, fixed = TRUE))) {
                                                    return(c("GOAL OPPOSITION"))
                                                }
                                                    if (grepl("PENALTY", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) &
                                                        !(grepl("CONCEDED", evt, fixed = TRUE))) {
                                                        return(c("PENALTY COBBLERS"))
                                                        } else
                                                            if (grepl("PENALTY", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) &
                                                                    !(grepl("CONCEDED", evt, fixed = TRUE))) {
                                                                    return(c("PENALTY OPPOSITION"))
                                                                }
                                                                    if (grepl("PENALTY CONCEDED BY", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                                                                        return(c("FOUL COBBLERS"))
                                                                        } else
                                                                            if (grepl("PENALTY CONCEDED BY", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                                                                                return(c("FOUL OPPOSITION"))
                                                                            }
                                                                            if (grepl("YELLOW CARD", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                                                                                return(c("YELLOW CARD COBBLERS"))
                                                                                } else
                                                                                if (grepl("YELLOW CARD", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
                                                                                    return(c("YELLOW CARD OPPOSITION"))
                                                                                }
    if (grepl("RED CARD", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        return(c("RED CARD COBBLERS"))
    } else
        if (grepl("RED CARD", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
            return(c("RED CARD OPPOSITION"))
        }
    if (grepl("INJURY", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        return(c("INJURY COBBLERS"))
    } else
        if (grepl("INJURY", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
            return(c("INJURY OPPOSITION"))
        }
    if (grepl("HAND BALL", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        return(c("HANDBALL COBBLERS"))
    } else
        if (grepl("HAND BALL", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
            return(c("HANDBALL OPPOSITION"))
        }
    else {
        return(c("NONE"))
    }
}

player_classify <- function(evt) {
    if (grepl("FOUL BY", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        tmpstr<-gsub("FOUL BY", "", evt)
        tmpstr<-gsub("NORTHAMPTON TOWN", "", tmpstr)
        tmpstr<-gsub("[().]", "", tmpstr)
        tmpstr<-trimws(tmpstr)
        return(tmpstr)
    }
    if (grepl("INJURY", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) &
       !(grepl("SUBSTITUTION", evt, fixed = TRUE))) {
        tmpstr<-gsub("DELAY IN MATCH ", "", evt)
        tmpstr<-gsub("NORTHAMPTON TOWN", "", tmpstr)
        tmpstr<-gsub("BECAUSE OF AN INJURY", "", tmpstr)
        tmpstr<-gsub("[().]", "", tmpstr)
        tmpstr<-trimws(tmpstr)
        return(tmpstr)
    }
    if (grepl("GOAL!", evt, fixed = TRUE) & (grepl("(NORTHAMPTON TOWN)", evt, fixed = TRUE))) {
        # find position in evt string, where the pattern (NORTHAMPTON TOWN) starts and ends
        # store it in pos1, and can then reference it, using pos1[,"start"]
        pos1<-str_locate(pattern="\\(NORTHAMPTON TOWN\\)",evt)
        # get the all characters in evt, from character 1, to the pos1 start position-2
        tmp_string<-substr(evt,1,pos1[,"start"]-2)
        pos2<-str_locate(pattern="\\.",tmp_string)
        # strip out the players name who scored goal
        tmp_string<-substr(tmp_string,pos2[,"start"]+2,str_length(tmp_string))
        tmp_string<-trimws(tmp_string)
        return(tmp_string)
    }
    if ((grepl("ATTEMPT", evt, fixed = TRUE) | grepl("HITS THE", evt, fixed = TRUE))
        & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        # find position in evt string, where the pattern (NORTHAMPTON TOWN) starts and ends
        # store it in pos1, and can then reference it, using pos1[,"start"]
        pos1<-str_locate(pattern="\\(NORTHAMPTON TOWN\\)",evt)
        # get the all characters in evt, from character 1, to the pos1 start position-2
        tmp_string<-substr(evt,1,pos1[,"start"]-2)
        pos2<-str_locate(pattern="\\.",tmp_string)
        if (is.na(pos2[,"start"])) {
            # strip out the players name who scored goal
            tmp_string<-substr(tmp_string,1,str_length(tmp_string))
        }
        else {
            # strip out the players name who scored goal
            tmp_string<-substr(tmp_string,pos2[,"start"]+2,str_length(tmp_string))
        }
        tmp_string<-trimws(tmp_string)
        return(tmp_string)
    }
    if (grepl("WINS A FREE KICK", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
            # find position in evt string, where the pattern (NORTHAMPTON TOWN) starts and ends
            # store it in pos1, and can then reference it, using pos1[,"start"]
            pos1<-str_locate(pattern="\\(NORTHAMPTON TOWN\\)",evt)
            # get the all characters in evt, from character 1, to the pos1 start position-2
            tmp_string<-substr(evt,1,pos1[,"start"]-2)
            pos2<-str_locate(pattern="\\.",tmp_string)
            if (is.na(pos2[,"start"])) {
                # strip out the players name who scored goal
                tmp_string<-substr(tmp_string,1,str_length(tmp_string))
            }
            else {
                # strip out the players name who scored goal
                tmp_string<-substr(tmp_string,pos2[,"start"]+2,str_length(tmp_string))
            }
            tmp_string<-trimws(tmp_string)
            if (tmp_string=="") {
                tmp_string<-"NONE"
            }
            return(tmp_string)
    }
    if (grepl("CORNER", evt, fixed = TRUE) & !(grepl("NORTHAMPTON TOWN", evt, fixed = TRUE)) &
        !(grepl("GOAL", evt, fixed = TRUE)) & !(grepl("ATTEMPT", evt, fixed = TRUE))) {
        pos1<-str_locate(pattern="CONCEDED BY ",evt)
        # get the all characters in evt, from character 1, to the pos1 start position-2
        tmp_string<-substr(evt,pos1[,"end"]+1,str_length(evt))
        tmp_string<-gsub("[.]", "", tmp_string)
        tmp_string<-trimws(tmp_string)
        return(tmp_string)
    }
    if (grepl("YELLOW CARD", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        pos1<-str_locate(pattern="\\(NORTHAMPTON TOWN\\)",evt)
        # get the all characters in evt, from character 1, to the pos1 start position-2
        tmp_string<-substr(evt,1, pos1[,"start"]-1)
        tmp_string<-trimws(tmp_string)
        return(tmp_string)
    }
    if (grepl("RED CARD", evt, fixed = TRUE) & (grepl("NORTHAMPTON TOWN", evt, fixed = TRUE))) {
        pos1<-str_locate(pattern="\\(NORTHAMPTON TOWN\\)",evt)
        # get the all characters in evt, from character 1, to the pos1 start position-2
        tmp_string<-substr(evt,1, pos1[,"start"]-1)
        tmp_string<-trimws(tmp_string)
        return(tmp_string)
    }    
    else {
        return(c("NONE"))
    }
}

mlist<-read.csv("match_list.csv")

matchid <- 1
# initialise empty master data frame
all_matches<-data.frame(Timing=integer(),
                        Event=character(),
                        match_date=character(),
                        event_type=character(),
                        match_id=integer(),
                        H_A=character(),
                        event_player=character(),
                        added_time=character())

for (m_row in 1:nrow(mlist)) {

    url<-as.character(mlist$url[m_row])
    #Reading the HTML code from the website
    webpage <- read_html(url)
    
    #Using CSS selectors to scrap the match events
    event_data_html <- html_nodes(webpage,'.detail-col')
    #Using CSS selectors to scrap the timings of each event
    time_data_html <- html_nodes(webpage,'.time-col')
    #Using CSS selectors to scrap the date of each event
    mdate_data_html <- html_nodes(webpage,'.footballMatchSummaryGreyText')
    
    #Converting the ranking data to text
    event_data <- html_text(event_data_html)
    event_data <- toupper(event_data)
    #Converting the timing data to text
    time_data <- html_text(time_data_html)
    #Converting the match date data to text
    mdate_data <- html_text(mdate_data_html)
    match_date <- mdate_data[1]
    
    #Data-Preprocessing: Might need to do this if want numerical timings ?? commented out at moment
    # for(i in 1:length(time_data)){
    #    print(time_data[i])
    #    if (grepl("+",time_data[i], fixed = TRUE)==TRUE){
    # remove the "'" (minute) symbol
    #        time_data[i]<-sub("'","",time_data[i])
    # get position of + within time_data if it exists, if not it will be set to -1
    #        pos_plus<-as.numeric(regexpr('+',time_data[i], fixed = TRUE))
    #        print(pos_plus)
    #        digit1<-as.numeric(substr(time_data[i],1,pos_plus-1))
    #        print(digit1)
    #        digit2<-as.numeric(substr(time_data[i],pos_plus+1,nchar(time_data[i])))
    #        print(digit2)
    #        time_data[i]<-digit1+digit2
    #        print(time_data[i])
    #    }
    # }
    
    #Combining all the lists to form a data frame
    match_df<-data.frame(Timing = time_data, Event = event_data, stringsAsFactors = FALSE)
    # adding event_type field to dataframe, and then populate it
    match_df$event_type <- "NONE"
    # adding in home or away value
    if (mlist$home[m_row]=="northampton") {
        match_df$H_A<-c("HOME")
    }
    else {
            match_df$H_A<-c("AWAY")
    }
    # initialising added_time
    match_df$added_time <- as.integer(0)
    
    # initialising match date
    match_df$match_date <- match_date

    # populate the event_type column, utilising the evt_classify function, and mutate function from dplyr
    for (row in 1:nrow(match_df)) {
        match_df$event_type[row] <- evt_classify(match_df$Event[row])
        match_df$event_player[row] <- player_classify(match_df$Event[row])
        match_df$Timing[row]<-as.character(gsub("[']","",match_df$Timing[row]))
        match_df$Timing[is.na(match_df$Timing)]<-as.integer(0)
        print(match_df$Timing[row])
        if (grepl("+", match_df$Timing[row], fixed = TRUE)) {
            # get added on time from timing
            print("Found + timing")
            tmp_min<-substr(match_df$Timing[row],1,2)
            tmp_added<-as.integer(substr(match_df$Timing[row],as.integer(gregexpr(pattern = "+", 
                                                         match_df$Timing[row], fixed = TRUE))+1,
                              nchar(match_df$Timing[row])))
            print(tmp_added)
            match_df$added_time[row]<-tmp_added
            match_df$Timing[row]<-as.integer(tmp_min)
        }
        else {
            match_df$Timing[row]<-as.integer(match_df$Timing[row])
        }
    }
    # add in match_id
    match_df$match_id <- matchid
    matchid<-matchid+1
    
    # add match_df dataframe to master data frame
    all_matches<-rbind(all_matches,match_df)
    all_matches$Timing<-as.integer(all_matches$Timing)
    # empty out the match_Df data frame
    match_df<-match_df[NULL,]
}


#Structure of the data frame
str(all_matches)
write.csv(all_matches,"match_data_v2.csv")

# ------------------------------------------------------------
# now get the possession stats from the stats pages of website
mstats<-read.csv("match_list_stats_2.csv")

matchid_stats <- 1
# initialise empty master data frame
all_possession<-data.frame(Possession=integer(),
                           League_Position=character(),
                           Team=character(),
                          match_id=integer())
venue_list<-list("HOME","AWAY")

for (m_row in 1:nrow(mstats)) {
# for (m_row in 1:22) {
    
    url<-as.character(mstats$url[m_row])
    #Reading the HTML code from the website
    webpage <- read_html(url)
    
    #Using CSS selectors to scrap the match events
    possession_data_html <- html_nodes(webpage,'.possession')
    lp_data_html <- html_nodes(webpage,'.mt01e')
    # posession_data_html <- html_nodes(webpage,'.simple-donut')
    # posession_data_html <- html_nodes(webpage,'.football-match-statset')
    
    #Converting the data to text
    possession_data <- html_text(possession_data_html)
    lp_data <- html_text(lp_data_html)
    
    #Combining all the lists to form a data frame
    match_stats_df<-data.frame(Possession = possession_data, stringsAsFactors = FALSE)
    
    # add in match_id
    match_stats_df$match_id <- matchid_stats
    matchid_stats<-matchid_stats+1
    
    for (row in 1:nrow(match_stats_df)) {
        if (row==1) {
            print(as.character(mstats$home[m_row]))
            match_stats_df$Team[row]<-as.character(mstats$home[m_row])
            # strip out position of team in league, using regexpr (finds position of pattern)
            match_stats_df$League_Position[row]<- substr(lp_data[3],
                                                         regexpr("Pos.",lp_data[4])[1]+5,
                                                         regexpr("Pos.",lp_data[4])[1]+6)
            # match_stats_df$League_Position[row]= as.integer(substr(lp_data[3],13,14))
        }
        else {
            print(as.character(mstats$away[m_row]))
            match_stats_df$Team[row]<-as.character(mstats$away[m_row])
            # strip out position of team in league, using regexpr (finds position of pattern)
            match_stats_df$League_Position[row]<- substr(lp_data[4],
                                                         regexpr("Pos.",lp_data[4])[1]+5,
                                                         regexpr("Pos.",lp_data[4])[1]+6)
            # match_stats_df$League_Position[row]= lp_data[4]
        }
    }
    
    # add match_df dataframe to master data frame
    all_possession<-rbind(all_possession,match_stats_df)

    # empty out the match_Df data frame
    match_stats_df<-match_stats_df[NULL,]
    
}

#Structure of the data frame
str(all_possession)
write.csv(all_possession,"match_pos_stats_v1.csv")
