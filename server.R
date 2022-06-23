#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  adjusted_crashes <- reactive({
    if(input$crashflight == "All")
      crashes %>% filter(year(Date) >= input$years[1] & year(Date) <= input$years[2])
    else
      crashes %>% filter(year(Date) >= input$years[1] & year(Date) <= input$years[2] &  `Flight type` == input$crashflight)
  })
  
  adjusted_crashcauses <- reactive({
    
    if(input$crashcause == "All")
      crash_table <- crashes %>% mutate(Year = year(Date))
    else
      crash_table <- crashes %>% filter(`Crash cause` == input$crashcause) %>% mutate(Year = year(Date))
    
    crash_table %>% filter(Year >= input$crashyears[1] & Year <= input$crashyears[2])
    
  })
  
  adjusted_crashoperator <- reactive({
    
    if(input$crashoperator == "All")
      crash_table <- crashes %>% mutate(Year = year(Date))
    else
      crash_table <- crashes %>% filter(Operator == input$crashoperator) %>% mutate(Year = year(Date))
    
    crash_table %>% filter(Year >= input$crashyearsoperator[1] & Year <= input$crashyearsoperator[2])
    
  })

  adjusted_crashaircraft <- reactive({
    
    if(input$crashaircraft == "All")
      crash_table <- crashes %>% mutate(Year = year(Date))
    else
      crash_table <- crashes %>% filter(Aircraft == input$crashaircraft) %>% mutate(Year = year(Date))
    
    crash_table %>% filter(Year >= input$crashyearsaircraft[1] & Year <= input$crashyearsaircraft[2])
    
  })
    
  compute_deadliest <- reactive({
    max_fatalities <- max(adjusted_crashes()$`Total fatalities`)
    max_crashes <- adjusted_crashes()$Date[adjusted_crashes()$`Total fatalities` == max_fatalities]
    final_date <- paste0(month(max_crashes[1], label = TRUE), ". " , day(max_crashes[1]), ", ", year(max_crashes[1]))
    final_date
  })
  
  compute_survival <- reactive({
    total_crashes_survived <- length(adjusted_crashes()$Survivors[adjusted_crashes()$Survivors == "Yes"])
    total_crashes <- length(adjusted_crashes()$Survivors[adjusted_crashes()$Survivors == "Yes"]) +
      length(adjusted_crashes()$Survivors[adjusted_crashes()$Survivors == "No"])
    percent_survival <- round(100*total_crashes_survived/total_crashes, 2)
    paste0(percent_survival, "%")
  })
  
  common_reason <- reactive({
    reasons_freq <- adjusted_crashes() %>% group_by(`Crash cause`) %>% summarize(Freq = n()) %>% arrange(-Freq)
    reasons_freq$`Crash cause`[1]
  })
  
  modify_crashplots <- reactive({
    
    yearly_table <- adjusted_crashcauses()
    sma_est = length(unique(yearly_table$Year))
    
    output$crashtrend <- renderPlot({
      
      yearly_crash_table <- yearly_table %>%  group_by(Year) %>% summarize(Freq = n()) %>% 
        mutate(sma = SMA(Freq, n = ifelse(sma_est * 0.1 < 1, 1, sma_est * 0.1)))
      
      yearly_crash_table %>% ggplot(aes(x = Year, y = Freq)) + geom_col(color = "grey", fill = "orange3") + 
        geom_line(data = yearly_crash_table, aes(x = Year, y = sma), color = "blue", size = 1.5) + labs(x = "", y = "", title = "Annual Number of Plane Crashes Worldwide") +
        theme_classic() + theme(axis.text = element_text(size = 12), plot.title = element_text(size = 15, face = "bold"))
      
      
    })
    
    output$survival_dist <- renderPlot({
      
      survived_crash_table <- yearly_table %>% filter(!(is.na(Survivors))) %>% group_by(Survivors) %>% 
        summarize(Freq = n(), Perc = round(100*(Freq/nrow(yearly_table %>% filter(!(is.na(Survivors))))))) 
      
      survived_crash_table %>% ggplot(aes(x = "", y = Freq, fill = Survivors)) + 
        geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) + theme_void() +
        geom_text(aes(label = paste(Perc, "%", sep="")), position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(palette="Set3") + labs(title = "Survivability of Plane Crashes") +
        theme(plot.title = element_text(face = "bold", hjust = 0, size = 12), legend.position = "right", legend.title = element_text(face="bold"))
      
      
    })
    
    output$fatalities_dist <- renderPlot({
      outcome_data <- yearly_table %>% select(Date, `Crash cause`, ends_with("on board"), ends_with("fatalities"))
      outcome_data <- outcome_data %>% select(-`Other fatalities`, -`Total fatalities`) %>% 
        mutate(`Crew survivors` = ifelse(`Crew on board` - `Crew fatalities` < 0, 0, `Crew on board` - `Crew fatalities`),
               `PAX survivors` = ifelse(`Pax on board` - `PAX fatalities` < 0, 0, `Pax on board` - `PAX fatalities`))
      
      grouped_outcome <- outcome_data %>% pivot_longer(cols = `Crew on board`:`PAX survivors`, names_to = "class", values_to = "freq")
      grouped_outcome <- grouped_outcome %>% mutate(people = ifelse(str_detect(class, "C"), "Crew", "Passenger"), 
                                                    Type = case_when(str_detect(class, "board") == TRUE ~"Aboard",
                                                                     str_detect(class, "fatalities") == TRUE ~"Fatalities",
                                                                     TRUE~"Survivors"))
      
      by_type = grouped_outcome %>% filter(Type != "Aboard") %>% 
        group_by(`Crash cause`, people, Type) %>% 
        summarize(freq = sum(freq))
      
      by_type = by_type %>% mutate(perc = round(100*freq/sum(by_type$freq)))
      
      by_type <- tibble(`Crash cause` = c(by_type$`Crash cause`, rep("All", 4)),
                                people = c(by_type$people, rep(c("Crew", "Passenger"), 1, each = 2)),
                                Type = c(by_type$Type, rep(c("Fatalities", "Survivors"), 2, each = 1)),
                                freq = c(by_type$freq, c(sum(by_type$freq[by_type$people == "Crew" & by_type$Type == "Fatalities"]),
                                                         sum(by_type$freq[by_type$people == "Crew" & by_type$Type == "Survivors"]),
                                                         sum(by_type$freq[by_type$people == "Passenger" & by_type$Type == "Fatalities"]),
                                                         sum(by_type$freq[by_type$people == "Passenger" & by_type$Type == "Survivors"]))),
                                perc = c(by_type$perc, c(round(100*sum(by_type$freq[by_type$people == "Crew" & by_type$Type == "Fatalities"])/sum(by_type$freq)),
                                                         round(100*sum(by_type$freq[by_type$people == "Crew" & by_type$Type == "Survivors"])/sum(by_type$freq)),
                                                         round(100*sum(by_type$freq[by_type$people == "Passenger" & by_type$Type == "Fatalities"])/sum(by_type$freq)),
                                                         round(100*sum(by_type$freq[by_type$people == "Passenger" & by_type$Type == "Survivors"])/sum(by_type$freq)))))
      
      if(input$crashcause == "All")
      {
        by_type %>% filter(`Crash cause` == "All") %>% ggplot(aes(x = freq, y = people, fill = Type)) + 
          geom_col(position = "dodge") + labs(x = "", y = "", title = "Survivability of Souls on Board") + 
          geom_text(aes(label = paste(perc, "%", sep="")), position = position_dodge(0.9), hjust = 1, size = 3) +
          theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                axis.text.y = element_text(size=9), legend.text = element_text(size=9), 
                legend.position = "right", legend.title = element_text(face="bold"), 
                panel.background = element_rect(fill = "white"), 
                plot.title = element_text(face = "bold", size = 12, hjust = 0))
      }
      else{
        by_type %>% ggplot(aes(x = freq, y = people, fill = Type)) + 
          geom_col(position = "dodge") + labs(x = "", y = "", title = "Survivability of Souls on Board") + 
          geom_text(aes(label = paste(perc, "%", sep="")), position = position_dodge(0.9), hjust = 1, size = 3) +
          theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                axis.text.y = element_text(size=9), legend.text = element_text(size=9), 
                legend.position = "right", legend.title = element_text(face="bold"), 
                panel.background = element_rect(fill = "white"), 
                plot.title = element_text(face = "bold", size = 12, hjust = 0))
      }
      
    })
    
    output$countries_bar <- renderPlot({
      
      top_countries <- yearly_table %>% filter(Country != "World") %>% group_by(Country) %>% 
        summarize(Freq = n()) %>% arrange(-Freq)
      top_countries <- top_countries %>% mutate(perc = round(100*Freq/sum(top_countries$Freq))) %>% head(7)
      
      top_countries$Country[top_countries$Country == "United States of America"] <- "USA"
      top_countries$Country[top_countries$Country == "United Kingdom"] <- "UK"

      top_countries %>% ggplot(aes(x = Freq, y = reorder(Country, Freq))) + geom_col(fill = "khaki3") + labs(x = "", y = "") +
        theme_void() + 
        geom_text(aes(label = ifelse(max(top_countries$perc) > 35 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) +
        labs(title = "Most Crashed Countries") + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
      
    })
    
    output$terrain_bar <- renderPlot({
      
      top_terrains <- yearly_table %>% group_by(`Crash site`) %>% summarize(Freq = n()) %>% arrange(-Freq)
      top_terrains <- top_terrains %>% mutate(perc = round(100*Freq/sum(top_terrains$Freq)))
      
      top_terrains$`Crash site`[top_terrains$`Crash site` == "Airport (less than 10 km from airport)"] <- "Near airport"
      top_terrains$`Crash site`[top_terrains$`Crash site` == "Lake, Sea, Ocean, River"] <- "Waters"
      top_terrains$`Crash site`[is.na(top_terrains$`Crash site`)] <- "Unknown"
      
      top_terrains %>% ggplot(aes(x = Freq, y = reorder(`Crash site`, Freq))) + geom_col(fill = "chocolate") + labs(x = "", y = "") +
        theme_void() + labs(title = "Most Crashed Terrains") + 
        geom_text(aes(label = ifelse(max(top_terrains$perc) > 30 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) + 
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 0, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
      
    })
  })
  
  modify_crash_operator_plots <- reactive({
    
    yearly_table <- adjusted_crashoperator()
    
    sma_est = length(unique(yearly_table$Year))
    
    output$crashoperatortrend <- renderPlot({
      
      yearly_crash_table <- yearly_table %>%  group_by(Year) %>% summarize(Freq = n()) %>% 
        mutate(sma = SMA(Freq, n = ifelse(sma_est * 0.1 < 1, 1, sma_est * 0.1)))
      
      yearly_crash_table %>% ggplot(aes(x = Year, y = Freq)) + geom_col(color = "grey", fill = "gold2") + 
        geom_line(data = yearly_crash_table, aes(x = Year, y = sma), color = "blue", size = 1.5) + labs(x = "", y = "", title = "Annual Number of Plane Crashes Worldwide") +
        theme_classic() + theme(axis.text = element_text(size = 12), plot.title = element_text(size = 15, face = "bold"))
      
      
    })
    
    
    output$planes_dist <- renderPlot({
      top_aircrafts <- yearly_table %>% group_by(Aircraft) %>% summarize(Freq = n())
      top_aircrafts <- top_aircrafts %>% mutate(perc = round(100*Freq/sum(top_aircrafts$Freq)))
      top_aircrafts <- top_aircrafts %>% arrange(-Freq) %>% head(7)
      top_aircrafts %>% ggplot(aes(x = Freq, y = reorder(Aircraft, Freq))) + geom_col(fill = "grey") + labs(x = "", y = "") +
        theme_void() + labs(title = "Most Crashed Aircrafts") + 
        geom_text(aes(label = ifelse(max(top_aircrafts$perc) > 30 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) +
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 1.5, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
    })
    
    output$countries_operator_bar <- renderPlot({
      
      top_countries <- yearly_table %>% filter(Country != "World") %>% group_by(Country) %>% summarize(Freq = n())
      top_countries <- top_countries %>% mutate(perc = round(100*Freq/sum(top_countries$Freq)))
      top_countries <- top_countries %>% arrange(-Freq) %>% head(7)
      top_countries$Country[top_countries$Country == "United States of America"] <- "USA"
      top_countries$Country[top_countries$Country == "United Kingdom"] <- "UK"
      top_countries %>% ggplot(aes(x = Freq, y = reorder(Country, Freq))) + geom_col(fill = "khaki3") + labs(x = "", y = "") +
        theme_void() + labs(title = "Most Crashed Countries") + 
        geom_text(aes(label = ifelse(max(top_countries$perc) > 40 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) +
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
      
    })
    
    output$terrain_operator_bar <- renderPlot({
      
      top_terrains <- yearly_table %>% group_by(`Crash site`) %>% summarize(Freq = n())
      top_terrains <- top_terrains %>% mutate(perc = round(100*Freq/sum(top_terrains$Freq)))
      top_terrains <- top_terrains %>% arrange(-Freq) 
      top_terrains$`Crash site`[top_terrains$`Crash site` == "Airport (less than 10 km from airport)"] <- "Near airport"
      top_terrains$`Crash site`[top_terrains$`Crash site` == "Lake, Sea, Ocean, River"] <- "Waters"
      top_terrains$`Crash site`[is.na(top_terrains$`Crash site`)] <- "Unknown"
      top_terrains %>% ggplot(aes(x = Freq, y = reorder(`Crash site`, Freq))) + geom_col(fill = "chocolate") + labs(x = "", y = "") +
        theme_void() + labs(title = "Most Crashed Terrains") + 
        geom_text(aes(label = ifelse(max(top_terrains$perc) > 40 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) +
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 0, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
      
    })
    
    output$cause_operator_dist <- renderPlot({
      
      crash_causes <- yearly_table %>% group_by(`Crash cause`) %>% 
        summarize(Freq = n()) 
      crash_causes <- crash_causes %>% mutate(perc = round(100*Freq/sum(crash_causes$Freq)))
      crash_causes$`Crash cause`[str_detect(crash_causes$`Crash cause`, "Terrorism")] <- "Terrorism"
      
      crash_causes %>% ggplot(aes(x = Freq, y = reorder(`Crash cause`, Freq))) + geom_col(fill = "palevioletred2") + labs(x = "", y = "") +
        theme_void() + labs(title = "Common Crash Causes") + 
        geom_text(aes(label = ifelse(max(crash_causes$perc) > 30 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) +
        theme(plot.title = element_text(size = 12, face = "bold", hjust = -1, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())      
      
    })
    
    
  })
  
  modify_crash_aircraft_plots <- reactive({
    
    yearly_table <- adjusted_crashaircraft()
    
    sma_est = length(unique(yearly_table$Year))
    
    output$crashaircrafttrend <- renderPlot({
      
      yearly_crash_table <- yearly_table %>%  group_by(Year) %>% summarize(Freq = n()) %>% 
        mutate(sma = SMA(Freq, n = ifelse(sma_est * 0.1 < 1, 1, sma_est * 0.1)))
      
      yearly_crash_table %>% ggplot(aes(x = Year, y = Freq)) + geom_col(color = "grey", fill = "grey80") + 
        geom_line(data = yearly_crash_table, aes(x = Year, y = sma), color = "blue", size = 1.5) + labs(x = "", y = "", title = "Annual Number of Plane Crashes Worldwide") +
        theme_classic() + theme(axis.text = element_text(size = 12), plot.title = element_text(size = 15, face = "bold"))
      
      
    })
    
    output$cause_aircraft_dist <- renderPlot({
      
      crash_causes <- yearly_table %>% group_by(`Crash cause`) %>% 
        summarize(Freq = n()) 
      crash_causes <- crash_causes %>% mutate(perc = round(100*Freq/sum(crash_causes$Freq)))
      crash_causes$`Crash cause`[str_detect(crash_causes$`Crash cause`, "Terrorism")] <- "Terrorism"
      
      crash_causes %>% ggplot(aes(x = Freq, y = reorder(`Crash cause`, Freq))) + geom_col(fill = "palevioletred2") + labs(x = "", y = "") +
        theme_void() + labs(title = "Common Crash Causes") + 
        geom_text(aes(label = ifelse(max(crash_causes$perc) > 30 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) + 
        theme(plot.title = element_text(size = 12, face = "bold", hjust = -0.7, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())      
      
    })
    
    output$phases_dist <- renderPlot({
      
      phases_crash_table <- yearly_table %>% filter(!(is.na(`Flight phase`))) %>% group_by(`Flight phase`) %>% 
        summarize(Freq = n(), perc = round(100*(Freq/nrow(yearly_table %>% filter(!(is.na(`Flight phase`))))))) %>% arrange(-Freq)
      
      phases_crash_table$`Flight phase`[str_detect(phases_crash_table$`Flight phase`, "Landing")] <- "Landing"
      phases_crash_table$`Flight phase`[str_detect(phases_crash_table$`Flight phase`, "Takeoff")] <- "Takeoff"
      
      phases_crash_table %>% ggplot(aes(x = "", y = Freq, fill = reorder(`Flight phase`, -Freq))) + 
        geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) + theme_void() + 
        geom_text(aes(label = ifelse(max(phases_crash_table$perc) > 40 & perc < 10, "", paste(perc, "%", sep=""))), position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(palette="Set2") + labs(title = "Phases of Plane Crashes") +
        theme(plot.title = element_text(face = "bold", hjust = 0, size = 12), legend.position = "right", 
              legend.text = element_text(size = 9), legend.title = element_blank())
      
    })
    
    output$ftype_dist <- renderPlot({
      top_ftype_aircrafts <- yearly_table %>% group_by(`Flight type`) %>% summarize(Freq = n()) 
      top_ftype_aircrafts <- top_ftype_aircrafts %>% mutate(perc = round(100*Freq/sum(top_ftype_aircrafts$Freq)))
      top_ftype_aircrafts <- top_ftype_aircrafts %>% arrange(-Freq) %>% head(7)
      top_ftype_aircrafts$`Flight type`[str_detect(top_ftype_aircrafts$`Flight type`, "Charter/Taxi")] <- "Charter/Taxi"
      
      top_ftype_aircrafts %>% ggplot(aes(x = Freq, y = reorder(`Flight type`, Freq))) + geom_col(fill = "orange") + labs(x = "", y = "") +
        theme_void() + labs(title = "Flight Types of Crashes") + 
          geom_text(aes(label = ifelse(max(top_ftype_aircrafts$perc) > 30 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 0.8, size = 3) + 
          theme(plot.title = element_text(size = 12, face = "bold", hjust = 1.5, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
    })
    
    output$countries_aircraft_bar <- renderPlot({
      
      top_countries <- yearly_table %>% filter(Country != "World") %>% group_by(Country) %>% summarize(Freq = n()) 
      top_countries <- top_countries %>% mutate(perc = round(100*Freq/sum(top_countries$Freq))) 
      top_countries <- top_countries %>% arrange(-Freq) %>% head(7)
      top_countries$Country[top_countries$Country == "United States of America"] <- "USA"
      top_countries$Country[top_countries$Country == "United Kingdom"] <- "UK"
      top_countries %>% ggplot(aes(x = Freq, y = reorder(Country, Freq))) + geom_col(fill = "khaki3") + labs(x = "", y = "") +
        theme_void() + labs(title = "Most Crashed Countries") + 
        geom_text(aes(label = ifelse(max(top_countries$perc) > 30 & perc < 5, "", paste(perc, "%", sep=""))), position = position_dodge(0.9), hjust = 1, size = 3) + 
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 0, vjust = 2), axis.text = element_text(size = 10), axis.text.x = element_blank())
      
    })
    
    })
  
  output$overall <- renderValueBox({
    valueBox(
        subtitle = "Total Plane Crashes Worldwide",
        value = tags$p(nrow(adjusted_crashes()), style = "font-size: 60%;"),
        icon = tags$i(icon("plane", lib = "glyphicon"), style="font-size: 40px"),
        color = "blue"
      )
  })
  
  output$survivalrate <- renderValueBox({
    valueBox(
      subtitle = "Percentage of Survivable Crashes",
      value = tags$p(compute_survival(), style = "font-size: 60%;"),
      icon = tags$i(icon("road", lib = "glyphicon"), style="font-size: 40px"),
      color = "green"
    )
  })
  
  output$deadliestcrash <- renderValueBox({
    valueBox(
      subtitle = paste0("Single Deadliest Crash (", max(adjusted_crashes()$`Total fatalities`), " dead)"),
      value = tags$p(compute_deadliest(), style = "font-size: 60%;"),
      icon = tags$i(icon("warning-sign", lib = "glyphicon"), style="font-size: 40px"),
      color = "orange"
    )
  })
  
  output$frequentreason <- renderValueBox({
    valueBox(
      subtitle = "Most Common Reason for Crashes",
      value = tags$p(common_reason(), style = "font-size: 60%;"),
      icon = tags$i(icon("remove-sign", lib = "glyphicon"), style="font-size: 40px"),
      color = "red"
    )
  })
  
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
  update_table <- reactive({
        inputs = list(i1 = input$id1, 
                      i2 = input$id2, 
                      i3 = input$id3, 
                      i4 = input$id4, 
                      i5 = input$id5,
                      i6 = input$id6, 
                      i7 = input$id7, 
                      i8 = input$id8, 
                      i9 = input$id9, 
                      i10 = input$id10)
        
        cols = c("Region", "Country", "Operator", "Aircraft", "Registration",
                 "Crash cause", "Crash site", "Flight phase", "Flight type", "Survivors")
        
        if(is.null(inputs)){
          crashes <- crashes %>% filter(ymd(Date) >= ymd(input$dates[1]) & ymd(Date) <= ymd(input$dates[2]))
          output$table <- renderDataTable(crashes)
        }
        else{
          updated_table <- crashes
          for (index in 1:10){
            if(!(is.null(inputs[[index]]))){
              search_val <- ifelse(length(inputs[[index]]) > 1, inputs[[index]][1], inputs[[index]]) 
              checker <- unlist(purrr::map(cols, function(col) search_val %in% unique_vals[[col]]))
              col_index <- match(TRUE, checker)
              updated_table <- updated_table %>% filter(.data[[cols[col_index]]] %in% inputs[[index]])
            }
          }
          
          updated_table <- updated_table %>% filter(ymd(Date) >= ymd(input$dates[1]) & ymd(Date) <= ymd(input$dates[2]))
          updated_table
      }
    })
  
  update_chart <- reactive({
    df <- adjusted_crashes() %>% group_by(Country) %>% summarize(count = n())
    colnames(df)[1] <- "id"
    
    custom_breaks <- list(high = c(-Inf,round(max(df$count)/c(9, 7, 5, 3)), Inf),
                          near_hi = c(-Inf,round(max(df$count)/c(8, 6, 4, 2)), Inf),
                          mid = c(-Inf, round(max(df$count)/c(10, 5)), Inf),
                          near_low = c(-Inf, 0, 5, Inf),
                          low = c(-Inf, 0, Inf))
    
    custom_labels <- list(high = c(paste0("Less than ", round(max(df$count)/9)), 
                                   paste0(round(max(df$count)/9)+ 1," - ", round(max(df$count)/7)),
                                   paste0(round(max(df$count)/7)+ 1," - ", round(max(df$count)/5)),
                                   paste0(round(max(df$count)/5)+ 1," - ", round(max(df$count)/3)), 
                                   paste0("More than ", round(max(df$count)/3))),
                          near_hi = c(paste0("Less than ", round(max(df$count)/8)), 
                                  paste0(round(max(df$count)/8)+ 1," - ", round(max(df$count)/6)),
                                  paste0(round(max(df$count)/6)+ 1," - ", round(max(df$count)/4)),
                                  paste0(round(max(df$count)/4)+ 1," - ", round(max(df$count)/2)), 
                                  paste0("More than ", round(max(df$count)/2))),
                          mid = c(paste0("Less than ", round(max(df$count)/10)), 
                                  paste0(round(max(df$count)/10)+ 1," - ", round(max(df$count)/5)),
                                  paste0("More than ", round(max(df$count)/5))),
                          near_low = c(paste0("Less than ", 0),
                                       paste0(1," - ", 6),
                                       paste0("More than ", 6)),
                          low = c(paste0("Less than ", 0),
                                  paste0("More than ", 0)))
    
    break_index <- case_when(max(df$count) < 3 ~ "low",
                             max(df$count) < 10 ~ "near_low",
                             max(df$count) <= 20 ~ "mid",
                             max(df$count) <= 100 ~ "near_hi",
                             TRUE~"high")
    
    df$count_group <- cut(df$count, 
                          breaks = custom_breaks[[break_index]], 
                          labels = custom_labels[[break_index]])
    
    df <- df %>% left_join(missing_countries_df, by="id")
    df <- df %>% filter(!(id %in% c(NA, invalid_countries))) %>%  mutate(id = ifelse(is.na(sorted_filled_countries), id, sorted_filled_countries))
    df
  })
  
  output$map <- renderPlot({
    df <- update_chart()
    map <- ggplot(df) +
      geom_map(aes(map_id = id, fill = fct_rev(count_group)), map = world_map) +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', fill = NA) +
      expand_limits(x = world_map$long, y = world_map$lat) +
      scale_fill_manual(name = "Number of Crashes", values = rev(brewer.pal(5, name = "Reds"))) +
      theme_void() + theme(legend.title = element_text(size = 15, face="bold"), legend.text = element_text(size = 12)) +
      coord_fixed()
    map
  })
  
  output$map_table <-  renderDataTable(
    update_chart() %>% 
      select(id, count) %>% 
      arrange(-count) %>% 
      mutate(Rank = rank(-count))%>%
      rename(Country = id, `Total Crashes` = count) %>% 
      select(Rank, Country, `Total Crashes`), 
    
    options = list(pageLength = 5, dom = "ltipr")
    )
  
  observeEvent(input$crashcause,{
    
    modify_crashplots()
    
  })
  
  observeEvent(input$crashyears,{
    
    modify_crashplots()
    
  })
  
  observeEvent(input$crashoperator,{
    
    modify_crash_operator_plots()
    
  })
  
  observeEvent(input$crashyearsoperator,{
    
    modify_crash_operator_plots()
    
  })  
  
  observeEvent(input$crashaircraft,{
    
    modify_crash_aircraft_plots()
    
  })
  
  observeEvent(input$crashyearsaircraft,{
    
    modify_crash_aircraft_plots()
    
  })
  
  observeEvent(input$id11,{
    
        circumstances_table <- update_table()[, c("Date", "Aircraft", "Operator", "Country", "Crash cause")] 
        profile_table <- update_table()[, c("Aircraft", "Operator", "Registration", "MSN", "YOM")] 
        purpose_table <- update_table()[, c("Time", "Flight phase", "Flight type", "Schedule", "Flight no.")] 
        crash_location <- update_table()[, c("Crash site", "Crash location", "Country", "Region")] 
        outcome <- update_table()[, c("Survivors", "Crew on board", "Crew fatalities", "Pax on board", "PAX fatalities", "Other fatalities", "Total fatalities")]
        
        output$table1 <- renderDataTable(circumstances_table, options = list(
          pageLength = 12
        ))
        output$table2 <- renderDataTable(profile_table, options = list(
          pageLength = 12
        ))
        output$table3 <- renderDataTable(purpose_table, options = list(
          pageLength = 12
        ))
        output$table4 <- renderDataTable(crash_location, options = list(
          pageLength = 12
        ))
        output$table5 <- renderDataTable(outcome, options = list(
          pageLength = 12
        ))
    })
   
})
