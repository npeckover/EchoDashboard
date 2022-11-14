ui <- dashboardPage(
  dashboardHeader(title = "PSPH Echo Lab"),
  dashboardSidebar(
    
    fixedPanel(
      dateRangeInput(inputId = "date", 
                     label = strong("Select a range of dates"),
                     start = past30,
                     end = today,
                     min = jan1,
                     max = today),
      uiOutput("ui"),
      width = "235px"),
    minified = F
    ),
  
  dashboardBody(plotOutput("comp_rs"),
                plotOutput("examtype"),
                plotOutput("bysono"),
                dataTableOutput("table"))
  
)

server <- function(input, output) {
  
  selected_dates <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] <= input$date[2], "Error: Start date should be earlier than or equal to end date."))
    sum_table %>%
      filter(DATE >= input$date[1] & DATE <= input$date[2])
  })
  
  ## Completed vs. Rescheduled ##
  output$comp_rs <- renderPlot({
    
    # plot colors
    colors <- c("Completed" = "#3B9AB2",
                "Rescheduled" = "#F21A00")
    
    # make plot
    selected_dates() %>%
      select(DATE, Completed, Rescheduled) %>%
      ggplot(aes(x = DATE)) +
      
      # rescheduled exams
      geom_step(aes(y = Rescheduled,
                    color = "Rescheduled"),
                alpha = 1,
                size = 1) +
      geom_rect(aes(xmin = DATE, xmax = lead(DATE),
                    ymin = 0, ymax = Rescheduled,
                    fill = "Rescheduled"),
                alpha = .4) +
      
      # completed exams
      geom_step(aes(y = Completed,
                    color = "Completed"),
                alpha = 1,
                size = 1) +
      geom_rect(aes(xmin = DATE, xmax = lead(DATE),
                    ymin = 0, ymax = Completed,
                    fill = "Completed"),
                alpha = .4) +
      
      scale_fill_manual(values = colors) + 
      scale_color_manual(values = colors) + 
      labs(x = "Date",
           y = "Count",
           color = "") +
      guides(fill = F) + 
      theme_np() +
      labs(title = "Completed and Rescheduled",
           subtitle = "Counts per Day")
  })
  
  ## Exam Type ##
  output$examtype <- renderPlot({
    
    # subset data
    pex <- selected_dates() %>%
      select(TEE.Att, DSE, Limited, PCC, 
             TEE, TTE, Covid, STAT, CCU, ED, BAV, WM.Pre,
             WM.Place, WM.Post) %>%
      select(where(~sum(.) != 0)) %>% 
      pivot_longer(everything(), names_to = "Exam_Type", values_to = "Count") 
    
    # create plot
    pex %>% ggplot(aes(x = Exam_Type,
                       y = Count,
                       fill = Exam_Type)) +
      geom_col(show.legend = F) + 
      scale_fill_manual(values = wes_palette("Zissou1",
                                             n = length(unique(pex$Exam_Type)), 
                                             type = "continuous")) +
      theme_np() + 
      labs(title = "Exam Type")
  })
  ## Sonographers ##
  output$bysono <- renderPlot({
    
    # subset data
    psono <- selected_dates() %>%
      select(all_of(tech_names)) %>%
      select(where(~sum(.) != 0)) %>%
      pivot_longer(everything(), names_to = "Sonographer", values_to = "Echoes_Performed")
    
    # create plot  
    psono %>% ggplot(aes(x = Sonographer,
                         y = Echoes_Performed,
                         fill = Sonographer)) +
      geom_col(show.legend = F) + 
      scale_fill_manual(values = wes_palette("Zissou1",
                                             n = length(unique(psono$Sonographer)), 
                                             type = "continuous")) +
      theme_np() +
      labs(title = "Exams By Sonographer",
           subtitle = "All Types")
  })
  
  # placeholder plot
  output$table <- renderDataTable({
    selected_dates() %>%
      select(DATE:WM.Post)
  }
  , options = list(scrollX = T,
                   scrollY = "600px")
  )
  
}
shinyApp(ui, server)
