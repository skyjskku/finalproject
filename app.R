library(tidyverse)
library(data.table)


boxoffice_new = data.table::fread("boxoffice_new.csv")
rmro = data.table::fread("rmro.csv")

mytheme = theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(face = "bold")
  )



ui <- fluidPage(
  titlePanel(strong("μ½λ‘?19 ?΄? κ³? ?΄? κ΅??΄ λ°μ€?€?Ό?€ ?€?")),
  
  mainPanel(
    p("2019? ?λ£λ 2019? 3? 1?ΌλΆ?° 10? 31?ΌκΉμ?? ?λ£μ΄λ©?, 2020? ?λ£λ 2020? 3? 1?ΌλΆ?° 10? 31?ΌκΉμ?? ?λ£λ‘,
      2019? ?λ£μ? 2020? ?λ£λ κ°κ° μ½λ‘?19 ?΄?  ?κΈ°μ? μ½λ‘?19 ?΄? ?κΈ°λ?? ??λ³??€.")
  ),

  ### Part 1 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 1. κ°λ³ ?¨? ?΅κ³?")),
      selectInput("category1_1", "?¨?", choices = c("?λ³?", "?°?λ³?")),
      selectInput("category1_2", "μ§?", choices = c("?? 1?Έ ?Ή ?λ£? λ§€μΆ? ?κ·?",
                                                     "?? 1?Έ ?Ή ?λ£? κ΄κ°? ?? ?κ·?",
                                                     "?? 1?Έ ?Ή ?λ£? ?? ??? ?κ·?",
                                                     "?? 1? ?Ή λ§€μΆ? ?κ·?",
                                                     "?? 1? ?Ή κ΄κ°? ?? ?κ·?",
                                                     "??κ΄ 1κ΄ ?Ή ?? ??? ?κ·?")
      )
    ),
    
    mainPanel(
      plotOutput("plot1")
      #textOutput("text1")
    )
    
  ),

  hr(),
  
  ### Part 2 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 2. κΈ°λ‘? ?΅κ³? (μ΄?, μ΅λ?)")),
      selectInput("category2", "μ§? (?? 1?Έ ?Ή)", choices = c("?λ£? μ΅λ? λ§€μΆ? ?κ·?",
                                                                 "μ΄? λ§€μΆ? ?κ·?",
                                                                 "?λ£? μ΅λ? κ΄κ°? ?? ?κ·?",
                                                                 "μ΄? κ΄κ°? ?? ?κ·?")
      ),
      strong("?¬κ°λ΄ ?? ? ?Έ? ?°?΄?° κΈ°λ°"),
      p("κΈ°κ° ?΄ λ°μ€?€?Ό?€? μ§μ? ? μ§κ? κ°λ΄?Όλ‘λ??° 365?Ό?΄ μ§? ?? ?΄λ©? ?¬κ°λ΄ ??λ‘? κ°μ£Ό?¨."),
    ),
    
    mainPanel(
      plotOutput("plot2")
    )
    
  ),

  hr(),
  
  ### Part 3 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 3. ?? μΉ΄νκ³ λ¦¬λ³? ?΅κ³?")),
      selectInput("category3_1", "λΆλ₯ κΈ°μ?", choices = c("κ΅??΄ λ°? ?΄?Έ ??", "κ΅?κ°λ³? ??", "?₯λ₯΄λ³ ??", "?¬κ°λ΄ λ°? μ΄κ°λ΄? ??")),
      selectInput("category3_2", "μ§?", choices = c("?? ??", "?? ? ? ?¨", "?", "? ? ?¨"))
    ),
    
    mainPanel(
      plotOutput("plot3")
    )
    
  ),
)


server <- function(input, output){
  ### Part 1 ###
  data1 <- reactive({
    if (input$category1_1 == "?λ³?"){
      if (input$category1_2 == "?? 1?Έ ?Ή ?λ£? λ§€μΆ? ?κ·?"){
        boxoffice_new[, mean(salesAmt), .(year, month)]
      }else if (input$category1_2 == "?? 1?Έ ?Ή ?λ£? κ΄κ°? ?? ?κ·?"){
        boxoffice_new[, mean(audiCnt), .(year, month)]
      }else if (input$category1_2 == "?? 1?Έ ?Ή ?λ£? ?? ??? ?κ·?"){
        boxoffice_new[, mean(showCnt), .(year, month)]
      }else if (input$category1_2 == "?? 1? ?Ή λ§€μΆ? ?κ·?"){
        boxoffice_new[, mean(sales_per_show), .(year, month)]
      }else if (input$category1_2 == "?? 1? ?Ή κ΄κ°? ?? ?κ·?"){
        boxoffice_new[, mean(audi_per_show), .(year, month)]
      }else{
        boxoffice_new[, mean(show_per_scrn), .(year, month)]
      }
    }else{
      if (input$category1_2 == "?? 1?Έ ?Ή ?λ£? λ§€μΆ? ?κ·?"){
        boxoffice_new[, mean(salesAmt), year]
      }else if (input$category1_2 == "?? 1?Έ ?Ή ?λ£? κ΄κ°? ?? ?κ·?"){
        boxoffice_new[, mean(audiCnt), year] 
      }else if (input$category1_2 == "?? 1?Έ ?Ή ?λ£? ?? ??? ?κ·?"){
        boxoffice_new[, mean(showCnt), year]
      }else if (input$category1_2 == "?? 1? ?Ή λ§€μΆ? ?κ·?"){
        boxoffice_new[, mean(sales_per_show), year]
      }else if (input$category1_2 == "?? 1? ?Ή κ΄κ°? ?? ?κ·?"){
        boxoffice_new[, mean(audi_per_show), year]
      }else{
        boxoffice_new[, mean(show_per_scrn), year]
      }
    }
  })
  
  ylab1 <- reactive({
    if (str_detect(input$category1_2, "λ§€μΆ? ?κ·?")){
      "λ§€μΆ (?)"
    }else if (str_detect(input$category1_2, "κ΄κ°? ?? ?κ·?")){
      "κ΄κ°? ? (λͺ?)"
    }else{
      "?? ?? (?Έ)"
    }
  })
  
  plotting1 <- reactive({
    if (input$category1_1 == "?λ³?"){
      ggplot(data1(), aes(x = factor(month), y = V1, fill = factor(year))) +
        geom_col(position = "dodge") +
        xlab("?") +
        ylab(ylab1())
    }else{
      ggplot(data1(), aes(x = factor(year), y = V1, fill = factor(year))) +
        geom_col() +
        xlab("?°?") +
        ylab(ylab1())
    }
  })
  
  output$plot1 <- renderPlot({
    plotting1() +
      ggtitle(input$category1_2) +
      labs(fill = "?°?") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 2 ###
  data2 <- reactive({
    if (input$category2 == "?λ£? μ΅λ? λ§€μΆ? ?κ·?"){
      rmro[, .N, .(year, salesPeak, movieCd)][, mean(salesPeak), year]
    }else if (input$category2 == "μ΄? λ§€μΆ? ?κ·?"){
      dt = rmro[, .N, .(year, salesTotal, movieCd)][, mean(salesTotal), year]
      dt[, V1 := V1 %>% as.numeric]
      return(dt)
    }else if (input$category2 == "?λ£? μ΅λ? κ΄κ°? ?? ?κ·?"){
      rmro[, .N, .(year, audiPeak, movieCd)][, mean(audiPeak), year] 
    }else{
      rmro[, .N, .(year, audiTotal, movieCd)][, mean(audiTotal), year]
    }
  })
  
  ylab2 <- reactive({
    if (input$category2 == "?λ£? μ΅λ? λ§€μΆ? ?κ·?"){
      "?λ£? μ΅λ? λ§€μΆ (?)"
    }else if (input$category2 == "μ΄? λ§€μΆ? ?κ·?"){
      "μ΄? λ§€μΆ (?)"
    }else if (input$category2 == "?λ£? μ΅λ? κ΄κ°? ?? ?κ·?"){
      "μ΅λ? κ΄κ°? ?"
    }else{
      "μ΄? κ΄κ°? ?"
    }
  })
  
  output$plot2 <- renderPlot({
    ggplot(data2(), aes(x = factor(year), y = V1, fill = factor(year))) +
      geom_col() +
      ggtitle(str_c("?? 1?Έ ?Ή ", input$category2)) +
      xlab("?°?") +
      ylab(ylab2()) +
      labs(fill = "?°?") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 3 ###
  var3 <- reactive({
    if (input$category3_1 == "κ΅??΄ λ°? ?΄?Έ ??"){
      "dom_or_for"
    }else if (input$category3_1 == "κ΅?κ°λ³? ??"){
      "nation"
    }else if (input$category3_1 == "?₯λ₯΄λ³ ??"){
      "genre"
    }else{
      "reopen"
    }
  })
  
  data3 <- reactive({
    if (input$category3_1 == "κ΅??΄ λ°? ?΄?Έ ??"){
      if (input$category3_2 == "?? ??"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
      }else if (input$category3_2 == "?? ? ? ?¨"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?"){
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
      }else{
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "κ΅?κ°λ³? ??"){
      if (input$category3_2 == "?? ??"){
        dt = boxoffice_new[, .N, .(year, nation)]
      }else if (input$category3_2 == "?? ? ? ?¨"){
        dt = boxoffice_new[, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?"){
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
      }else{
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "?₯λ₯΄λ³ ??"){
      if (input$category3_2 == "?? ??"){
        dt = boxoffice_new[, .N, .(year, genre)]
      }else if (input$category3_2 == "?? ? ? ?¨"){
        dt = boxoffice_new[, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?"){
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
      }else{
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }
    }else{
      if (input$category3_2 == "?? ??"){
        dt = boxoffice_new[, .N, .(year, reopen)]
      }else if (input$category3_2 == "?? ? ? ?¨"){
        dt = boxoffice_new[, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?"){
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
      }else{
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }
      #dt[reopen == "μ΄κ°λ΄?" | reopen == "?¬κ°λ΄"]
    }
    setnames(dt, var3(), "V1")
    return(dt)
  })
  
  output$plot3 <- renderPlot({
    if (str_detect(input$category3_2, "? ? ?¨")){
      p = ggplot(data3(), aes(x = factor(year), y = N, fill = V1)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent)
      xlab = "?°?"
      ylab = "? ? ?¨"
      if (str_detect(input$category3_1, "λ³?")){
        labs = str_replace(input$category3_1, "λ³? ??", "")
      }else{
        labs = str_replace(input$category3_1, " ??", "")
      }
    }else{
      p = ggplot(data3(), aes(x = V1, y = N, fill = factor(year))) +
        geom_col(position = "dodge")
      if (str_detect(input$category3_1, "λ³?")){
        xlab = str_replace(input$category3_1, "λ³? ??", "")
      }else{
        xlab = str_replace(input$category3_1, " ??", "")
      }
      ylab = "? (?Έ)"
      labs = "?°?"
    }
    p + 
      ggtitle(str_c(input$category3_1, " ", input$category3_2)) +
      xlab(xlab) +
      ylab(ylab) +
      labs(fill = labs) +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
}


shinyApp(ui = ui, server = server)