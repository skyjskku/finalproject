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
  titlePanel(strong("ì½”ë¡œ?‚˜19 ?´? „ê³? ?´?›„ êµ??‚´ ë°•ìŠ¤?˜¤?”¼?Š¤ ?‹¤?ƒœ")),
  
  mainPanel(
    p("2019?…„ ?ë£ŒëŠ” 2019?…„ 3?›” 1?¼ë¶€?„° 10?›” 31?¼ê¹Œì?€?˜ ?ë£Œì´ë©?, 2020?…„ ?ë£ŒëŠ” 2020?…„ 3?›” 1?¼ë¶€?„° 10?›” 31?¼ê¹Œì?€?˜ ?ë£Œë¡œ,
      2019?…„ ?ë£Œì?€ 2020?…„ ?ë£ŒëŠ” ê°ê° ì½”ë¡œ?‚˜19 ?´? „ ?‹œê¸°ì?€ ì½”ë¡œ?‚˜19 ?´?›„ ?‹œê¸°ë?? ??€ë³€?•œ?‹¤.")
  ),

  ### Part 1 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 1. ê°œë³„ ?‹¨?œ„ ?†µê³?")),
      selectInput("category1_1", "?‹¨?œ„", choices = c("?›”ë³?", "?—°?„ë³?")),
      selectInput("category1_2", "ì§€?‘œ", choices = c("?˜?™” 1?¸ ?‹¹ ?•˜ë£? ë§¤ì¶œ?˜ ?‰ê·?",
                                                     "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ê´€ê°? ?ˆ˜?˜ ?‰ê·?",
                                                     "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ?ƒ?˜ ?šŸ?ˆ˜?˜ ?‰ê·?",
                                                     "?ƒ?˜ 1?šŒ ?‹¹ ë§¤ì¶œ?˜ ?‰ê·?",
                                                     "?ƒ?˜ 1?šŒ ?‹¹ ê´€ê°? ?ˆ˜?˜ ?‰ê·?",
                                                     "?ƒ?˜ê´€ 1ê´€ ?‹¹ ?ƒ?˜ ?šŸ?ˆ˜?˜ ?‰ê·?")
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
      h4(strong("Part 2. ê¸°ë¡?˜• ?†µê³? (ì´?, ìµœë?€)")),
      selectInput("category2", "ì§€?‘œ (?˜?™” 1?¸ ?‹¹)", choices = c("?•˜ë£? ìµœë?€ ë§¤ì¶œ?˜ ?‰ê·?",
                                                                 "ì´? ë§¤ì¶œ?˜ ?‰ê·?",
                                                                 "?•˜ë£? ìµœë?€ ê´€ê°? ?ˆ˜?˜ ?‰ê·?",
                                                                 "ì´? ê´€ê°? ?ˆ˜?˜ ?‰ê·?")
      ),
      strong("?¬ê°œë´‰ ?˜?™” ? œ?™¸?•œ ?°?´?„° ê¸°ë°˜"),
      p("ê¸°ê°„ ?‚´ ë°•ìŠ¤?˜¤?”¼?Š¤?— ì§„ì…?•œ ?‚ ì§œê?€ ê°œë´‰?¼ë¡œë?€?„° 365?¼?´ ì§€?‚œ ?‹œ? ?´ë©? ?¬ê°œë´‰ ?˜?™”ë¡? ê°„ì£¼?•¨."),
    ),
    
    mainPanel(
      plotOutput("plot2")
    )
    
  ),

  hr(),
  
  ### Part 3 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 3. ?˜?™” ì¹´í…Œê³ ë¦¬ë³? ?†µê³?")),
      selectInput("category3_1", "ë¶„ë¥˜ ê¸°ì?€", choices = c("êµ??‚´ ë°? ?•´?™¸ ?˜?™”", "êµ?ê°€ë³? ?˜?™”", "?¥ë¥´ë³„ ?˜?™”", "?¬ê°œë´‰ ë°? ì´ˆê°œë´? ?˜?™”")),
      selectInput("category3_2", "ì§€?‘œ", choices = c("?ƒ?˜ ?šŸ?ˆ˜", "?ƒ?˜ ? ?œ ?œ¨", "?ˆ˜", "? ?œ ?œ¨"))
    ),
    
    mainPanel(
      plotOutput("plot3")
    )
    
  ),
)


server <- function(input, output){
  ### Part 1 ###
  data1 <- reactive({
    if (input$category1_1 == "?›”ë³?"){
      if (input$category1_2 == "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ë§¤ì¶œ?˜ ?‰ê·?"){
        boxoffice_new[, mean(salesAmt), .(year, month)]
      }else if (input$category1_2 == "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ê´€ê°? ?ˆ˜?˜ ?‰ê·?"){
        boxoffice_new[, mean(audiCnt), .(year, month)]
      }else if (input$category1_2 == "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ?ƒ?˜ ?šŸ?ˆ˜?˜ ?‰ê·?"){
        boxoffice_new[, mean(showCnt), .(year, month)]
      }else if (input$category1_2 == "?ƒ?˜ 1?šŒ ?‹¹ ë§¤ì¶œ?˜ ?‰ê·?"){
        boxoffice_new[, mean(sales_per_show), .(year, month)]
      }else if (input$category1_2 == "?ƒ?˜ 1?šŒ ?‹¹ ê´€ê°? ?ˆ˜?˜ ?‰ê·?"){
        boxoffice_new[, mean(audi_per_show), .(year, month)]
      }else{
        boxoffice_new[, mean(show_per_scrn), .(year, month)]
      }
    }else{
      if (input$category1_2 == "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ë§¤ì¶œ?˜ ?‰ê·?"){
        boxoffice_new[, mean(salesAmt), year]
      }else if (input$category1_2 == "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ê´€ê°? ?ˆ˜?˜ ?‰ê·?"){
        boxoffice_new[, mean(audiCnt), year] 
      }else if (input$category1_2 == "?˜?™” 1?¸ ?‹¹ ?•˜ë£? ?ƒ?˜ ?šŸ?ˆ˜?˜ ?‰ê·?"){
        boxoffice_new[, mean(showCnt), year]
      }else if (input$category1_2 == "?ƒ?˜ 1?šŒ ?‹¹ ë§¤ì¶œ?˜ ?‰ê·?"){
        boxoffice_new[, mean(sales_per_show), year]
      }else if (input$category1_2 == "?ƒ?˜ 1?šŒ ?‹¹ ê´€ê°? ?ˆ˜?˜ ?‰ê·?"){
        boxoffice_new[, mean(audi_per_show), year]
      }else{
        boxoffice_new[, mean(show_per_scrn), year]
      }
    }
  })
  
  ylab1 <- reactive({
    if (str_detect(input$category1_2, "ë§¤ì¶œ?˜ ?‰ê·?")){
      "ë§¤ì¶œ (?›)"
    }else if (str_detect(input$category1_2, "ê´€ê°? ?ˆ˜?˜ ?‰ê·?")){
      "ê´€ê°? ?ˆ˜ (ëª?)"
    }else{
      "?ƒ?˜ ?šŸ?ˆ˜ (?¸)"
    }
  })
  
  plotting1 <- reactive({
    if (input$category1_1 == "?›”ë³?"){
      ggplot(data1(), aes(x = factor(month), y = V1, fill = factor(year))) +
        geom_col(position = "dodge") +
        xlab("?›”") +
        ylab(ylab1())
    }else{
      ggplot(data1(), aes(x = factor(year), y = V1, fill = factor(year))) +
        geom_col() +
        xlab("?—°?„") +
        ylab(ylab1())
    }
  })
  
  output$plot1 <- renderPlot({
    plotting1() +
      ggtitle(input$category1_2) +
      labs(fill = "?—°?„") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 2 ###
  data2 <- reactive({
    if (input$category2 == "?•˜ë£? ìµœë?€ ë§¤ì¶œ?˜ ?‰ê·?"){
      rmro[, .N, .(year, salesPeak, movieCd)][, mean(salesPeak), year]
    }else if (input$category2 == "ì´? ë§¤ì¶œ?˜ ?‰ê·?"){
      dt = rmro[, .N, .(year, salesTotal, movieCd)][, mean(salesTotal), year]
      dt[, V1 := V1 %>% as.numeric]
      return(dt)
    }else if (input$category2 == "?•˜ë£? ìµœë?€ ê´€ê°? ?ˆ˜?˜ ?‰ê·?"){
      rmro[, .N, .(year, audiPeak, movieCd)][, mean(audiPeak), year] 
    }else{
      rmro[, .N, .(year, audiTotal, movieCd)][, mean(audiTotal), year]
    }
  })
  
  ylab2 <- reactive({
    if (input$category2 == "?•˜ë£? ìµœë?€ ë§¤ì¶œ?˜ ?‰ê·?"){
      "?•˜ë£? ìµœë?€ ë§¤ì¶œ (?›)"
    }else if (input$category2 == "ì´? ë§¤ì¶œ?˜ ?‰ê·?"){
      "ì´? ë§¤ì¶œ (?›)"
    }else if (input$category2 == "?•˜ë£? ìµœë?€ ê´€ê°? ?ˆ˜?˜ ?‰ê·?"){
      "ìµœë?€ ê´€ê°? ?ˆ˜"
    }else{
      "ì´? ê´€ê°? ?ˆ˜"
    }
  })
  
  output$plot2 <- renderPlot({
    ggplot(data2(), aes(x = factor(year), y = V1, fill = factor(year))) +
      geom_col() +
      ggtitle(str_c("?˜?™” 1?¸ ?‹¹ ", input$category2)) +
      xlab("?—°?„") +
      ylab(ylab2()) +
      labs(fill = "?—°?„") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 3 ###
  var3 <- reactive({
    if (input$category3_1 == "êµ??‚´ ë°? ?•´?™¸ ?˜?™”"){
      "dom_or_for"
    }else if (input$category3_1 == "êµ?ê°€ë³? ?˜?™”"){
      "nation"
    }else if (input$category3_1 == "?¥ë¥´ë³„ ?˜?™”"){
      "genre"
    }else{
      "reopen"
    }
  })
  
  data3 <- reactive({
    if (input$category3_1 == "êµ??‚´ ë°? ?•´?™¸ ?˜?™”"){
      if (input$category3_2 == "?ƒ?˜ ?šŸ?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
      }else if (input$category3_2 == "?ƒ?˜ ? ?œ ?œ¨"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
      }else{
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "êµ?ê°€ë³? ?˜?™”"){
      if (input$category3_2 == "?ƒ?˜ ?šŸ?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, nation)]
      }else if (input$category3_2 == "?ƒ?˜ ? ?œ ?œ¨"){
        dt = boxoffice_new[, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
      }else{
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "?¥ë¥´ë³„ ?˜?™”"){
      if (input$category3_2 == "?ƒ?˜ ?šŸ?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, genre)]
      }else if (input$category3_2 == "?ƒ?˜ ? ?œ ?œ¨"){
        dt = boxoffice_new[, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
      }else{
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }
    }else{
      if (input$category3_2 == "?ƒ?˜ ?šŸ?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, reopen)]
      }else if (input$category3_2 == "?ƒ?˜ ? ?œ ?œ¨"){
        dt = boxoffice_new[, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?ˆ˜"){
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
      }else{
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }
      #dt[reopen == "ì´ˆê°œë´?" | reopen == "?¬ê°œë´‰"]
    }
    setnames(dt, var3(), "V1")
    return(dt)
  })
  
  output$plot3 <- renderPlot({
    if (str_detect(input$category3_2, "? ?œ ?œ¨")){
      p = ggplot(data3(), aes(x = factor(year), y = N, fill = V1)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent)
      xlab = "?—°?„"
      ylab = "? ?œ ?œ¨"
      if (str_detect(input$category3_1, "ë³?")){
        labs = str_replace(input$category3_1, "ë³? ?˜?™”", "")
      }else{
        labs = str_replace(input$category3_1, " ?˜?™”", "")
      }
    }else{
      p = ggplot(data3(), aes(x = V1, y = N, fill = factor(year))) +
        geom_col(position = "dodge")
      if (str_detect(input$category3_1, "ë³?")){
        xlab = str_replace(input$category3_1, "ë³? ?˜?™”", "")
      }else{
        xlab = str_replace(input$category3_1, " ?˜?™”", "")
      }
      ylab = "?ˆ˜ (?¸)"
      labs = "?—°?„"
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