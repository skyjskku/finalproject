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
  titlePanel(strong("코로?��19 ?��?���? ?��?�� �??�� 박스?��?��?�� ?��?��")),
  
  mainPanel(
    p("2019?�� ?��료는 2019?�� 3?�� 1?��부?�� 10?�� 31?��까�?�?�� ?��료이�?, 2020?�� ?��료는 2020?�� 3?�� 1?��부?�� 10?�� 31?��까�?�?�� ?��료로,
      2019?�� ?��료�?� 2020?�� ?��료는 각각 코로?��19 ?��?�� ?��기�?� 코로?��19 ?��?�� ?��기�?? ??�변?��?��.")
  ),

  ### Part 1 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 1. 개별 ?��?�� ?���?")),
      selectInput("category1_1", "?��?��", choices = c("?���?", "?��?���?")),
      selectInput("category1_2", "지?��", choices = c("?��?�� 1?�� ?�� ?���? 매출?�� ?���?",
                                                     "?��?�� 1?�� ?�� ?���? 관�? ?��?�� ?���?",
                                                     "?��?�� 1?�� ?�� ?���? ?��?�� ?��?��?�� ?���?",
                                                     "?��?�� 1?�� ?�� 매출?�� ?���?",
                                                     "?��?�� 1?�� ?�� 관�? ?��?�� ?���?",
                                                     "?��?��관 1관 ?�� ?��?�� ?��?��?�� ?���?")
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
      h4(strong("Part 2. 기록?�� ?���? (�?, 최�?�)")),
      selectInput("category2", "지?�� (?��?�� 1?�� ?��)", choices = c("?���? 최�?� 매출?�� ?���?",
                                                                 "�? 매출?�� ?���?",
                                                                 "?���? 최�?� 관�? ?��?�� ?���?",
                                                                 "�? 관�? ?��?�� ?���?")
      ),
      strong("?��개봉 ?��?�� ?��?��?�� ?��?��?�� 기반"),
      p("기간 ?�� 박스?��?��?��?�� 진입?�� ?��짜�?� 개봉?��로�?�?�� 365?��?�� 지?�� ?��?��?���? ?��개봉 ?��?���? 간주?��."),
    ),
    
    mainPanel(
      plotOutput("plot2")
    )
    
  ),

  hr(),
  
  ### Part 3 ###
  sidebarLayout(
    sidebarPanel(
      h4(strong("Part 3. ?��?�� 카테고리�? ?���?")),
      selectInput("category3_1", "분류 기�?�", choices = c("�??�� �? ?��?�� ?��?��", "�?가�? ?��?��", "?��르별 ?��?��", "?��개봉 �? 초개�? ?��?��")),
      selectInput("category3_2", "지?��", choices = c("?��?�� ?��?��", "?��?�� ?��?��?��", "?��", "?��?��?��"))
    ),
    
    mainPanel(
      plotOutput("plot3")
    )
    
  ),
)


server <- function(input, output){
  ### Part 1 ###
  data1 <- reactive({
    if (input$category1_1 == "?���?"){
      if (input$category1_2 == "?��?�� 1?�� ?�� ?���? 매출?�� ?���?"){
        boxoffice_new[, mean(salesAmt), .(year, month)]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� ?���? 관�? ?��?�� ?���?"){
        boxoffice_new[, mean(audiCnt), .(year, month)]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� ?���? ?��?�� ?��?��?�� ?���?"){
        boxoffice_new[, mean(showCnt), .(year, month)]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� 매출?�� ?���?"){
        boxoffice_new[, mean(sales_per_show), .(year, month)]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� 관�? ?��?�� ?���?"){
        boxoffice_new[, mean(audi_per_show), .(year, month)]
      }else{
        boxoffice_new[, mean(show_per_scrn), .(year, month)]
      }
    }else{
      if (input$category1_2 == "?��?�� 1?�� ?�� ?���? 매출?�� ?���?"){
        boxoffice_new[, mean(salesAmt), year]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� ?���? 관�? ?��?�� ?���?"){
        boxoffice_new[, mean(audiCnt), year] 
      }else if (input$category1_2 == "?��?�� 1?�� ?�� ?���? ?��?�� ?��?��?�� ?���?"){
        boxoffice_new[, mean(showCnt), year]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� 매출?�� ?���?"){
        boxoffice_new[, mean(sales_per_show), year]
      }else if (input$category1_2 == "?��?�� 1?�� ?�� 관�? ?��?�� ?���?"){
        boxoffice_new[, mean(audi_per_show), year]
      }else{
        boxoffice_new[, mean(show_per_scrn), year]
      }
    }
  })
  
  ylab1 <- reactive({
    if (str_detect(input$category1_2, "매출?�� ?���?")){
      "매출 (?��)"
    }else if (str_detect(input$category1_2, "관�? ?��?�� ?���?")){
      "관�? ?�� (�?)"
    }else{
      "?��?�� ?��?�� (?��)"
    }
  })
  
  plotting1 <- reactive({
    if (input$category1_1 == "?���?"){
      ggplot(data1(), aes(x = factor(month), y = V1, fill = factor(year))) +
        geom_col(position = "dodge") +
        xlab("?��") +
        ylab(ylab1())
    }else{
      ggplot(data1(), aes(x = factor(year), y = V1, fill = factor(year))) +
        geom_col() +
        xlab("?��?��") +
        ylab(ylab1())
    }
  })
  
  output$plot1 <- renderPlot({
    plotting1() +
      ggtitle(input$category1_2) +
      labs(fill = "?��?��") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 2 ###
  data2 <- reactive({
    if (input$category2 == "?���? 최�?� 매출?�� ?���?"){
      rmro[, .N, .(year, salesPeak, movieCd)][, mean(salesPeak), year]
    }else if (input$category2 == "�? 매출?�� ?���?"){
      dt = rmro[, .N, .(year, salesTotal, movieCd)][, mean(salesTotal), year]
      dt[, V1 := V1 %>% as.numeric]
      return(dt)
    }else if (input$category2 == "?���? 최�?� 관�? ?��?�� ?���?"){
      rmro[, .N, .(year, audiPeak, movieCd)][, mean(audiPeak), year] 
    }else{
      rmro[, .N, .(year, audiTotal, movieCd)][, mean(audiTotal), year]
    }
  })
  
  ylab2 <- reactive({
    if (input$category2 == "?���? 최�?� 매출?�� ?���?"){
      "?���? 최�?� 매출 (?��)"
    }else if (input$category2 == "�? 매출?�� ?���?"){
      "�? 매출 (?��)"
    }else if (input$category2 == "?���? 최�?� 관�? ?��?�� ?���?"){
      "최�?� 관�? ?��"
    }else{
      "�? 관�? ?��"
    }
  })
  
  output$plot2 <- renderPlot({
    ggplot(data2(), aes(x = factor(year), y = V1, fill = factor(year))) +
      geom_col() +
      ggtitle(str_c("?��?�� 1?�� ?�� ", input$category2)) +
      xlab("?��?��") +
      ylab(ylab2()) +
      labs(fill = "?��?��") +
      scale_fill_brewer(palette = "Pastel1") +
      mytheme
  })
  
  ### Part 3 ###
  var3 <- reactive({
    if (input$category3_1 == "�??�� �? ?��?�� ?��?��"){
      "dom_or_for"
    }else if (input$category3_1 == "�?가�? ?��?��"){
      "nation"
    }else if (input$category3_1 == "?��르별 ?��?��"){
      "genre"
    }else{
      "reopen"
    }
  })
  
  data3 <- reactive({
    if (input$category3_1 == "�??�� �? ?��?�� ?��?��"){
      if (input$category3_2 == "?��?�� ?��?��"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
      }else if (input$category3_2 == "?��?�� ?��?��?��"){
        dt = boxoffice_new[, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?��"){
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
      }else{
        dt = boxoffice_new[, .N, .(year, dom_or_for, movieCd)][, .N, .(year, dom_or_for)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "�?가�? ?��?��"){
      if (input$category3_2 == "?��?�� ?��?��"){
        dt = boxoffice_new[, .N, .(year, nation)]
      }else if (input$category3_2 == "?��?�� ?��?��?��"){
        dt = boxoffice_new[, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?��"){
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
      }else{
        dt = boxoffice_new[, .N, .(year, nation, movieCd)][, .N, .(year, nation)]
        dt[, N := (N/sum(N))]
      }
    }else if (input$category3_1 == "?��르별 ?��?��"){
      if (input$category3_2 == "?��?�� ?��?��"){
        dt = boxoffice_new[, .N, .(year, genre)]
      }else if (input$category3_2 == "?��?�� ?��?��?��"){
        dt = boxoffice_new[, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?��"){
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
      }else{
        dt = boxoffice_new[, .N, .(year, genre, movieCd)][, .N, .(year, genre)]
        dt[, N := (N/sum(N))]
      }
    }else{
      if (input$category3_2 == "?��?�� ?��?��"){
        dt = boxoffice_new[, .N, .(year, reopen)]
      }else if (input$category3_2 == "?��?�� ?��?��?��"){
        dt = boxoffice_new[, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }else if (input$category3_2 == "?��"){
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
      }else{
        dt = boxoffice_new[, .N, .(year, reopen, movieCd)][, .N, .(year, reopen)]
        dt[, N := (N/sum(N))]
      }
      #dt[reopen == "초개�?" | reopen == "?��개봉"]
    }
    setnames(dt, var3(), "V1")
    return(dt)
  })
  
  output$plot3 <- renderPlot({
    if (str_detect(input$category3_2, "?��?��?��")){
      p = ggplot(data3(), aes(x = factor(year), y = N, fill = V1)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent)
      xlab = "?��?��"
      ylab = "?��?��?��"
      if (str_detect(input$category3_1, "�?")){
        labs = str_replace(input$category3_1, "�? ?��?��", "")
      }else{
        labs = str_replace(input$category3_1, " ?��?��", "")
      }
    }else{
      p = ggplot(data3(), aes(x = V1, y = N, fill = factor(year))) +
        geom_col(position = "dodge")
      if (str_detect(input$category3_1, "�?")){
        xlab = str_replace(input$category3_1, "�? ?��?��", "")
      }else{
        xlab = str_replace(input$category3_1, " ?��?��", "")
      }
      ylab = "?�� (?��)"
      labs = "?��?��"
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