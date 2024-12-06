library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
#0. 직업분류
    classify_job <- function(job_title) {
      if (job_title %in% c("CEO", "Architect", "Accountant", "Journalist", "Manager", 
                           "ITProfessional", "FilmMaker", "DataScientist", "Clerks", 
                           "Lawyer", "Buisnessman", "CA", "Actor", "Politician", 
                           "Singer", "Academician", "FashionDesigner", "HomeMakers", 
                           "Doctor", "Student", "FilmDirector","Analyst")) {
        return("Class 1")
      } else if (job_title %in% c("Engineer", "Dancer", "Photographer", "Beautician", 
                                  "Blogger", "GovEmployee")) {
        return("Class 2")
      } else if (job_title %in% c("HouseKeeper", "Farmer", "DefencePersonnels", 
                                  "Chef", "Technician", "Labourer")) {
        return("Class 3")
      } else {
        return("Declined")
      }}
    
    categorize_job <- function(job_title) {
      if (job_title %in% c("Engineer", "Dancer", "Photographer", "FilmMaker", "Actor", 
                           "Police", "Farmer", "DefencePersonnels", "Chef", "Labourer", 
                           "HouseKeeper", "Technician", "Beautician", "Singer", "FashionDesigner")) {
        return("Field")
      } else if (job_title %in% c("CEO", "Architect", "Accountant", "Journalist", "Manager", 
                                  "ITProfessional", "DataScientist", "Clerks", "Lawyer", 
                                  "Buisnessman", "CA", "Politician", "Academician", "Doctor", 
                                  "FilmDirector", "GovEmployee", "Student", "Analyst", "Blogger", "HomeMakers")) {
        return("Office")
      } else {
        return("Unknown")
      }
    }
# 데이터 불러오기
data <- read.csv("C:/Users/GAG01/OneDrive/바탕 화면/yonsei/2024-2/탐자분/Final project/data_final.csv")
data$Class <- sapply(data$job_title, classify_job)
data$job_category <- sapply(data$job_title, categorize_job)
data$regular_ex <- factor(data$regular_ex, levels = c(0, 1), labels = c("Non-Exerciser", "Exerciser"))
data$smoker <- factor(data$smoker, levels = c(0, 1), labels = c("Non-Smoker", "Smoker"))

# UI 정의
ui <- fluidPage(
  titlePanel("Advanced Visualization with Group Options"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X-axis Variable:",
                  choices = c("claim", "bmi", "age", "weight", "bloodpressure", "None"),
                  selected = "age"),
      selectInput("y_var", "Select Y-axis Variable:",
                  choices = c("claim", "bmi", "age", "weight", "bloodpressure"),
                  selected = "claim"),
      
      checkboxInput("x_interval_enable", "Enable X-axis Interval", value = FALSE),
      numericInput("x_interval_size", "X-axis Interval Size:", value = 10, min = 1, step = 1),
      checkboxInput("y_interval_enable", "Enable Y-axis Interval", value = FALSE),
      numericInput("y_interval_size", "Y-axis Interval Size:", value = 10, min = 1, step = 1),
      
      checkboxInput("y_range_enable", "Enable Y-axis Range", value = FALSE),
      numericInput("y_min", "Y-axis Minimum:", value = 0, step = 1),
      numericInput("y_max", "Y-axis Maximum:", value = 60000, step = 1),
      
      checkboxGroupInput("comparison_groups", "Select Comparison Groups (For Faceting Plots):",
                         choices = list("Job Category (Occupational Class)" = "Class",
                                        "Job Category (Work Environment)" = "job_category",
                                        "Sex" = "sex",
                                        "Regular Exercise" = "regular_ex",
                                        "Smoker" = "smoker")),
      checkboxGroupInput("color_groups", "Select Color Groups (For Differentiation):",
                         choices = list("Job Category (Occupational Class)" = "Class",
                                        "Job Category (Work Environment)" = "job_category",
                                        "Sex" = "sex",
                                        "Regular Exercise" = "regular_ex",
                                        "Smoker" = "smoker")),
      
      selectInput("plot_type", "Select Plot Type:", choices = c("Scatterplot", "Boxplot")),
      selectInput("trendline", "Add Trendline:", choices = c("None", "Loess", "Linear Regression (lm)"), selected = "None"),
      
      sliderInput("plot_height", "Adjust Plot Height (px):", min = 400, max = 1200, value = 600),
      sliderInput("plot_width", "Adjust Plot Width (px):", min = 400, max = 1200, value = 800)
    ),
    
    mainPanel(
      plotOutput("main_plot", width = "auto", height = "auto")
    )
  )
)

# Server 로직 정의
server <- function(input, output) {
  
  # 동적 데이터 처리
  processed_data <- reactive({
    filtered <- data
    
    # x축 구간화
    if (input$x_interval_enable && input$x_var != "None") {
      filtered <- filtered %>%
        mutate(x_interval = cut(!!sym(input$x_var),
                                breaks = seq(min(filtered[[input$x_var]], na.rm = TRUE),
                                             max(filtered[[input$x_var]], na.rm = TRUE) + input$x_interval_size,
                                             by = input$x_interval_size),
                                include.lowest = TRUE))
    }
    
    # y축 구간화
    if (input$y_interval_enable) {
      filtered <- filtered %>%
        mutate(y_interval = cut(!!sym(input$y_var),
                                breaks = seq(min(filtered[[input$y_var]], na.rm = TRUE),
                                             max(filtered[[input$y_var]], na.rm = TRUE) + input$y_interval_size,
                                             by = input$y_interval_size),
                                include.lowest = TRUE))
    }
    
    return(filtered)
  })
  
  # 그래프 생성
  output$main_plot <- renderPlot({
    filtered <- processed_data()
    
    # 큰 제목 설정
    main_title <- paste(input$y_var, "against", input$x_var)
    
    # Comparison 그룹 조합 생성
    comparison_groups <- input$comparison_groups
    if (length(comparison_groups) > 0) {
      comparison_levels <- lapply(comparison_groups, function(group) unique(filtered[[group]]))
      comparison_combinations <- expand.grid(comparison_levels, KEEP.OUT.ATTRS = FALSE)
      colnames(comparison_combinations) <- comparison_groups
    } else {
      comparison_combinations <- data.frame(dummy = 1)
    }
    
    # Color 그룹 설정
    color_groups <- input$color_groups
    color_var <- if (length(color_groups) > 0) paste(color_groups, collapse = "+") else NULL
    
    
    # 모든 조합에 대해 그래프 생성
    plot_list <- list()
    for (i in seq_len(nrow(comparison_combinations))) {
      subset_data <- filtered
      
      # Comparison Groups 필터링
      if (length(comparison_groups) > 0) {
        for (group in names(comparison_combinations)) {
          subset_data <- subset_data[subset_data[[group]] == comparison_combinations[[group]][i], ]
        }
      }
      
      # 빈 데이터 예외 처리
      if (nrow(subset_data) == 0) {
        next
      }
      
      # 소제목 생성
      if (length(comparison_groups) > 0) {
        subtitle <- paste(sapply(comparison_groups, function(group) {
          paste(group, comparison_combinations[i, group], sep = ": ")
        }), collapse = " | ")
      } else {
        subtitle <- "No Faceting Groups Selected"
      }
      
      # 기본 ggplot 객체 생성
      p <- ggplot(subset_data, aes_string(
        x = ifelse(input$x_var == "None", NA, input$x_var),
        y = input$y_var,
        color = color_var
      )) +
        geom_point(alpha = 0.6, size = 2) +
        labs(title = subtitle) +
        theme_minimal(base_size = 10)
      
      
      
      
      
      # 트렌드 라인 추가
      if (input$trendline == "Loess" && input$x_var != "None") {
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.2)
      } else if (input$trendline == "Linear Regression (lm)" && input$x_var != "None") {
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.2)
      }
      
      # y축 범위 설정
      if (input$y_range_enable) {
        p <- p + ylim(input$y_min, input$y_max)
      }
      
      plot_list <- append(plot_list, list(p))
    }
    
    # 그래프 배치
    if (length(plot_list) > 1) {
      grid.arrange(grobs = plot_list, ncol = 2, top = main_title) # 큰 제목 설정
    } else if (length(plot_list) == 1) {
      print(plot_list[[1]] + ggtitle(main_title))
    } else {
      ggplot() + labs(title = main_title, subtitle = "No data available for selected filters") +
        theme_minimal(base_size = 10)
    }
  }, height = reactive({ input$plot_height }), width = reactive({ input$plot_width }))
}

# 앱 실행
shinyApp(ui = ui, server = server)

