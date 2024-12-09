library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
#0. 
    # Job classification
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
                                  "Chef", "Technician", "Labourer", "Police")) {
        return("Class 3")
      } else {
        return("Declined")
      }}
    
    # State classification
    high <- c("indiana", "illinois", "nevada", "washington", "colorado")
    low <- c("maryland", "kansas", "missouri", "louisiana", "michigan", "arizona")

# data import
data <- read.csv("C:/Users/GAG01/OneDrive/바탕 화면/yonsei/2024-2/탐자분/Final project/data_final.csv")
data$Class <- sapply(data$job_title, classify_job)
data$job_category <- sapply(data$job_title, categorize_job)
data$regular_ex <- factor(data$regular_ex, levels = c(0, 1), labels = c("X", "O"))
data$smoker <- factor(data$smoker, levels = c(0, 1), labels = c("X", "O"))
data$diabetes <- factor(data$diabetes, levels = c(0, 1), labels = c("X", "O"))
data$hereditary_diseases <- ifelse(data$hereditary_diseases == "NoDisease", 
                                   "No hereditary diseases", 
                                   "Have hereditary diseases")
data$hereditary_diseases <- factor(data$hereditary_diseases, 
                                   levels = c("No hereditary diseases", "Have hereditary diseases"), labels = c("X", "O"))
data$state_group <- ifelse(data$states %in% high, "high",
                          ifelse(data$states %in% low, "low", "Other"))
data$no_of_dependents <- factor(data$no_of_dependents, levels = c(0, 1, 2, 3, 4, 5), labels = c( "0", "1","2", "3", "4", "5"))

# UI
ui <- fluidPage(
  titlePanel("Advanced Visualization with Group Options"),
  
  sidebarLayout(
    sidebarPanel(
      
      #selectInput("data_filter", "Filter Data:", 
                 #choices = c("All Data", "Filter by State Groups"), 
                 #selected = "All Data"),
      
      selectInput("x_var", "Select X-axis Variable:",
                  choices = c("claim", "bmi", "age", "weight", "bloodpressure", "None"),
                  selected = "age"),
      selectInput("y_var", "Select Y-axis Variable:",
                  choices = c("claim", "bmi", "age", "weight", "bloodpressure"),
                  selected = "claim"),
      checkboxInput("log_transform", "Log-transform Y-axis", value = FALSE),
      checkboxInput("x_interval_enable", "Enable X-axis Interval", value = FALSE),
      numericInput("x_interval_size", "X-axis Interval Size:", value = 10, min = 1, step = 1),

      selectInput("plot_type", "Select Plot Type:", choices = c("Scatterplot", "Boxplot (When x-asis is none)", "Violin plot (When x-asis is none)", "Density plot (When x-asis is none)")),
      selectInput("trendline", "Add Trendline:", choices = c("None", "Loess", "Linear Regression (lm)"), selected = "None"),

      checkboxInput("y_range_enable", "Enable Y-axis Range", value = FALSE),
      numericInput("y_min", "Y-axis Minimum:", value = 0, step = 1),
      numericInput("y_max", "Y-axis Maximum:", value = 60000, step = 1),
      
      checkboxGroupInput("comparison_groups", "1. Select Groups (For Faceting Plots):",
                         choices = list("Sex" = "sex",
                                        "Regular Exercise" = "regular_ex",
                                        "Smoker" = "smoker",
                                        "Diabetes" = "diabetes",
                                        "Hereditary Diseases" = "hereditary_diseases", #,"Group by states"= "state_group"
                                        "Number of dependents"="no_of_dependents",
                                        "Job Category (Occupational Class)" = "Class",
                                        "State Category (High, Low)" = "state_group")),
      checkboxGroupInput("color_groups", "2. Select Group (For Color Differentiation, Select only 1):",
                         choices = list("Sex" = "sex",
                                        "Regular Exercise" = "regular_ex",
                                        "Smoker" = "smoker",
                                        "Diabetes" = "diabetes",
                                        "Hereditary Diseases" = "hereditary_diseases", #,"Group by states"= "state_group"
                                        "Number of dependents"="no_of_dependents",
                                        "Job Category (Occupational Class)" = "Class",
                                        "State Category (High, Low)" = "state_group"
                                        ))),
    
    mainPanel(
      plotOutput("main_plot", width = "auto", height = "auto"),
      # 그래프 크기 조절 슬라이더를 그래프 아래로 이동
      sliderInput("plot_height", "Adjust Plot Height (px):", min = 400, max = 1200, value = 600),
      sliderInput("plot_width", "Adjust Plot Width (px):", min = 400, max = 1200, value = 800)
    )
  )
)

# Server 
server <- function(input, output) {
  
  processed_data <- reactive({
    filtered <- data
    
    # x-axis interval
    if (input$x_interval_enable && input$x_var != "None") {
      filtered <- filtered %>%
        mutate(x_interval = cut(!!sym(input$x_var),
                                breaks = seq(min(filtered[[input$x_var]], na.rm = TRUE),
                                             max(filtered[[input$x_var]], na.rm = TRUE) + input$x_interval_size,
                                             by = input$x_interval_size),
                                include.lowest = TRUE,
                                labels = paste0(
                                  seq(min(filtered[[input$x_var]], na.rm = TRUE), 
                                      max(filtered[[input$x_var]], na.rm = TRUE), 
                                      by = input$x_interval_size),
                                  "-",
                                  seq(min(filtered[[input$x_var]], na.rm = TRUE) + input$x_interval_size,
                                      max(filtered[[input$x_var]], na.rm = TRUE) + input$x_interval_size, 
                                      by = input$x_interval_size)
                                )))
    }
    
    return(filtered)
  })
  # graph
  output$main_plot <- renderPlot({
    filtered <- processed_data()
    
    # Log-transformation of y-variable if selected
    y_var <- input$y_var
    if (input$log_transform) {
      # Apply log transformation to y-axis variable
      filtered[[y_var]] <- log1p(filtered[[y_var]])  # log1p is used to handle 0 values correctly
      y_var <- paste("log(", y_var, "+1)", sep = "")  # Update the y-variable label
    }
    
    # main title
    main_title <- paste(input$y_var, "against", input$x_var)
    
    # Comparison group combination
    comparison_groups <- input$comparison_groups
    if (length(comparison_groups) > 0) {
      comparison_levels <- lapply(comparison_groups, function(group) unique(filtered[[group]]))
      comparison_combinations <- expand.grid(comparison_levels, KEEP.OUT.ATTRS = FALSE)
      colnames(comparison_combinations) <- comparison_groups
    } else {
      comparison_combinations <- data.frame(dummy = 1)
    }
    
    # Color group setting
    color_groups <- input$color_groups
    color_var <- if (length(color_groups) > 0) color_groups[1] else NULL
    
    # error message
    if (input$x_var == "None" && input$plot_type == "Scatterplot") {
      showNotification("Please select either Boxplot or Violin plot when X-axis is None.", type = "error")
      return(NULL)
    }
    
    # Making plots for all combinations
    plot_list <- list()
    for (i in seq_len(nrow(comparison_combinations))) {
      subset_data <- filtered
      
      # Comparison Groups filtering
      if (length(comparison_groups) > 0) {
        for (group in names(comparison_combinations)) {
          subset_data <- subset_data[subset_data[[group]] == comparison_combinations[[group]][i], ]
        }
      }
      
      
      if (nrow(subset_data) == 0) {
        next
      }
      
      # title
      if (length(comparison_groups) > 0) {
        subtitle <- paste(sapply(comparison_groups, function(group) {
          paste(group, comparison_combinations[i, group], sep = ": ")
        }), collapse = " | ")
      } else {
        subtitle <- "No Faceting Groups Selected"
      }
      
      # ggplot 
      if (input$x_var == "None") {
        
        # x-axis is none, Boxplot or Violin Plot
        if (input$plot_type == "Boxplot (When x-asis is none)") {
          p <- ggplot(subset_data, aes_string(
            x = if (!is.null(color_var)) color_var else "1", # color_var
            y = input$y_var, 
            fill = if (!is.null(color_var)) color_var else "1"
          )) +
            geom_boxplot(alpha = 0.6) +  
            stat_summary(fun = mean, geom = "point", shape = 21, size = 3.5, color = "black", fill = "white",stroke = 0.8) +  
            labs(title = subtitle, x = if (!is.null(color_var)) color_var else "Group") +
            theme_minimal(base_size = 10) +
            theme(
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
              axis.title.x = element_text(size = 14, face = "bold"),  
              axis.title.y = element_text(size = 14, face = "bold"),  
              axis.text.x = element_text(size = 12),  
              axis.text.y = element_text(size = 12),  
              legend.title = element_text(size = 14, face = "bold"),  
              legend.text = element_text(size = 13)  
            )
          
          
          if (!is.null(color_var)) {
            group_means <- subset_data %>%
              group_by_at(color_var) %>%  
              summarise(mean_value = mean(!!sym(input$y_var), na.rm = TRUE))  

            p <- p +
              geom_line(
                data = group_means,
                aes_string(x = color_var, y = "mean_value", group = 1),  
                color = "black",
                linetype = "dashed",
                size = 0.5,
                inherit.aes = FALSE  
              )
          }
          
        } else if (input$plot_type == "Violin plot (When x-asis is none)") {
          p <- ggplot(subset_data, aes_string(
            x = if (!is.null(color_var)) color_var else "1", 
            y = input$y_var, 
            fill = if (!is.null(color_var)) color_var else "1"
          )) +
            geom_violin(alpha = 0.6, scale = "width", trim = FALSE) +  
            stat_summary(fun = mean, geom = "point", shape = 21, size = 3.5, color = "black", fill = "white",stroke = 0.8) +  
            stat_summary(fun = median, geom = "crossbar", size = 0.4, color = "black") +  
            labs(title = subtitle, x = if (!is.null(color_var)) color_var else "Group") +
            theme_minimal(base_size = 10) +
            theme(
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
              axis.title.x = element_text(size = 14, face = "bold"),  
              axis.title.y = element_text(size = 14, face = "bold"),  
              axis.text.x = element_text(size = 12),  
              axis.text.y = element_text(size = 12),  
              legend.title = element_text(size = 14, face = "bold"),  
              legend.text = element_text(size = 13)  
            )
          
          if (!is.null(color_var)) {
            group_means <- subset_data %>%
              group_by_at(color_var) %>%  # color_var 
              summarise(mean_value = mean(!!sym(input$y_var), na.rm = TRUE))  
            p <- p +
              geom_line(
                data = group_means,
                aes_string(x = color_var, y = "mean_value", group = 1),  
                color = "black",
                linetype = "dashed",
                size = 0.5,
                inherit.aes = FALSE
              )
          }
          
        } else if (input$plot_type == "Density plot (When x-asis is none)") {
          p <- ggplot(subset_data, aes_string(
            x = input$y_var, 
            fill = if (!is.null(color_var)) color_var else "1" 
          )) +
            geom_density(alpha = 0.6, adjust = 1.2) +
            labs(
              title = subtitle, 
              x = input$y_var, 
              y = "Density", 
              fill = if (!is.null(color_var)) color_var else "Group"
            ) +
            theme_minimal(base_size = 10) +
            theme(
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
              axis.title.x = element_text(size = 14, face = "bold"),  
              axis.title.y = element_text(size = 14, face = "bold"),  
              axis.text.x = element_text(size = 12),  
              axis.text.y = element_text(size = 12),  
              legend.title = element_text(size = 14, face = "bold"),  
              legend.text = element_text(size = 13)  
            )
        }
      } else {
        p <- ggplot(subset_data, aes_string(
          x = input$x_var, y = input$y_var,
          color = if (!is.null(color_var)) color_var else NULL
        )) +
          geom_point(alpha = 0.6, size = 2) +
          labs(title = subtitle) +
          theme_minimal(base_size = 10) +
          theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
            axis.title.x = element_text(size = 14, face = "bold"),  
            axis.title.y = element_text(size = 14, face = "bold"), 
            axis.text.x = element_text(size = 12),  
            axis.text.y = element_text(size = 12),  
            legend.title = element_text(size = 14, face = "bold"),  
            legend.text = element_text(size = 13)  
          )
      }
      
      if (is.null(color_var)) {
        p <- p + theme(legend.position = "none")
      }
      

      if (input$trendline == "Loess" && input$x_var != "None") {
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.2)
      } else if (input$trendline == "Linear Regression (lm)" && input$x_var != "None") {
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.2)
      }
      
      if (input$y_range_enable) {
        p <- p + coord_cartesian(ylim = c(input$y_min, input$y_max))  # coord_cartesian robust mean
      }
      
      
      # Facet by x-intervals
      if (input$x_interval_enable && input$x_var != "None" && "x_interval" %in% colnames(subset_data)) {
        p <- p + facet_wrap(~x_interval, scales = "free_x")
      }
      
      plot_list <- append(plot_list, list(p)) 
    }
    
    # graph set
    if (length(plot_list) > 1) {
      grid.arrange(grobs = plot_list, ncol = 2, top = main_title) 
    } else if (length(plot_list) == 1) {
      print(plot_list[[1]] + ggtitle(main_title))
    } else {
      ggplot() + labs(title = main_title, subtitle = "No data available for selected filters") +
        theme_minimal(base_size = 10)
    }
  }, height = reactive({ input$plot_height }), width = reactive({ input$plot_width }))
}


shinyApp(ui = ui, server = server)

