# app.R
# This is the main Shiny app file that will be deployed to shinyapps.io
# It loads pre-computed results, dramatically reducing the bundle size

# Load necessary packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)

# Load pre-computed results (from data_preparation.R)
results <- readRDS("prediction_results.rds")
error_df <- results$error_df
metrics <- results$metrics

# Optional: Load sample movie data if available
if (file.exists("sample_movies.rds")) {
  sample_movies <- readRDS("sample_movies.rds")
}

# UI
ui <- navbarPage(
  title = "Movie Revenue Prediction & Recommender System Dashboard",
  theme = shinytheme("cosmo"),
  
  # Page 1: Summary
  tabPanel("Summary",
           fluidPage(
             titlePanel("Movie Analysis Summary"),
             
             # Display model metrics in cards
             fluidRow(
               column(3, 
                      div(style = "background-color:#f8f9fa; padding:15px; border-radius:5px; margin-bottom:20px;",
                          h4("R²", style = "text-align:center;"),
                          h3(sprintf("%.4f", metrics$r_squared), style = "text-align:center; color:#007bff;")
                      )
               ),
               column(3, 
                      div(style = "background-color:#f8f9fa; padding:15px; border-radius:5px; margin-bottom:20px;",
                          h4("MAPE", style = "text-align:center;"),
                          h3(sprintf("%.2f%%", metrics$mape), style = "text-align:center; color:#007bff;")
                      )
               ),
               column(3, 
                      div(style = "background-color:#f8f9fa; padding:15px; border-radius:5px; margin-bottom:20px;",
                          h4("Mean Abs Error", style = "text-align:center;"),
                          h3(paste0("$", format(round(metrics$mae/1e6), big.mark=",")), "M", 
                             style = "text-align:center; color:#007bff;")
                      )
               ),
               column(3, 
                      div(style = "background-color:#f8f9fa; padding:15px; border-radius:5px; margin-bottom:20px;",
                          h4("Sample Size", style = "text-align:center;"),
                          h3(format(nrow(error_df), big.mark=","), 
                             style = "text-align:center; color:#007bff;")
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      h4("Background:"),
                      p("The movie industry plays an important role in the cultural and economic aspects of the world. We believe that the development of movies can influence or show the trends of the world. With the advancements in technology, users can go to media and search for any movies they want. However, researchers, film companies, and users face challenges when they create a movie and search for movies. These challenges are, how to predict the revenue of movies; which kinds of movies would have a high probability of success at the box office; and if users explore the streaming media, how the algorithm behind the website recommends movies for them. Therefore, based on these problems, we build a multiplayer perceptron (MLP) neural network to predict the film's revenue. And implement content-based filtering to recommend movies for users based on similar overviews or plots of movies."),
                      
                      h4("Methods:"),
                      p("In this project, we created a multilayer perceptron (MLP) neural network to predict revenue. Since the TMDB movie dataset that we found from Kaggle is huge, we randomly extracted 10,000 samples from the dataset and applied them to our analysis. During the training process, we optimized our MLP by using regularization, dropout, and learning rate scheduling. Users can use TF-IDF vectorization and cosine similarity over combined text fields (overview, genres, and keywords). The content-based filtering method had been used to find thematically similar films. The recommendation system was built manually using Python. But a little bit different with the collaborative filtering we discussed in the class. When it comes to movie suggestions, content-based filtering implies recommending films that are comparable to ones that a user has already enjoyed by looking at elements like genre, actors, director, and narrative summaries."),
                      
                      h4("Findings:"),
                      p("Our revenue prediction model had an R² of ", strong(sprintf("%.4f", metrics$r_squared)), 
                        " and a MAPE of ", strong(sprintf("%.2f%%", metrics$mape)), 
                        ", with lower errors for high-budget films ($100M–$1B), and higher errors for low-budget films (less than $1M). These results indicate the model performed well on more tightly organized, bigstudio efforts. Moreover, the recommendations performed moderately well in generating suggestions. For example, if the user typed in \"Inception\", the system would list other similar film names. The algorithms behind the system are based on genres or keywords. Our results would show each recommended movies' similarity scores (0.07 – 0.18)."),
                      
                      h4("Conclusions:"),
                      p("Our analysis shows that machine learning techniques can be used to solve two main problems in movie industries: predicting box office and improving user experience in streaming media (better recommender system). Our project still has limitations. For example, we were not using all the data in the original dataset. In the future, we will compare another method to predict movie revenue, for example, deep neural networks with CNN. For the recommender system, we will combine user behavior data and other filtering methods to improve the efficiency of the recommender system."),
                      
                      br(),
                      div(style = "background-color:#f8f9fa; padding:15px; border-radius:5px; margin-bottom:20px;",
                          h4("Error by Revenue Group"),
                          tableOutput("groupMetricsTable")
                      ),
                      
                      p("* A recommender system, or a recommendation system, sometimes only called 'the algorithm' or 'algorithm' is a subclass of information filtering system that provides suggestions for items that are most pertinent to a particular user."),
                      p("* The second tab includes an interactive scatter plot to explore how well the model predicted revenue across various scales.")
               )
             )
           )
  ),
  
  # Page 2: Interactive Plot
  tabPanel("Actual vs Predicted",
           fluidPage(
             titlePanel("Actual vs. Predicted Revenue"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("group", "Select Revenue Group:",
                             choices = c("All", levels(error_df$revenue_group)),
                             selected = "All"),
                 checkboxInput("log_scale", "Use log scale", TRUE),
                 checkboxInput("show_line", "Show perfect prediction line", TRUE),
                 br(),
                 verbatimTextOutput("clickInfo"),
                 br(),
                 div(style = "font-size: 0.9em; color: #6c757d;",
                     "Click on any point in the scatter plot to see its details.")
               ),
               mainPanel(
                 plotlyOutput("scatterPlot", height = "600px")
               )
             )
           )
  ),
  
  # Page 3: Sample Movies (Optional, only if sample_movies.rds exists)
  if (exists("sample_movies")) {
    tabPanel("Sample Movies",
             fluidPage(
               titlePanel("Sample Movies from Dataset"),
               tableOutput("moviesTable")  # Changed from DT::dataTableOutput to regular tableOutput
             )
    )
  }
)

# Server
server <- function(input, output, session) {
  # Render group metrics table
  output$groupMetricsTable <- renderTable({
    metrics$group_metrics %>%
      mutate(
        mean_error_pct = sprintf("%.2f%%", mean_error_pct),
        count = format(count, big.mark = ",")
      ) %>%
      rename(
        "Revenue Group" = revenue_group,
        "Mean Error %" = mean_error_pct,
        "Count" = count
      )
  })
  
  # Filtered data reactive expression
  filtered_data <- reactive({
    if (input$group == "All") {
      error_df
    } else {
      filter(error_df, revenue_group == input$group)
    }
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = actual, y = predicted, color = revenue_group, text = paste0(
      "Actual: $", scales::comma(actual),
      "<br>Predicted: $", scales::comma(predicted),
      "<br>Group: ", revenue_group,
      "<br>Error %: ", round(abs(predicted - actual) / actual * 100, 1), "%"
    ))) +
      geom_point(alpha = 0.7, size = 3) +
      labs(
        title = "Actual vs Predicted Revenue",
        x = "Actual Revenue",
        y = "Predicted Revenue",
        color = "Revenue Group"
      ) +
      theme_minimal(base_size = 14)
    
    # Add diagonal line if checked
    if(input$show_line) {
      p <- p + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray")
    }
    
    # Apply scale based on user input
    if (input$log_scale) {
      p <- p +
        scale_x_log10(labels = dollar_format(scale = 1e-6, suffix = "M")) +
        scale_y_log10(labels = dollar_format(scale = 1e-6, suffix = "M"))
    } else {
      p <- p +
        scale_x_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
        scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))
    }
    
    # Convert to plotly with minimal configuration
    ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>%
      layout(autosize = TRUE)
  })
  
  # Point click info
  output$clickInfo <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      cat("Click on a point in the scatter plot to view details.")
    } else {
      clicked <- d$pointNumber + 1
      # Check bounds to prevent index errors
      if (clicked <= nrow(filtered_data())) {
        movie <- filtered_data()[clicked, ]
        cat(paste0(
          "Detail for Selected Point:\n",
          "Actual Revenue: $", format(round(movie$actual), big.mark = ","), "\n",
          "Predicted Revenue: $", format(round(movie$predicted), big.mark = ","), "\n",
          "Error %: ", round(abs(movie$predicted - movie$actual) / movie$actual * 100, 2), "%\n",
          "Revenue Group: ", as.character(movie$revenue_group)
        ))
      } else {
        cat("Point information not available.")
      }
    }
  })
  
  # Render sample movies table (optional) - using standard table instead of DT
  if (exists("sample_movies")) {
    output$moviesTable <- renderTable({
      sample_movies %>%
        mutate(
          release_date = as.Date(release_date),
          revenue = paste0("$", format(round(revenue/1e6, 1), big.mark = ","), "M"),
          budget = paste0("$", format(round(budget/1e6, 1), big.mark = ","), "M")
        )
    })
  }
  
  # Clean up resources when session ends
  session$onSessionEnded(function() {
    gc()
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)