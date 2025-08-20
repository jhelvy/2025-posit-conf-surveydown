library(surveydown)
library(dplyr)
library(ggplot2)
library(cowplot)

# sd_db_config()
db <- sd_db_connect()

ui <- sd_ui()

server <- function(input, output, session) {

  # Refresh data every 5 seconds
  data <- sd_get_data(db, refresh_interval = 5)

  # Render the plot
  output$hero_plot <- renderPlot({
    data() |> # Note the () here, as this is a reactive expression
      count(fav_hero_name) |>
      mutate(
        fav_hero_name = ifelse(is.na(fav_hero_name), 'No response', fav_hero_name),
        fav_hero_name = ifelse(fav_hero_name == '', 'No response', fav_hero_name)
      ) |>
      ggplot() +
      geom_col(aes(x = n, y = reorder(fav_hero_name, n)), width = 0.7) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme_minimal_vgrid(font_size = 12, font_family = 'Fira Sans Condensed') +
      labs(x = "Count", y = "Hero name", title = "Favorite Hero Count")
  }, height = 600, res = 150)

  # Main server function
  sd_server(
    db = db,
    all_questions_required = TRUE,
    use_cookies = FALSE
  )
}

# Launch the survey
shiny::shinyApp(ui = ui, server = server)
