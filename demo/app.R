library(surveydown)
library(dplyr)

# sd_db_config()
db <- sd_db_connect()

ui <- sd_ui()

server <- function(input, output, session) {

  # Generate a completion code for each respondent
  completion_code <- sd_completion_code(8)
  sd_store_value(completion_code)

  # Conditional skip logic - screen out people with no superhero interest
  sd_skip_forward(
    input$has_fav_hero == "no" ~ "screenout"
  )

  # Conditional question display - only show hero name if they have a favorite
  sd_show_if(
    input$has_fav_hero == "yes" ~ "fav_hero_name"
  )

  # Create a reactive summary of their hero preferences
  hero_summary <- sd_reactive("hero_summary", {
    paste("Based on your responses, you prefer",
          input$hero_universe, "heroes with qualities like",
          paste(input$hero_qualities, collapse = ", "))
  })

  # Main server function
  sd_server(
    db = db,
    required_questions = c("has_fav_hero", "hero_universe", "dream_power"),
    all_questions_required = FALSE,
    use_cookies = FALSE
  )
}

# Launch the survey
shiny::shinyApp(ui = ui, server = server)
