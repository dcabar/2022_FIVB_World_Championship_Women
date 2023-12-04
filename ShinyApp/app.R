## Final Project (Assignment 12) - DATA 824 - Damjana V. Cabarkapa  --------------

# Loading packages
library(shiny)
library(tidyverse)
library(DT)

# Reading the data
full_dat <- read_csv("2022_FIVB_VolleyballRound1results.csv", show_col_types = F)
str(full_dat)
full_dat

# Cleaning the existing data to only keep the variables we are interested in for the table
table_dat <- full_dat %>% 
  select(Team1, Team2, Team1_Score, Team2_Score, Winner)
table_dat

# Creating the data for the plot
# First, we create a unique game ID variable for ease of selecting which variables we want to plot
plot_dat <- full_dat %>% 
  select(date, Team1, Team2, contains("set")) %>% 
  mutate(game_id = paste0(date, ": ", Team1, " vs. ", Team2),
         .before = date)

# Then, we pivot the plot data longer for ease in plotting
plot_dat2 <- plot_dat %>% 
  select(-date) %>% 
  pivot_longer(cols = team1_set1_score:team2_set5_score,
               names_to = c("team", "set", "uselessscore"),
               names_sep = "_",
               values_to = "score") %>% 
  select(-uselessscore) %>% 
  # now for plotting purposes, we create team  variable that corresponds to what team1 
  # and team 2 is
  mutate(Team_Actual = case_when(team == "team1" ~ Team1,
                                 team == "team2" ~ Team2),
         .before = "set")

# Making the plot look more presentable
plot_dat3 <- plot_dat2 %>% 
  select(game_id, Team = Team_Actual, Set = set, Score = score) %>% 
  mutate(Set = str_replace_all(Set, "set", "Set "))
plot_dat3

# Lastly, we are defining the plot themes
theme_set(theme_minimal())
theme_update(axis.text.y = element_text(size = 20, face = "bold",
                                        color = "black"),
             axis.text.x = element_text(size = 20, face = "bold",
                                        color = "black"),
             axis.title.y = element_text(size = 20, face = "bold",
                                         color = "black"),
             legend.position = "top",
             axis.title.x = element_blank(),
             text = element_text(size = 20, face = "bold",
                                 color = "black"))


# Making a shinny app ----------
ui <- fluidPage(
  
  # Application title
  titlePanel("2022 FIVB Volleyball Match Data"),
  
  # using tabset panel
  tabsetPanel(
    id = "full_results",
    tabPanel(
      title = "Full results of matches",
      DTOutput("results_tbl")
    ),
    tabPanel(title = "Single Game Plot",
             sidebarPanel(
               selectInput(
                 inputId = "selected_game_id",
                 label = "Select game of interest",
                 choices = sort(unique(plot_dat3$game_id)),
                 multiple = F, 
                 selected = NULL)),
             br(),
             br(),
             br(),
             br(),
             mainPanel(
               plotOutput("game_plt"),
               width = 12
             )
    )
  )
)

# Defining server logic 
server <- function(input, output) {
  
  output$results_tbl <- renderDT({
    table_dat %>% 
      set_names(c("Team 1", "Team 2", "Team 1 Score", "Team 2 Score", "Match Winner")) %>% 
      datatable()
  })
  
  output$game_plt <- renderPlot({
    plot_dat3 %>% 
      filter(game_id == input$selected_game_id) %>% 
      drop_na() %>% 
      ggplot(aes(x = Set,
                 y = Score,
                 group = Team,
                 fill = Team)) +
      geom_col(position = position_dodge(0.8),
               width = 0.8,
               color = "black") +
      geom_text(aes(label = Score),
                vjust = 1.5,
                fontface = "bold",
                color = "black",
                position = position_dodge(0.8),
                size = 16) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
      scale_fill_brewer(type = "qual") +
      theme(legend.position = "top")
  })
}

# Running the application 
shinyApp(ui = ui, server = server)

