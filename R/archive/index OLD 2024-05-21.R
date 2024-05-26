```{r, include=FALSE}
# Load the required libraries
library(tidyverse)
library(googlesheets4)
library(wesanderson)  # Color palettes
library(gridExtra)
library(grid)
library(cowplot)
library(glue)

# Define input variables
data_url <- 'https://docs.google.com/spreadsheets/d/1CpXpoodchi_0ui6dQRNzxE2Dl4VO9b0W2zzWzZ5j8nA/edit#gid=0'
month_start_date <- "2024-05-01"
month_end_date <- "2024-05-31"
week_start_date <- "2024-05-19"
week_end_date <- "2024-05-25"

# Function to process data
process_data <- function(start_date, end_date) {
  # Read google sheets data into R
  daily_workout_tbl <- read_sheet(data_url)

  # Transform and summarize data
  daily_workout_tbl %>%
    pivot_longer(
      cols = starts_with("workouts_"),
      names_to = "Person",
      values_to = "Workouts",
      names_prefix = "workouts_"
    ) %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date)) %>%
    group_by(Person) %>%
    summarize(Total_Workouts = sum(Workouts, na.rm = TRUE)) %>%
    arrange(desc(Total_Workouts)) %>%
    mutate(
      Rank = row_number(),
      Person = glue("{Rank}. {Person}"),
      Person = fct_inorder(Person) %>% fct_rev()
    )
}

# Process data for the full month and the specific week
full_data <- process_data(month_start_date, month_end_date)
filtered_week <- process_data(week_start_date, week_end_date)

```


```{r}

# Helper function to format dates in ordinal style (1st, 2nd, 3rd, etc.)
ordinal_date <- function(date) {
  suffix <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  day <- as.integer(format(date, "%d"))
  month <- format(date, "%B")
  paste0(month, " ", day, suffix[day %% 10 + 1 * (day < 10 | day > 20)])
}


# Function to create plots
create_plot <- function(data, title) {
  ggplot(data, aes(x = Person, y = Total_Workouts, fill = Person)) +
    geom_col(show.legend = FALSE, width = 0.7) +
    scale_fill_manual(values = wes_palette("GrandBudapest2", n = length(unique(data$Person)), type = "continuous")) +
    labs(x = "", y = "# of workouts", title = title) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(hjust = 0),
          axis.title.x = element_text(margin = margin(t = 10, b = 10))) +
    coord_flip()
}

# Create the plots with dynamic titles using glue and formatted dates
weekly_plot <- create_plot(filtered_week, glue::glue("{ordinal_date(as.Date(week_start_date))} to {ordinal_date(as.Date(week_end_date))} Workout Leaderboard"))
full_plot <- create_plot(full_data, glue::glue("{ordinal_date(as.Date(month_start_date))} to {ordinal_date(as.Date(month_end_date))} Workout Leaderboard"))


library(gridExtra)
library(grid)
library(gtable)

# Assuming that you have already created `weekly_plot` and `full_data`
# Create a table grob from full_data
full_data_table_grob <- tableGrob(full_data)

# Create a title for the table grob using textGrob
table_title <- textGrob("Full Data Summary", gp = gpar(fontsize = 12, fontface = "bold"), vjust = 0.5)

# Construct a gtable for the titled table
titled_table_grob <- gtable::gtable(
  widths = unit(1, "npc"),
  heights = unit.c(unit(1, "lines"), unit(1, "npc"))
)

# Add the title and table grob to the gtable
titled_table_grob <- gtable::gtable_add_grob(
  titled_table_grob,
  grobs = list(table_title, full_data_table_grob),
  t = c(1, 2), l = c(1, 1), b = c(1, 2), r = c(1, 1)
)

# Adjust the row heights if necessary to ensure no overlap
titled_table_grob$heights <- unit.c(unit(10, "lines"), unit(1, "npc")) # Give more space to the title

# Use plot_grid to combine the weekly plot and the titled table, ensuring vertical alignment
combined_grob <- plot_grid(weekly_plot, titled_table_grob, ncol = 1, align = 'v', rel_heights = c(1, 1.5))

combined_grob
# Save the image with adjusted dimensions for better mobile viewing
# Setting the width smaller and the height larger for a vertical format
ggsave(glue::glue("figures/workout_summary_{week_start_date}-{week_end_date}.png"), combined_grob, width = 6, height = 12, dpi = 300, bg = "white")


```


