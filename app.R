# Now we add theming - the fun part!
# Also added scrollable side panel - convenient!

test tes

# Load libraries - add bslib. I also needed to update shiny.

# After this step, publish to shinyapps.io to make sure it works.

library(shiny)       # for app
library(tidyverse)   # for plotting and wrangling
library(tidymodels)  # for modeling
library(ranger)      # for random forest
library(bslib)       # for theming

data("lending_club")

# This time we need this for predicting.
lending_mod <- read_rds("rf_final.rds")

# Find unique states and put them in alphabetical order:
states <- 
  lending_club  %>% 
  select(addr_state) %>% 
  distinct(addr_state) %>% 
  arrange(addr_state) %>% 
  pull(addr_state)

# Fix employment length

emp_len <- 
  lending_club %>% 
  select(emp_length) %>% 
  distinct() %>% 
  mutate(emp_length = fct_relevel(emp_length,
                                  "emp_ge_10", 
                                  after = 10)) %>% 
  arrange(emp_length) %>% 
  pull(emp_length)


# Find min's, max's, and median's for quantitative vars:

stats_num <-
  lending_club  %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(variable) %>% 
  summarize(min_val = min(value),
            max_val = max(value),
            med_val = median(value))

# Used for creating observation. Run and then copy and paste, without Class.  
# Works because I used variable name as InputId.

# paste(names(lending_club), 
#       " = input$", 
#       names(lending_club),
#       sep = "",
#       collapse = ", ")

# Define UI 
# NOTE: I haven't made all the labels look nice - I should.

ui <- fluidPage(
  theme = bs_theme(primary = "#123B60", 
                   secondary = "#D44420", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                   "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                   "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                   "Segoe UI Symbol"), 
                   bootswatch = "sketchy"),
  # Application title
  titlePanel("Ceteris Perabus Profile"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      # added this for scrollable side panel:
      tags$head(tags$style(
        type = 'text/css',
        'form.well { max-height: 600px; overflow-y: auto; }'
      )),
      sliderInput(inputId = "acc_now_delinq",
                  label = "Number of accounts delinquent:",
                  min = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "all_util",
                  label = "Balance to credit limit:",
                  min = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(min_val) %>% 
                    round(digits = -1),
                  max = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(max_val) %>% 
                    round(digits = -1),
                  value = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(med_val) %>% 
                    round(digits = -1), 
                  step = 10),
      sliderInput(inputId = "annual_inc",
                  label = "Annual income:",
                  min = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      sliderInput(inputId = "delinq_2yrs",
                  label = "# of 30+ days past-due incideces for past 2 yrs:",
                  min = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "delinq_amnt",
                  label = "Past due amount owed:",
                  min = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(med_val), 
                  step = 1000, 
                  round = TRUE),
      sliderInput(inputId = "funded_amnt",
                  label = "Funded amount:",
                  min = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(min_val) %>% 
                    round(digits = -3),
                  max = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(max_val) %>% 
                    round(digits = -3),
                  value = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(med_val) %>% 
                    round(digits = -3), 
                  step = 1000),
      sliderInput(inputId = "inq_fi",
                  label = "inq_fi:",
                  min = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "inq_last_12m",
                  label = "inq_last_12m:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "inq_last_6mths",
                  label = "inq_last_6mths:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "num_il_tl",
                  label = "num_il_tl:",
                  min = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_12m",
                  label = "open_il_12m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_24m",
                  label = "open_il_24m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_6m",
                  label = "open_il_6m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "revol_util",
                  label = "Revolving line utilization rate:",
                  min = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(med_val), 
                  step = 2, 
                  round = TRUE),
      sliderInput(inputId = "int_rate",
                  label = "Interest rate:",
                  min = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = .1, 
                  round = TRUE),
      sliderInput(inputId = "total_bal_il",
                  label = "Total current balance of installment accts:",
                  min = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      sliderInput(inputId = "total_il_high_credit_limit",
                  label = "Total installments high credit/credit limit:",
                  min = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      selectInput(inputId = "addr_state", 
                  label = "State:", 
                  choices = states),
      selectInput(inputId = "emp_length", 
                  label = "Employment length:", 
                  choices = emp_len),
      selectInput(inputId = "sub_grade", 
                  label = "LC assigned loan subgrade:", 
                  choices = lending_club %>% 
                    select(sub_grade) %>% 
                    distinct() %>% 
                    arrange(sub_grade) %>% 
                    pull(sub_grade)),
      selectInput(inputId = "term", 
                  label = "Term:", 
                  choices = lending_club %>% 
                    select(term) %>% 
                    distinct() %>% 
                    arrange(term) %>% 
                    pull(term)),
      selectInput(inputId = "verification_status", 
                  label = "Verification Status:", 
                  choices = lending_club %>% 
                    select(verification_status) %>% 
                    distinct() %>% 
                    arrange(verification_status) %>% 
                    pull(verification_status)),
      selectInput(inputId = "var",
                  label = "Variable to vary in the plot:",
                  choices = list(Income = "annual_inc",
                                 `Interest rate` =  "int_rate",
                                 `Funded amount` = "funded_amnt")),
      submitButton(text = "Create the CP profile"),
    ),
    
    # Show a cp plot for this observation 
    # Make sure you have the plotOutput!
    mainPanel(
      plotOutput("cp_plot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$cp_plot <- renderPlot({
    
      obs <- tibble(funded_amnt = input$funded_amnt, 
                    term = input$term, 
                    int_rate = input$int_rate, 
                    sub_grade = input$sub_grade, 
                    addr_state = input$addr_state, 
                    verification_status = input$verification_status, 
                    annual_inc = input$annual_inc, 
                    emp_length = input$emp_length, 
                    delinq_2yrs = input$delinq_2yrs, 
                    inq_last_6mths = input$inq_last_6mths, 
                    revol_util = input$revol_util, 
                    acc_now_delinq = input$acc_now_delinq, 
                    open_il_6m = input$open_il_6m, 
                    open_il_12m = input$open_il_12m, 
                    open_il_24m = input$open_il_24m, 
                    total_bal_il = input$total_bal_il, 
                    all_util = input$all_util, 
                    inq_fi = input$inq_fi, 
                    inq_last_12m = input$inq_last_12m, 
                    delinq_amnt = input$delinq_amnt,
                    num_il_tl = input$num_il_tl, 
                    total_il_high_credit_limit = input$total_il_high_credit_limit)
    
      obs_many <- obs %>% 
      sample_n(size = 50, replace = TRUE) %>% 
      # The .data[[input$var]] refers to the actual variable rather
      # than the quoted variable, eg. annual_inc vs. "annual_inc"
      select(-.data[[input$var]]) %>% 
      # Change name from annual_inc to var_of_interest
      mutate(var_of_interest = seq(stats_num %>% 
                                # here we want the quoted variable
                                filter(variable == input$var) %>% 
                                pull(min_val), 
                              stats_num %>% 
                                filter(variable == input$var) %>% 
                                pull(max_val), 
                              length.out = 50)) %>% 
        # This renames the variable its original name
        set_names(ifelse(names(.) == "var_of_interest", 
                         input$var, 
                         names(.)))
      
    # funded_amt needs to be an integer ... still not sure why
    # There is probably a more elegant way of doing this
      if (input$var == "funded_amnt") {
        obs_many %>% 
          select(.data[[input$var]]) %>% 
          bind_cols(
            predict(lending_mod,
                    new_data = obs_many %>% 
                      mutate(across(.data[[input$var]], 
                                    ~as.integer(round(.x)))), 
                    type = "prob")
          ) %>% 
          ggplot(aes(x = .data[[input$var]],
                     y = .pred_good)) +
          geom_line() +
          labs(title = "Predicted probability of loan fully paid back \nor currently on-time",
               y = NULL) +
          theme_minimal()
      } else {
    obs_many %>% 
      select(.data[[input$var]]) %>% 
      bind_cols(
        predict(lending_mod,
                new_data = obs_many, 
                type = "prob")
      ) %>% 
      ggplot(aes(x = .data[[input$var]],
                 y = .pred_good)) +
      geom_line() +
      labs(title = "Predicted probability of loan fully paid back \nor currently on-time",
           y = NULL) +
      theme_minimal()
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
