#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#### Loading Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

bank_train <- read.csv("BankCamp_train.csv", header = T)
bank_test <- read.csv("BankCamp_test.csv", header = T)

# Since the test set has no target column, we will create one before combining both
bank_test$y <- NA

# We will add a column with a factor of train/test to help with the dataset splitting further
bank_train$train_test <- "train"
bank_test$train_test <- "test"

# Combining both datasets
bank_dataset <- rbind(bank_train, bank_test)
cat("The train set has ", dim(bank_train)[1], "rows and ", dim(bank_train)[2], "columns, ")
cat("The test set has ", dim(bank_test)[1], "rows and ", dim(bank_test)[2], "columns, ")
cat("The combined dataset has ", dim(bank_dataset)[1], "rows and ", dim(bank_dataset)[2], "columns")


# Define UI for application that draws a histogram

ui <- dashboardPage(
  
  #Header of the dashboard
  dashboardHeader(title = "Bank Campaign Exploratory Data Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bank Campaign EDA", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dataset Description", icon = icon("cog",lib='glyphicon'), 
               href = "http://archive.ics.uci.edu/ml/datasets/Bank+Marketing#"),
      menuItem("Model Results", icon = icon("send", lib = "glyphicon"), 
               href = "http://127.0.0.1:5378")
      
    )
  ),
  
  dashboardBody(

    frow2 <- fluidRow( 
      box(
        title = "Pie Chart"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("pieChart", height = "300px")
      )
      ,box(
        title = "Class Proportions"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("classProp", height = "300px")
      ),
      frow3 <- fluidRow(
        box(
          title = "Job Count"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("jobCount", height = "300px")
        )
        ,box(
          title = "Job Proportions vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("job2", height = "300px")
        )
        ,box(
          title = "Marital Count"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("maritalCount", height = "300px")
        )
        ,box(
          title = "Marital Proportions vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("marital2", height = "300px")
        )
        ,box(
          title = "Age Distribution"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("agedistribution", height = "300px")
        )
        ,box(
          title = "Age Groups"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("agegroup", height = "300px")
        )
        ,box(
          title = "Age Groups vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("agecounts", height = "300px")
        )
        ,box(
          title = "Education Groups"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("educationcount", height = "300px")
        )
        ,box(
          title = "Education Groups vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("educationcount2", height = "300px")
        )
        ,box(
          title = "Default"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("default", height = "300px")
        )
        ,box(
          title = "Default vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("defaultcount", height = "300px")
        )
        ,box(
          title = "Housing Counts"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("housing", height = "300px")
        )
        ,box(
          title = "Housing vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("housingcount", height = "300px")
        )
        ,box(
          title = "Loan"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("loan", height = "300px")
        )
        ,box(
          title = "Loan vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("loancount", height = "300px")
        )
        ,box(
          title = "Duration Distribution"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("durationdistribution", height = "300px")
        )
        ,box(
          title = "Duration Groups"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("duration", height = "300px")
        )
        ,box(
          title = "Duration Outliers"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("durationboxplot", height = "300px")
        )
        ,box(
          title = "Poutcome"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("poutcome", height = "300px")
        )
        ,box(
          title = "Poutcome Groups"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("poutcomecounts", height = "300px")
        )
        ,box(
          title = "Previous"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("previous", height = "300px")
        )
        ,box(
          title = "Balance"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("balance", height = "300px")
        )
        ,box(
          title = "Balance Groups"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("balancecount", height = "300px")
        )
        ,box(
          title = "Balance Groups vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("balancecount2", height = "300px")
        )
        ,box(
          title = "Month"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("month", height = "300px")
        )
        ,box(
          title = "Month vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("monthcount", height = "300px")
        )
        ,box(
          title = "Day"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("day", height = "300px")
        )
        ,box(
          title = "Day vs Class"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("daycount", height = "300px")
        )
      )
      
    )
  )
)


server <- function(input, output) {
  
  #creating the plotOutput content
  
  #Determine Class Proportions
  class_percentage <- bank_dataset[bank_dataset$train_test == "train",] %>% group_by(y) %>% summarise(count = n(), percentage = round(n()*100/36168,2))
  
  
  output$classProp <- renderPlot({
    
    #Barplot of Classes
    ggplot(class_percentage, aes(x=y, y=count, label=count, fill = y)) + 
      geom_bar(stat='identity') + 
      labs(title="Proportions of Classes in the Training Set",subtitle = "",caption = "Source: Group 4",
           x = "Class", y= "Count", fill = "Subscribe") + theme_classic() + 
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      scale_fill_manual(values = c("tomato3", "seagreen4"))
  })
  
  output$pieChart <- renderPlot({
    #Pie Chart of Classes
    ggplot(class_percentage, aes(x = "", y = percentage, fill = y)) +
      geom_bar(width = 1, stat = "identity", color = "gray55") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = sum(percentage) - cumsum(percentage) + percentage / 2, label = percentage), color = "black", size = 5) +
      theme_minimal() + ggtitle("Percentage of each class") + labs(x="", y="", fill = "Subscribe", caption = "Source: Group 4") + scale_fill_manual(values = c("tomato3", "seagreen4"))
    
  })
  
  ## Job ##
  
  ## Job ##
  # Probability table
  job <- as.data.frame(prop.table(table(bank_train$job, bank_train$y)))
  colnames(job) <-  c("job", "y", "perc")
  # Using count
  job2 <- as.data.frame(table(bank_train$job, bank_train$y))
  colnames(job2) <-  c("job", "y", "count")
  
  
  output$jobCount <- renderPlot({
  # Count of each category in the whole dataset
  ggplot(data = bank_dataset, aes(x = job)) + geom_bar(fill = "seagreen4") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + labs(x = "Job", y = "Count", caption = "Source: Group 4")
  })
  
  
  output$job2 <- renderPlot({
    
    ggplot(data = job2, aes(x = job, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Job")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  

  ## Marital ##
  
  ## Marital ##
  # Using count
  marital2 <- as.data.frame(table(bank_train$marital, bank_train$y))
  colnames(marital2) <-  c("marital", "y", "count")
  
  output$maritalCount <- renderPlot({
    
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = marital)) + geom_bar(fill = "seagreen4") + theme_minimal() + labs(x = "Marital Status", y = "Count", caption = "Source: Group 4")
  })
  
  output$marital2 <- renderPlot({
    ggplot(data = marital2, aes(x = marital, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Marital")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
    
  })
  
  ## Age ##
  
  # We will bin the age into age groups and visualize it with subscribtions
  bank_dataset <- bank_dataset %>% mutate(age_group = ifelse(age >= 18 & age <= 25, "18-25",
                                                             ifelse(age > 25 & age <= 35, "26-35",
                                                                    ifelse(age > 35 & age < 50, "36-49",
                                                                           ifelse(age >= 50 & age <= 65, "50-65", ">65")))))
  
  output$agedistribution <- renderPlot({
    # Age distribution
    
    ggplot(bank_dataset, aes(age)) + geom_density(fill = "seagreen4") + theme_minimal() + labs(x = "Age", y = "Density", caption = "Source: Group 4")
  })
  
  output$agegroup <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = age_group)) + geom_bar(fill = "seagreen4") + theme_minimal() + labs(x = "Age Group", y = "Count", caption = "Source: Group 4") + scale_x_discrete(limits = c("18-25", "26-35", "36-49", "50-65", ">65"))
  })
  
  age_group2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$age_group, bank_dataset[bank_dataset$train_test=="train",]$y))
  colnames(age_group2) <-  c("age_group", "y", "count")
  
  output$agecounts <- renderPlot({
    ggplot(data = age_group2, aes(x = age_group, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Age Group")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## Education ##
  
  output$educationcount <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = education)) + geom_bar(fill = "seagreen4") + theme_minimal() + labs(x = "Education", y = "Count", caption = "Source: Group 4")
    
  })
  output$educationcount2 <- renderPlot({
    # Using count
    edu2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$education, bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(edu2) <-  c("education", "y", "count")
    
    ggplot(data = edu2, aes(x = education, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Education")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## Default ##
  # Count of each category in the whole dataset
  
  output$default <- renderPlot({
    ggplot(data = bank_dataset, aes(x = default)) + geom_bar(fill = "seagreen4") + theme_minimal() +
      labs(x = "default", y = "Count", caption = "Source: Group 4")
    
  })
  output$defaultcount <- renderPlot({
    # Using count
    default2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$default, bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(default2) <-  c("default", "y", "count")
    
    ggplot(data = default2, aes(x = default, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Default")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## Housing ##
  output$housing <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = housing)) + geom_bar(fill = "seagreen4") +
      theme_minimal() + labs(x = "housing", y = "Count", caption = "Source: Group 4")
  })
  output$housingcount <- renderPlot({
    # Using count
    housing2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$housing, bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(housing2) <-  c("housing", "y", "count")
    
    ggplot(data = housing2, aes(x = housing, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Housing")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## Loan ##
  output$loan <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = loan)) + geom_bar(fill = "seagreen4") + theme_minimal() + labs(x = "loan", y = "Count", caption = "Source: Group 4")
  })
  output$loancount <- renderPlot({
    # Using count
    loan2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$loan, bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(loan2) <-  c("loan", "y", "count")
    
    ggplot(data = loan2, aes(x = loan, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Loan")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## Duration ##
  output$durationdistribution <- renderPlot({
    # Duration distribution
    ggplot(bank_dataset, aes(duration)) + geom_density(fill = "seagreen4") +
      theme_minimal() + labs(x = "Duration", y = "Density", caption = "Source: Group 4")
  })
  
  output$duration <- renderPlot({
    # We will group durations to see if call rduration has an effect on the probability of subscribtion
    # We will bin the age into age groups and visualize it with subscribtions
    bank_dataset <- bank_dataset %>% mutate(duration_group = ifelse(duration >= 0 & duration <= 500, "Very Short",
                                                                    ifelse(duration > 500 & duration <= 1500, "Short",
                                                                           ifelse(duration > 1500 & duration < 3000, "Long", "Very Long"
                                                                           ))))
    
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = duration_group)) + geom_bar(fill = "seagreen4") +
      theme_minimal() + labs(x = "Call Duration", y = "Count", caption = "Source: Group 4")
  })
  
  
  output$durationboxplot <- renderPlot({
    ggplot(bank_dataset[bank_dataset$train_test=="train",], aes(x=y, y=duration)) +
      geom_boxplot(fill='seagreen4', color="tomato3") +
      labs(x = "Subscribe", y = "Call Duration", caption = "Source: Group 4") + theme_minimal()
  })
  
  # output$durationcount <- renderPlot({
  #   # Using count
  #   duration_group2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$duration_group, bank_dataset[bank_dataset$train_test=="train",]$y))
  #   colnames(duration_group2) <-  c("duration_group", "y", "count")
  #   
  #   ggplot(data = duration_group2, aes(x = duration_group, y = count, fill = y)) + 
  #     geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Call Duration")+
  #     ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  #     scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  # })
  
  ## poutcome ##
  output$poutcome <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = poutcome)) + geom_bar(fill = "seagreen4") + theme_minimal() + labs(x = "poutcome", y = "Count", caption = "Source: Group 4")
  })
  
  output$poutcomecounts <- renderPlot({
    # Using count
    poutcome2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$poutcome, bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(poutcome2) <-  c("poutcome", "y", "count")
    
    ggplot(data = poutcome2, aes(x = poutcome, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("poutcome")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## previous ##
  output$previous <- renderPlot({
    # Count of each category in the whole dataset
    # for a better visualization, we will 
    ggplot(data = bank_dataset[bank_dataset$previous < 20,], aes(x = previous)) +
      geom_bar(fill = "seagreen4") + theme_minimal() + labs(x = "previous", y = "Count", caption = "Source: Group 4")
  })
  
  ## balance ##
  output$balance <- renderPlot({
    
    ggplot(bank_dataset, aes(balance)) + geom_histogram(fill = "seagreen4") + theme_minimal() +
      labs(x = "Balance", y = "Count", caption = "Source: Group 4")
  })
  
  
  bank_dataset <- bank_dataset %>% mutate(balance_range = ifelse(balance < 0, "negative balance",
                                                                 ifelse(balance >=0 & balance <= 20000, "low balance",
                                                                        ifelse(balance > 20000 & balance <= 60000, "medium balance",
                                                                               ifelse(balance > 60000, "high balance", NA)))))
  output$balancecount <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = balance_range)) + geom_bar(fill = "seagreen4") +
      theme_minimal() + labs(x = "Balance", y = "Count", caption = "Source: Group 4") + scale_x_discrete(limits = c("negative balance",
                                                                                                                    "low balance",
                                                                                                                    "medium balance",
                                                                                                                    "high balance"))
  })
  
  output$balancecount2 <- renderPlot({
    # Using count
    balance_group <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$balance_range,
                                         bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(balance_group) <-  c("balance_range", "y", "count")
    
    ggplot(data = balance_group, aes(x = balance_range, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Balance")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")+ scale_x_discrete(limits = c("negative balance",
                                                                                                                                                 "low balance",
                                                                                                                                                 "medium balance",
                                                                                                                                                 "high balance"))
  })
  
  ## Month ##
  output$month <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = month)) + geom_bar(fill = "seagreen4") +
      theme_minimal() + labs(x = "Month", y = "Count", caption = "Source: Group 4")
  })
  
  output$monthcount <- renderPlot({
    # Using count
    month <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$month,
                                 bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(month) <-  c("month", "y", "count")
    
    ggplot(data = month, aes(x = month, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("Month")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
  
  ## day ##
  output$day <- renderPlot({
    # Count of each category in the whole dataset
    ggplot(data = bank_dataset, aes(x = day)) + geom_bar(fill = "seagreen4") +
      theme_minimal() + labs(x = "day", y = "Count", caption = "Source: Group 4")
  })
  
  output$daycount <- renderPlot({
    # Using count
    day2 <- as.data.frame(table(bank_dataset[bank_dataset$train_test=="train",]$day, bank_dataset[bank_dataset$train_test=="train",]$y))
    colnames(day2) <-  c("day", "y", "count")
    
    ggplot(data = day2, aes(x = day, y = count, fill = y)) + 
      geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) +xlab("day")+
      ylab("Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_fill_manual(values = c("tomato3", "seagreen4")) + labs(fill = "Subscribe", caption = "Source: Group 4")
  })
}


shinyApp(ui, server)

