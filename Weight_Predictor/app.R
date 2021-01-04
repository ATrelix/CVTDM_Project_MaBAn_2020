library(shinyjs)
library(shinythemes)
library(shiny)
library(pander)
library(dplyr)
library(gt)
library(car)
library(ggplot2)
library(gridExtra)
library(psych)
library(corrplot)
library(ellipse)
library(dummies)
library(nnet)
library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(ehaGoF)
library(forecast)



# Working Directory :

setwd("~/GitHub/CVTDM_Project_MaBAn_2020")


# Reading the data :

obesity <- read.csv("Obesity.csv", header=T, sep=",")


# Changing column names:

names(obesity)[5] = "family_history"
names(obesity)[6] = "eat_caloric"
names(obesity)[7] = "vegetables"
names(obesity)[8] = "main_meals"
names(obesity)[9] = "food_inbetween"
names(obesity)[12] = "monitor_cal"
names(obesity)[13] = "physical_act"
names(obesity)[14] = "tech_devices"
names(obesity)[15] = "alcohol"


# Binning some numerical variables :


binning <- function(x) {
    
    #vegetables 
    
    x$vegetables[x$vegetables <= 1] <- "Never"
    
    x$vegetables[x$vegetables > 1 & x$vegetables <=2] <- "Sometimes"
    
    x$vegetables[x$vegetables > 2 & x$vegetables <=3] <- "Always"
    
    
    #main_meals
    
    x$main_meals[x$main_meals >= 1 & x$main_meals < 3] <- "Btw_1_&_2"
    
    x$main_meals[x$main_meals == 3] <- "Three"
    
    x$main_meals[x$main_meals > 3 & x$main_meals <= 4] <- "More_than_3"
    
    
    #tech_devices
    
    x$tech_devices[x$tech_devices >= 0 & x$tech_devices <= 0.5] <- "Zero_hours"
    
    x$tech_devices[x$tech_devices <= 1.5] <- "One_hour"
    
    x$tech_devices[x$tech_devices <= 2] <- "Two_hours"
    
    
    #physical_act
    
    x$physical_act[x$physical_act < 1] <- "I do not have"
    
    x$physical_act[x$physical_act >= 1 & x$physical_act <= 2] <- "1 or 2 days"
    
    x$physical_act[x$physical_act >= 2 & x$physical_act <= 4] <- "2 or 4 days"
    
    x$physical_act[x$physical_act >= 4 & x$physical_act <= 5] <- "4 or 5 days"
    
    
    #CH2O
    
    x$CH2O[x$CH2O <= 1] <- "Less than a liter"
    
    x$CH2O[x$CH2O <= 2] <- "Between 1 and 2 L"
    
    x$CH2O[x$CH2O <=3] <- "More than 2 L"
    
    
    return(x)  
    
}


obesity_bin = binning(obesity)



# Converting character variables to factor :


to_factor <- function(x) {
    
    x$Gender = as.factor(x$Gender)
    x$family_history = as.factor(x$family_history)
    x$eat_caloric = as.factor(x$eat_caloric)
    x$food_inbetween = as.factor(x$food_inbetween)
    x$SMOKE = as.factor(x$SMOKE)
    x$monitor_cal = as.factor(x$monitor_cal)
    x$alcohol = as.factor(x$alcohol)
    x$MTRANS = as.factor(x$MTRANS)
    x$NObeyesdad = as.factor(x$NObeyesdad)
    x$vegetables = as.factor(x$vegetables)
    x$main_meals= as.factor(x$main_meals)
    x$CH2O= as.factor(x$CH2O)
    x$physical_act= as.factor(x$physical_act)
    x$tech_devices= as.factor(x$tech_devices)
    
    return(x)
    
}

obesity_factor = to_factor(obesity_bin)



dummify <- function(x) {
    
    # Gender 1 = female, 0 = male
    obesity_dummy <- cbind(dummy(x$Gender, sep = "_"), x[2:17])
    names(obesity_dummy)[1] <- c("Gender")
    obesity_dummy <- subset(obesity_dummy, select = -c(2) )
    
    
    # family_history 1 = yes, 0 = no
    obesity_dummy <- cbind( obesity_dummy[1:4], dummy(obesity_dummy$family_hist, sep = "_"), obesity_dummy[6:17])
    names(obesity_dummy)[6] <- c("family_hist")
    obesity_dummy <- subset(obesity_dummy, select = -c(5) )
    
    
    # eat_caloric with 1 = yes, 0 = no
    obesity_dummy <- cbind( obesity_dummy[1:5], dummy(obesity_dummy$eat_caloric, sep = "_"), obesity_dummy[7:17])
    names(obesity_dummy)[7] <- c("eat_caloric")
    obesity_dummy <- subset(obesity_dummy, select = -c(6) )
    
    
    # SMOKE 1 = yes, 0 = no
    obesity_dummy <- cbind( obesity_dummy[1:9], dummy(obesity_dummy$SMOKE, sep = "_"), obesity_dummy[11:17])
    names(obesity_dummy)[11] <- c("smoke")
    obesity_dummy <- subset(obesity_dummy, select = -c(10) )
    
    
    # monitor_cal 1 = yes, 0 = no
    obesity_dummy <- cbind( obesity_dummy[1:11], dummy(obesity_dummy$monitor_cal, sep = "_"), obesity_dummy[13:17])
    names(obesity_dummy)[13] <- c("monitor_cal")
    obesity_dummy <- subset(obesity_dummy, select = -c(12) )
    
    
    
    # Dummmyfying the categorical variables
    
    # vegetables 
    obesity_dummy <- cbind(obesity_dummy[1:6], dummy(obesity_dummy$vegetables, sep = "_"), obesity_dummy[8:17])
    names(obesity_dummy)[7:9] <- c("vegetables_always","vegetables_never","vegetables_sometimes")
    
    # main_meals
    obesity_dummy <- cbind(obesity_dummy[1:9], dummy(obesity_dummy$main_meals, sep = "_"), obesity_dummy[11:19])
    names(obesity_dummy)[10:12] <- c("main_meals_Btw_1_2","main_meals_More_than_3","main_meals_three")
    
    # food_in_between
    obesity_dummy <- cbind(obesity_dummy[1:12], dummy(obesity_dummy$food_inbetween, sep = "_"), obesity_dummy[14:21])
    names(obesity_dummy)[13:16] <- c("food_inbetween_always","food_inbetween_frequently","food_inbetween_no", "food_inbetween_sometimes")
    
    # alcohol
    obesity_dummy <- cbind(obesity_dummy[1:21], dummy(obesity_dummy$alcohol, sep = "_"), obesity_dummy[23:24])
    names(obesity_dummy)[22:25] <- c("alcohol_always","alcohol_frequently","alcohol_no", "alcohol_sometimes")
    
    # MTRANS
    obesity_dummy <- cbind(obesity_dummy[1:25], dummy(obesity_dummy$MTRANS, sep = "_"), obesity_dummy[27])
    names(obesity_dummy)[26:30] <- c("mtrans_automobile","mtrans_bike","mtrans_motorbike", "mtrans_public_transportation", "mtrans_walking")
    
    # CH2O
    obesity_dummy <- cbind(obesity_dummy[1:17], dummy(obesity_dummy$CH2O, sep = "_"), obesity_dummy[19:31])
    names(obesity_dummy)[18:20] <- c("CH2O_between_1_and_2","CH2O_less_than_a_liter","CH2O_more_than_2")
    
    # physical_act
    obesity_dummy <- cbind(obesity_dummy[1:21], dummy(obesity_dummy$physical_act, sep = "_"), obesity_dummy[23:33])
    names(obesity_dummy)[22:24] <- c("physical_act_1_2","physical_act_2_4", "physical_act_do_not_have")
    
    
    # tech_devices : this one is a little bit tricky since there a many categories but only one is represented within the data!
    
    obesity_dummy <- cbind(obesity_dummy[1:24], dummy(obesity_dummy$tech_devices, sep = "_"), obesity_dummy[26:35])
    names(obesity_dummy)[25:27] <- c("tech_1_hour", "tech_2_hours_or_more", "tech_0_hours")
    
    #remove(obesity_dum)
    obesity_dummy <- subset(obesity_dummy[c(1:36)])
    
    return(obesity_dummy)
    
}

obesity_dum = dummify(obesity_factor)





# Partitioning the data (60% training, 40% validation)

set.seed(1)

train.obs <- sample(rownames(obesity_dum), dim(obesity_dum)[1]*0.6)
train.set <- obesity_dum[train.obs, ]  

set.seed(1)

valid.obs <- setdiff(rownames(obesity_dum), train.obs)
valid.set <- obesity_dum[valid.obs, ]



# Linear regression

lm_weight <- lm(Weight ~ Gender + Age + Height + family_hist + eat_caloric + vegetables_sometimes +vegetables_always + main_meals_Btw_1_2 + main_meals_More_than_3 + food_inbetween_always + food_inbetween_frequently + food_inbetween_sometimes + smoke + CH2O_between_1_and_2 + CH2O_more_than_2 + monitor_cal + physical_act_1_2 +physical_act_2_4 + tech_1_hour+ tech_2_hours_or_more + alcohol_always + alcohol_frequently + alcohol_sometimes + mtrans_automobile + mtrans_bike + mtrans_public_transportation  , data = train.set)

lm_backward_obesity <- step(lm_weight, direction = "backward")

backward_pred_obesity <- predict(lm_backward_obesity, valid.set)





# Normalizing the data :

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

train.set.norm <- as.data.frame(lapply(train.set[, c(2:4)], normalize))

valid.set.norm <- as.data.frame(lapply(valid.set[, c(2:4)], normalize))


# Regrouping into final dataset, with replacement of non-normalized variables :

train.final <- cbind(train.set.norm, train.set[, c(1, 5:36)])

valid.final <- cbind(valid.set.norm, valid.set[, c(1, 5:36)])



# Denormalizing function

denormalize <- function(x, y) {
    
    return ((x*(max(y) - min(y))) + min(y))
    
}



# KNN
# Running the FINAL model (with k = 1):

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(1)

k_nn <-
    train(
        Weight ~ .,
        data = train.set,
        method = "knn",
        trControl=trctrl,
        preProcess = c("range")
        
    )




# TREE

set.seed(1)

tree_2 <- rpart(
    Weight ~ .,
    data = train.set,
    method = "anova",
    control = rpart.control(
        cp = 0.0051561,
        minbucket = 1,
        maxdepth = 10
    )
)


predicted_valid <- predict(tree_2, valid.set)



    
    app.df = obesity_bin[1,]
    
    app.df = to_factor(app.df)
    
    app.big.df = rbind(obesity_factor, app.df)
    
    app.big.df = dummify(app.big.df)
    
    
    app.final.df = app.big.df[2112, ]
    
    norm.app.values <- preProcess(obesity[, c(2:4)], method = "range")
    
    app.final.norm = predict(norm.app.values, app.final.df)
    






Q1 <- c("Male", "Female")

Q10 <- c("Yes", "No")

ui <- fluidPage(theme = shinytheme("flatly"),
                
                
                
                titlePanel("Weight Prediction"),
                
                tabsetPanel(type="tab",
                            tabPanel("Predictor",
                                     
                mainPanel(
                    
                    
                    radioButtons("Q1", "Q1 - You are a ... ", Q1, selected = character(0)),
                    numericInput("Q2", "Q2 - How old are you ?", 0, min = 1, max = 100),
                    radioButtons("Q10", "Q10 - Do you smoke ?", Q10, selected = character(0)),
                    
                    actionButton(inputId = "save", label = "Save your answers", class="btn btn-secondary", icon = icon("key"), width = NULL),
                    
                    dataTableOutput("df")
                ),
                
              
                
                sidebarPanel(
                    selectInput("models", label = "Choose a model : ", choices = list("Multiple Linear Regression", "k-NN", "Regression Tree", "Ensemble")),
                    br(),
                             br(),
                             paste("Your weight will be"),
                             verbatimTextOutput("weight_pred"),
                             paste("if you continue with your daily habits."),
                             br(),
                             br(),
                             actionButton(inputId = "calculate_weight", label = "Know your weight !", class="btn btn-secondary", icon = icon("child"), width = NULL),
                             br(),
                             br(),
                )
                            ),
                
                tabPanel("About",
                         
                         
                         paste("This is us!"))

)
)

server <- function(input, output) {
    
    output$weight_pred <- renderText({
        
       
        paste(" ")
        
        
    })
    
    observeEvent(input$save, {
        
        if(input$Q1 == "Male"){app.final.df[, 1] = 0}
        if(input$Q1 == "Female"){app.final.df[, 1] = 1}
        
        app.final.df[, 2] = input$Q2
        app.final.norm[, 2] = (input$Q2 - min(train.set[, 2]))/(max(train.set[, 2]) - min(train.set[, 2]))
        
        if(input$Q10 == "Yes"){app.final.df[, 17] = 1}
        if(input$Q10 == "No"){app.final.df[, 17] = 0}
        
        
        
        
        
        output$df <- renderDataTable(app.final.df)
    
    
        observeEvent(input$calculate_weight, {
            
            if(input$models == "Multiple Linear Regression"){
                
                mlr = predict(lm_backward_obesity, app.final.df) 
                
                output$weight_pred <- renderText({
                    
                    paste(mlr)
                    
                })
                
            }
            
            
                
                if(input$models == "k-NN"){
                    
                    
                    knn.app = predict(k_nn, app.final.df)
                   
                    
                
                    output$weight_pred <- renderText({
                        
                        paste(knn.app)
                        
                        
                    })
                    
                    output$df <- renderDataTable(app.final.norm)
                    
                }
            
           
        }
        
        )
    
    }
    
    
    
    )
    
   

}


shinyApp(ui, server)


