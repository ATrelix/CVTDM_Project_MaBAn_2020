library(shinyjs)
library(shinythemes)
library(shiny)
library(pander)
library(dplyr)
library(gt)
library(car)
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

Q5 <- c("Yes", "No")
Q6 <- c("Yes", "No")
Q7 <- c("Never", "Sometimes", "Always")
Q8 <- c("Between 1 and 2", "Three", "More than three")
Q9 <- c("No", "Sometimes", "Frequently", "Always")
Q10 <- c("Yes", "No")
Q11 <- c("Less than a liter", "Between 1 and 2 liters", "More than 2 liters")
Q12 <- c("Yes", "No")
Q13 <- c("I do not have", "Between 1 and 2 hours", "Between 2 and 4 hours")
Q14 <- c("0 hours", "1 hour", "2 hours or more")
Q15 <- c("I do not drink", "Sometimes", "Frequently", "Every day")
Q16 <- c("Automobile", "Motorbike", "Bike", "Public Transportation", "Walking")


ui <- fluidPage(theme = shinytheme("flatly"),
                
                
                
                titlePanel(h1("Weight Predictor", align = "center")),
              
                div(img(src="picture.jpg", height="20%", width="20%"), style="text-align: center;"),
                
                tabsetPanel(type="tab",
                            tabPanel("Predictor",
                                     
                mainPanel(
                    
                    br(),
                    paste("Welcome to the Weight Predictor! "),
                    br(),
                    br(),
                    
                    paste("Please, answer a few questions to obtain some data about you."),
                    br(),
                    br(),
                    br(),
                    
                    radioButtons("Q1", "Q1 - You are a ... ", Q1, selected = character(0)),
                    br(),
                    numericInput("Q2", "Q2 - How old are you?", "", min = 1, max = 100),
                    br(),
                    numericInput("Q3", "Q3 - How tall are you (in meters)? (e.g. 1.56)", "", min = 1, max = 100),
                    br(),
                    numericInput("Q4", "Q4 - How much (in Kg) do you weight currently?", "", min = 1, max = 300),
                    br(),
                    radioButtons("Q5", "Q5 - Has a family member suffered or suffers from overweight?", Q5, selected = character(0)),
                    br(),
                    radioButtons("Q6", "Q6 - Do you eat high caloric food frequently?", Q6, selected = character(0)),
                    br(),
                    radioButtons("Q7", "Q7 - Do you usually eat vegetables in your meals?", Q7, selected = character(0)),
                    br(),
                    radioButtons("Q8", "Q8 - How many main meals do you have daily?", Q8, selected = character(0)),
                    br(),
                    radioButtons("Q9", "Q9 - Do you eat any food between meals?", Q9, selected = character(0)),
                    br(),
                    radioButtons("Q10", "Q10 - Do you smoke?", Q10, selected = character(0)),
                    br(),
                    radioButtons("Q11", "Q11 - How much water do you drink daily?", Q11, selected = character(0)),
                    br(),
                    radioButtons("Q12", "Q12 - Do you monitor the calories you eat daily?", Q12, selected = character(0)),
                    br(),
                    radioButtons("Q13", "Q13 - How often do you have physical activity?", Q13, selected = character(0)),
                    br(),
                    radioButtons("Q14", "Q14 - How much time do you daily use technological devices such as cell phone, videogames, 
                                 television, computer and others?", Q14, selected = character(0)),
                    br(),
                    radioButtons("Q15", "Q15 - How often do you drink alcohol?", Q15, selected = character(0)),
                    br(),
                    radioButtons("Q16", "Q16 - Which transportation do you usually use?", Q16, selected = character(0)),
                    
                    br(),
                    br(),
                    
                    paste("Thank you! Now, click on 'Save your answers'."),
                    br(),
                    br(),
                    
                    actionButton(inputId = "save", label = "Save your answers", class="btn btn-secondary", icon = icon("key"), width = NULL),
                    
                    br(),
                    br(),
                    br(),
                    br(),
                    
                    
                ),
                
              
                
                sidebarPanel(
                    style = "position:fixed;width:inherit;",
                    selectInput("models", label = "When you are done with the questions, please choose a model from the dropdown list below : ", choices = list("Multiple Linear Regression", "k-NN", "Regression Tree", "Ensemble")),
                    br(),
                    br(),
                    paste("Now, click here :"),
                    br(),
                    actionButton(inputId = "calculate_weight", label = "Know your weight !", class="btn btn-secondary", icon = icon("child"), width = NULL),
                    br(),
                    br(),
                             paste("Your weight (in Kg) will be"),
                             verbatimTextOutput("weight_pred"),
                             paste("if you continue with your daily habits."),
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
        
        app.final.df[, 3] = input$Q3
        app.final.norm[, 3] = (input$Q3 - min(train.set[, 3]))/(max(train.set[, 3]) - min(train.set[, 3]))
        
        app.final.df[, 4] = input$Q4
        app.final.norm[, 4] = (input$Q4 - min(train.set[, 4]))/(max(train.set[, 4]) - min(train.set[, 4]))
        
        if(input$Q5 == "Yes"){app.final.df[, 5] = 1}
        if(input$Q5 == "No"){app.final.df[, 5] = 0}
        
        if(input$Q6 == "Yes"){app.final.df[, 6] = 1}
        if(input$Q6 == "No"){app.final.df[, 6] = 0}
        
        if(input$Q7 == "Always"){app.final.df[, 7] = 1 ; app.final.df[, 8] = 0 ; app.final.df[, 9] = 0}
        if(input$Q7 == "Never"){app.final.df[, 7] = 0 ; app.final.df[, 8] = 1 ; app.final.df[, 9] = 0}
        if(input$Q7 == "Sometimes"){app.final.df[, 7] = 0 ; app.final.df[, 8] = 0 ; app.final.df[, 9] = 1}
        
        if(input$Q8 == "Between 1 and 2"){app.final.df[, 10] = 1 ; app.final.df[, 11] = 0 ; app.final.df[, 12] = 0}
        if(input$Q8 == "Three"){app.final.df[, 10] = 0 ; app.final.df[, 11] = 0 ; app.final.df[, 12] = 1}
        if(input$Q8 == "More than three"){app.final.df[, 10] = 0 ; app.final.df[, 11] = 1 ; app.final.df[, 12] = 0}
        
        if(input$Q9 == "Always"){app.final.df[, 13] = 1 ; app.final.df[, 14] = 0 ; app.final.df[, 15] = 0 ; app.final.df[, 16] = 0}
        if(input$Q9 == "Frequently"){app.final.df[, 13] = 0 ; app.final.df[, 14] = 1 ; app.final.df[, 15] = 0 ; app.final.df[, 16] = 0}
        if(input$Q9 == "Sometimes"){app.final.df[, 13] = 0 ; app.final.df[, 14] = 0 ; app.final.df[, 15] = 0 ; app.final.df[, 16] = 1}
        if(input$Q9 == "No"){app.final.df[, 13] = 0 ; app.final.df[, 14] = 0 ; app.final.df[, 15] = 1 ; app.final.df[, 16] = 0}
        
        if(input$Q10 == "Yes"){app.final.df[, 17] = 1}
        if(input$Q10 == "No"){app.final.df[, 17] = 0}
        
        if(input$Q11 == "Between 1 and 2 liters"){app.final.df[, 18] = 1 ; app.final.df[, 19] = 0 ; app.final.df[, 20] = 0}
        if(input$Q11 == "Less than a liter"){app.final.df[, 18] = 0 ; app.final.df[, 19] = 1 ; app.final.df[, 20] = 0}
        if(input$Q11 == "More than 2 liters"){app.final.df[, 18] = 0 ; app.final.df[, 19] = 0 ; app.final.df[, 20] = 1}
        
        if(input$Q12 == "Yes"){app.final.df[, 21] = 1}
        if(input$Q12 == "No"){app.final.df[, 21] = 0}
        
        if(input$Q13 == "I do not have"){app.final.df[, 22] = 0 ; app.final.df[, 23] = 0 ; app.final.df[, 24] = 1}
        if(input$Q13 == "Between 1 and 2 hours"){app.final.df[, 22] = 1 ; app.final.df[, 23] = 0 ; app.final.df[, 24] = 0}
        if(input$Q13 == "Between 2 and 4 hours"){app.final.df[, 22] = 0 ; app.final.df[, 23] = 1 ; app.final.df[, 24] = 0}
        
        if(input$Q14 == "0 hours"){app.final.df[, 25] = 0 ; app.final.df[, 26] = 0 ; app.final.df[, 27] = 1}
        if(input$Q14 == "1 hour"){app.final.df[, 25] = 1 ; app.final.df[, 26] = 0 ; app.final.df[, 27] = 0}
        if(input$Q14 == "2 hours or more"){app.final.df[, 25] = 0 ; app.final.df[, 26] = 1 ; app.final.df[, 27] = 0}
        
        if(input$Q15 == "Every day"){app.final.df[, 28] = 1 ; app.final.df[, 29] = 0 ; app.final.df[, 30] = 0 ; app.final.df[, 31] = 0}
        if(input$Q15 == "Frequently"){app.final.df[, 28] = 0 ; app.final.df[, 29] = 1 ; app.final.df[, 30] = 0 ; app.final.df[, 31] = 0}
        if(input$Q15 == "Sometimes"){app.final.df[, 28] = 0 ; app.final.df[, 29] = 0 ; app.final.df[, 30] = 0 ; app.final.df[, 31] = 1}
        if(input$Q15 == "I do not drink"){app.final.df[, 28] = 0 ; app.final.df[, 29] = 0 ; app.final.df[, 30] = 1 ; app.final.df[, 31] = 0}
        
        if(input$Q16 == "Automobile"){app.final.df[, 32] = 1 ; app.final.df[, 33] = 0 ; app.final.df[, 34] = 0 ; app.final.df[, 35] = 0 ; app.final.df[, 36] = 0}
        if(input$Q16 == "Bike"){app.final.df[, 32] = 0 ; app.final.df[, 33] = 1 ; app.final.df[, 34] = 0 ; app.final.df[, 35] = 0 ; app.final.df[, 36] = 0}
        if(input$Q16 == "Motorbike"){app.final.df[, 32] = 0 ; app.final.df[, 33] = 0 ; app.final.df[, 34] = 1 ; app.final.df[, 35] = 0 ; app.final.df[, 36] = 0}
        if(input$Q16 == "Public Transportation"){app.final.df[, 32] = 0 ; app.final.df[, 33] = 0 ; app.final.df[, 34] = 0 ; app.final.df[, 35] = 1 ; app.final.df[, 36] = 0}
        if(input$Q16 == "Walking"){app.final.df[, 32] = 0 ; app.final.df[, 33] = 0 ; app.final.df[, 34] = 0 ; app.final.df[, 35] = 0 ; app.final.df[, 36] = 1}
        
        
        
        observeEvent(input$calculate_weight, {
            
            if(input$models == "Multiple Linear Regression"){
                
                mlr = predict(lm_backward_obesity, app.final.df) 
                
                output$weight_pred <- renderText({
                    
                    paste(round(mlr, 2))
                    
                })
                
            }
            
            
                
                if(input$models == "k-NN"){
                    
                    
                    knn.app = predict(k_nn, app.final.df)
                   
                    
                
                    output$weight_pred <- renderText({
                        
                        paste(round(knn.app, 2))
                        
                        
                    })
                    
                    output$df <- renderDataTable(app.final.norm)
                    
                }
            
           
            if(input$models == "Regression Tree"){
                
                
                tree.app = predict(tree_2, app.final.df)
                
                
                
                output$weight_pred <- renderText({
                    
                    paste(round(tree.app, 2))
                    
                    
                })
                
                
                
            }
            
            if(input$models == "Ensemble"){
                
                
                tree.app = predict(tree_2, app.final.df)
                
                knn.app = predict(k_nn, app.final.df)
                
                mlr = predict(lm_backward_obesity, app.final.df) 
                
                ensemble = (mlr + knn.app + tree.app) / 3
                
                
                output$weight_pred <- renderText({
                    
                    paste(round(ensemble, 2))
                    
                    
                })
                
                
                
            }
            
        }
        
        )
    
    }
    
    
    
    )
    
   

}


shinyApp(ui, server)


