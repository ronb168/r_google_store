#####PART 1: questions by lessons#################################
rm(list=ls())
####libraries####
library(tidyverse)
##############1. for, if , while
setwd("D:/Ron Borower/Afeka/R projects/Final_assignment/")
Airlines=read.csv("Airlines.csv")
str(Airlines)
Flights =read.csv("Flights.csv")
str(Flights)
Planes=read.csv("Planes.csv")
str(Planes)
Airports=read.csv("airports.csv")
str(Airports)


#1A. Automatically find common names between Airlines and Flights, 
# assign the common names into a vector#
Common_names = c(colnames(Airlines)
                 [which(colnames(Airlines) %in% colnames(Flights))])

  #1B. using the relevant column from 1A combine Airlines and Flights  using inner_join,
  # the by attribute in left join should refer to the name in Common_names#
  Flights_airlines = Airlines %>% 
    inner_join(Flights,by = Common_names)
  
  
  #1C. Automatically find common names between Flights_airlines and Planes, 
  # assign the common names into a vector
  Common_names1 = c(colnames(Planes)
                    [which(colnames(Planes) %in% colnames(Flights_airlines))])
  
  #1D. using the relevant column from 1C combine Flights_airlines and Planes  using left_join,
  # the by attribute in left join should refer to the name in Common_names1#
  Flights_airlines_planes = Flights_airlines %>% 
    left_join(Planes, by = Common_names1)
  
  dim(Flights_airlines_planes)
  
  #1E.How many of the unique destinations are there (marked by dest)?
  unique_destinations = unique(Flights_airlines_planes$dest)
  length(unique_destinations)
  
  
  #1F. What is the top 10 destinations om the table?
  #    How many flights were directed to each of them? Create an object that sums this data 
  Top_airports_codes = sort(table(Flights_airlines_planes$dest),
                            decreasing = T)[1:10]
  
  Top_airports_codes

  #1E. Using for, while and if only once write a code that imitates left_Join
  
  Flights_airlines1 = Flights
  Flights_airlines1$name = 'NA'
  row = 1
  while(row <= nrow(Flights_airlines1)){
    for(i in 1:nrow(Airlines)){
      if(Flights_airlines1[row,Common_names] == Airlines[i,Common_names]){
        Flights_airlines1$name[row] = Airlines$name[i]
      }
    }
    row = row + 1
  }
  F_W_I_Flights_airlines_planes = Flights_airlines1
  F_W_I_Flights_airlines_planes$type = 'NA'
  F_W_I_Flights_airlines_planes$manufacturer = 'NA'
  F_W_I_Flights_airlines_planes$model = 'NA'
  F_W_I_Flights_airlines_planes$engines = 'NA'
  F_W_I_Flights_airlines_planes$seats = 'NA'
  F_W_I_Flights_airlines_planes$speed = 'NA'
  F_W_I_Flights_airlines_planes$engine = 'NA'
  row1 = 1
  while(row <= nrow(F_W_I_Flights_airlines_planes)){
    for(j in 1:nrow(Planes)){
      if(F_W_I_Flights_airlines_planes[row1,Common_names1$tailnum] == Planes[j,Common_names1$tailnum]){
        F_W_I_Flights_airlines_planes$type[row1] = Planes$type[j]
        F_W_I_Flights_airlines_planes$manufacturer[row1] = Planes$manufacturer[j]
        F_W_I_Flights_airlines_planes$model[row1] = Planes$model[j]
        F_W_I_Flights_airlines_planes$engines[row1] = Planes$engines[j]
        F_W_I_Flights_airlines_planes$seats[row1] = Planes$seats[j]
        F_W_I_Flights_airlines_planes$speed[row1] = Planes$speed[j]
        F_W_I_Flights_airlines_planes$engine[row1] = Planes$engine[j]
      }
    }
    row1 = row1 + 1
  }
  
  #1G. Now convert the top 10 destinations airport codes to full names.
  #    use iata_code column in Airport dataframe to find the vector of row indices in Airport,
  #    using %in% opreator. Using the vector of indices, 
  #    extract the name column in the relevant 10 destinations
  #    and save the vector of full names to Top_airports
  Top_airports = filter(Airports,iata_code %in% rownames(Top_airports_codes))[, colnames(Airports) %in% c('name','iata_code')]
  Top_airports$name

#1H.  Using for, while and if at least once write a code that imitates A-D:
#check:
  
Flights_airlines_planes=Flights_airlines_planes[order(Flights_airlines_planes$carrier),]
F_W_I_Flights_airlines_planes=F_W_I_Flights_airlines_planes[order(F_W_I_Flights_airlines_planes$carrier),]

Flights_airlines_planes = Flights_airlines_planes %>% select(order(colnames(Flights_airlines_planes)))
F_W_I_Flights_airlines_planes = F_W_I_Flights_airlines_planes %>% select(order(colnames(F_W_I_Flights_airlines_planes)))

Flights_airlines_planes[1,]==F_W_I_Flights_airlines_planes[1,]#Should return TRUE
Flights_airlines_planes[57,]==F_W_I_Flights_airlines_planes[57,]#Should return TRUE


##############2.Functions: 
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyverse)
url="D:/Ron Borower/Afeka/R projects/Final_assignment/googleplaystore.csv"

lables_vec = c(x="X Axis",y = "Y Axis",title = "Popular Apps on Google Play Store")

DF=read.csv(url)

DF = DF[complete.cases(DF),]

DF$Size = str_replace(DF$Size, "M", "000")
DF$Size = str_replace(DF$Size, "k", "")
DF$Size = str_replace(DF$Size, "Varies with device", "0")
DF$Size = as.numeric(DF$Size)

############## A. plot generator
Plot_generator=function(df,X_var,Y_var,Color_var,facet_var,label_vec){
  library(tidyverse)
  library(dplyr)
  
  df <- df %>% mutate(Y_var = UQ(enquo(Y_var)), 
                      Color_var = UQ(enquo(Color_var)),
                      facet_var = UQ(enquo(facet_var)))
  attach(df)
  
  #######Your code here:###
  if(is.character(X_var) == T){
    Plot=ggplot(df,aes(X_var,Y_var))+
      labs(title=label_vec[3],y=label_vec[2],x=label_vec[1])+
      geom_bar(stat = "identity", aes(fill = Color_var/10^4))+facet_wrap(facet_var~.)
    Plot
    Plot+
      scale_fill_gradient(low = "steelblue4",high = "turquoise2")+
      theme(Plot.title = element_text(face = "bold", size = 12, hjust=0.5))
    
  }
  else if (is.numeric(X_var) == T){
    
    Plot = ggplot(df,aes(X_var,Y_var)) +
      geom_point(aes(fill=Color_var)) + 
      facet_wrap(facet_var~.) +
      labs(title=label_vec[3],
           y=label_vec[2],
           x=label_vec[1])
    
  }
  
  #########################
  detach(df)
  return(Plot)
}
#check for categorial X_var:
Plot_generator(DF,DF$Category, DF$Rating, DF$Reviews, DF$Type, lables_vec)
#check for numerical X_var:
Plot_generator(DF,DF$Size, DF$Rating, DF$Reviews, DF$Type, lables_vec)


############## B. 

is_arithmetic_progression = function(v){
  x = sort(v)
  d = x[2] - x[1]
  for (i in 1:length(x)){
    if (x[i+1] - x[i] != d && i != length(x) ){
      return(F)
    }
  }
  return(d)
}
is_geometric = function(v){
  x = sort(v)
  q = x[2]/x[1]
  for (i in 1:length(x)){
    if (x[i+1] / x[i] != q && i != length(x) ){
      return(F)
    }
  }
  return(q)
}
is_fibonacci = function(v){
  x = sort(v)
  if (x[1] != 0 || x[2] != 1){
    return(F)
  }
  for (i in 3:length(x)){
    if ((x[i - 1] + x[i - 2])!= x[i]){
      return(F) 
    }
  }
  return(length(x))
}
  
SIDRA_SIDRA=function(Some_input){
  #your code here:
  if(is.vector(Some_input) == F){
    return("The input is not a vector!!")
  }
  else if (is.numeric(Some_input) == F) {
    return("Not a numeric vector!!")  
  }
  else if (length(Some_input) < 3) {
    return("The Vector is too short!")
  }
  else if (is.numeric(is_arithmetic_progression(Some_input))){
    msg = "The vector is an arithmetic series, with d"
    d = is_arithmetic_progression(Some_input)
    return(paste(msg, d, sep = " = "))
  }
  else if (is.numeric(is_geometric(Some_input))){
    msg = "The vector is a geometric series, with q"
    q = is_geometric(Some_input)
    return(paste(msg, q, sep = " = "))
  }
  else if (is.numeric(is_fibonacci(Some_input))){
    msg = "The vector is a fibonacci series, with"
    n = is_fibonacci(Some_input)
    s = paste(msg, n)
    return(paste(s, "items"))
  }
  else if (is_fibonacci(Some_input) == F){
    return("Sometimes numbers are just numbers…")
  }
}
##Tests:
SIDRA_SIDRA(1)
SIDRA_SIDRA("a")
SIDRA_SIDRA(c(1,2))
SIDRA_SIDRA(matrix(NA,2,2))
SIDRA_SIDRA(data.frame(matrix(NA,2,2)))
SIDRA_SIDRA(c(1,2,4)) 
SIDRA_SIDRA(c(2,4,6,8)) 
SIDRA_SIDRA(c(1,1,2,3,5,8)) 
SIDRA_SIDRA(c(0,1,1,2,3,5,8,13,21)) 


##########################################################
##Part B: Machine learning and web applications: 60 points
##########################################################

#Q3. Logisitic regression: 15 points
####################################
library(tidyverse)
library(caret)

rm(list=ls())

#######################
####A.
url="D:/Ron Borower/Afeka/R projects/Final_assignment/googleplaystore.csv"

Data=read.csv(url)

Data$Installs = str_replace(Data$Installs, ",", "")
Data$Installs = str_replace(Data$Installs, ",", "")
Data$Installs = str_replace(Data$Installs, "\\+", "")
Data$Installs = as.numeric(Data$Installs)
unique(Data$Installs)
class(Data$Installs)

Data$Size = str_replace(Data$Size, "M", "000")
Data$Size = str_replace(Data$Size, "k", "")
Data$Size = str_replace(Data$Size, "Varies with device", "0")
Data$Size = as.numeric(Data$Size)
unique(Data$Size)
class(Data$Size)

Data$Price = as.numeric(Data$Price)

Data$Reviews  = as.numeric(Data$Reviews)

Data$Size  = as.numeric(Data$Size)

names(Data)[5] = "Size_KB"

Data$Success = ifelse(Data$Installs >= 1000000 & Data$Rating >=4, 1, 0)
unique(Data$Success)

Data = subset(Data, select = -c(Genres,Content.Rating, Last.Updated, Current.Ver, Android.Ver,Type,Category, ï..App))
#remove na
Data = Data[complete.cases(Data),]

str(Data)
head(Data)

####B.
row_sampler=function(df){
  set.seed(789)
  #your code here
  n_rows_data = nrow(df)
  random_row_nums = sample(x=1:n_rows_data, size=n_rows_data, replace = F)
    
  return(random_row_nums)
}
#Check:
Random_row_nums=row_sampler(Data)
length(unique(Random_row_nums))

####C.
Train_test_division=function(train_fraction,df){
  #your code here
  
  Indices_vec = row_sampler(df)
  
  Division_point = round(nrow(df)*train_fraction, digits = 0)
    Train_Indices = Indices_vec[1:Division_point]
    Test_Indices = Indices_vec[(1+Division_point):length(Indices_vec)]
    Train = df[Train_Indices, ]
    Test = df[Test_Indices, ]
    return(list(Train=Train,Test=Test))
}
Train_test_Data=Train_test_division(0.8,Data)
str(Train_test_Data$Train)
####D.
Train=Train_test_Data$Train
Test=Train_test_Data$Test
#your code here

Logistic_train = glm(formula = Success~ ., data = Train, family="binomial")
Predictions = predict(object = Logistic_train, newdata = Test, type="response")

Predictions = as.numeric(Predictions)

str(Predictions)
str(Test$Success)

Predictions_glm = as.factor(ifelse(Predictions>0.5,1,0))

factor(Predictions_glm)
factor(Test$Success)

  ####E.
Learning_confusion_matrix=confusionMatrix(data =Predictions_glm, 
                                           reference = as.factor(Test$Success))
Learning_confusion_matrix

  ####F.
  #out of 1732 samples, 954 that are 0 were indeed classified as 0 - meaning 954 true positive outcomes.
  #out of 1732 samples, 534 that are 1 were indeed classified as 1 - meaning 534 true negative outcomes.
  #220 samples are false positive - falsely classified as 0. 
  #24 samples are false negative - falsely classified as 1. 
  #The accuracy of the model is 85.91%. 
  
#Q4. Linear regression: 15 points
#################################

####A.
LM_Data=Data

  
Train_test_LM_Data=Train_test_division(0.8,LM_Data)
LM_Train=Train_test_LM_Data$Train
LM_Test=Train_test_LM_Data$Test
####B.
Linear_train=lm(formula = Success~ ., data = Train)
Linear_Predictions = predict(object = Linear_train, newdata = Test, type="response")
#Linear_Predictions = as.factor(ifelse(Linear_Predictions>0.5,1,0))

  ####C.
error = Linear_Predictions-LM_Test$Success
RMSE = sqrt(mean(error^2))
RMSE
  
  ####D.
library(tidyverse)
#Your code here:


Test %>% 
  ggplot(aes(x = Success,y = Linear_Predictions)) +
  geom_point(alpha=0.5) + 
  stat_smooth(formula = y ~ x, aes(colour='black'),method = "lm") +
  xlab('Actual value') +
  ylab('Predicted value')+
  theme_bw()#+
  #geom_text(RMSE)

####E.
summary(Linear_train)
#Your code here:
#i.

Siginificant_cols = c(2,3,6)
#Siginificant_cols=c()
Siginificant_Data_Train=LM_Train[,Siginificant_cols]
Siginificant_Data_Test=LM_Test[,Siginificant_cols]

#ii.
Siginificant_Data_LM=lm(formula = Success~ Reviews + Size_KB, data = Siginificant_Data_Train)
Siginificant_Data_Predictions = predict(object = Siginificant_Data_LM,
                                        newdata = Siginificant_Data_Test,
                                        type="response")
error1 = Siginificant_Data_Predictions-Siginificant_Data_Test$Success
RMSE1 = sqrt(mean(error1^2))
  
min_rmse = min(RMSE, RMSE1)
  
#Q5. advnced topics- XGBOOST and hiny apps: 30 points
#####################################################

#i. XGBOOST: 18 points
######################

library(xgboost)
#a.
XG_train = xgboost(data = as.matrix(Train[, 1:5]),
                              label = Train$Success,
                              max.depth = 3,
                              eta = 1,
                              nthread = 2,
                              nrounds = 10,
                              objective = "binary:logistic")

  
  #b.
  Predictions_XG = predict(XG_train, as.matrix(Test[,1:5]))
  Predictions_XG
  #c. 
  Predictions_XG = as.factor(ifelse(Predictions_XG>0.5,1,0))
  
  confmat_XGB=confusionMatrix(data =Predictions_XG, 
                              reference = as.factor(Test$Success))
  confmat_XGB
#your answer here:
####C.
# XGboost Accuracy : 1
# Logistic Regression Accuracy : 0.8591
# The best model is the XGboost with 100% accuracy vs 85.91% with Logistic Regression
  


#d.
print(confmat_XGB)  #XGboost
print(Learning_confusion_matrix) #Logistic Regression
#I.
#your answer here:

# XGboost, Sensitivity : 1         
# Logistic Regression, Sensitivity : 0.9755          
 
# XGboost, Specificity : 1  
# Logistic Regression, Specificity : 0.7082         

# The best model is the XGboost with 100% Sensitivity and Specificity


#II.
#your answer here:

# Our main goal is to predict if an app is successful and for that we care more about the sensitivity of the models

#II.
#your answer here:

# The sensitivity of XGboost is 100%, so we don't miss any successful app.
# However,the sensitivity logistic regression is 97.55% which is very good score, but sometimes we can miss a successful app 

#ii. Additional classification models: 18 points  (only for groups of 3 students)
######################
#for the following questions perform the same steps as requierd in question 5.i. (XGBoost)



#a. random forest:
library(randomForest)
RF_train = randomForest(formula = Success~ ., data = Train)
Predictions_RF = predict(object = RF_train, newdata = Test, type="response")
Predictions_RF = as.factor(ifelse(Predictions_RF>0.5,1,0))

Learning_confusion_matrix_RF=confusionMatrix(data =Predictions_RF, 
                                          reference = as.factor(Test$Success))
Learning_confusion_matrix_RF


# Random Forest Accuracy : 1
# Logistic Regression Accuracy : 0.8591
# The best model is the Random Forest with 100% accuracy vs 85.91% with Logistic Regression

#II.
# Our main goal is to predict if an app is successful and for that we care more about the sensitivity of the models

# Random Forest, Sensitivity : 1         
# Logistic Regression, Sensitivity : 0.9755          

# Random Forest, Specificity : 1  
# Logistic Regression, Specificity : 0.7082         

# The best model is the Random Forest with 100% Sensitivity and Specificity

# The sensitivity of Random Forest is 100%, so we don't miss any successful app.
# However,the sensitivity logistic regression is 97.55% which is very good score, but sometimes we can miss a successful app



library('e1071')

SVM_train = svm(formula = Success~ ., data = Train)
Predictions_SVM = predict(object = SVM_train, newdata = Test, type="response")
Predictions_SVM = as.factor(ifelse(Predictions_SVM>0.5,1,0))

Learning_confusion_matrix_SVM=confusionMatrix(data =Predictions_SVM, 
                                             reference = as.factor(Test$Success))
Learning_confusion_matrix_SVM

# SVM Accuracy : 0.6397
# Logistic Regression Accuracy : 0.8591
# The best model is the Logistic Regression with 85.91% accuracy vs 63.97%% with SVM

#II.
# Our main goal is to predict if an app is successful and for that we care more about the sensitivity of the models

# SVM, Sensitivity : 1         
# Logistic Regression, Sensitivity : 0.9755          

# SVM, Specificity : 0.1724  
# Logistic Regression, Specificity : 0.7082         

# The sensitivity of SVM is 100%, so we don't miss any successful app.
# However,the sensitivity logistic regression is 97.55% which is very good score, but sometimes we can miss a successful app
#i. shiny web apps: 12 points
#############################

library(shiny)
library(rsconnect)
#a.runExample("04_mpg") from Shiny_tutorial1.Rmd:

runExample("04_mpg")

ui <- fluidPage(
  
  # App title
  titlePanel("Successful Apps"),
  
  # Sidebar layout 
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Var selection
      selectInput("Variable", "Variable:",
                  c("Rating" = "Rating")),
      
      # Input: Checkbox for whether outliers should be included
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for outputs
    mainPanel(
      
      # Output: Formatted text for caption
      h3(textOutput("caption")),
      
      # Output: plot by variable
      plotOutput("uiPlot")
      
    )
  )
)
Rating = Data$Rating

server <- function(input, output) {
  
  output$uiPlot <- renderPlot({
    boxplot(Success ~ Rating , data=Data, 
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

shinyApp(ui, server)


#b. k-means by Shiny_tutorial2.Rmd:
ui1=pageWithSidebar(
  headerPanel('Google Apps k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(Data)),
    selectInput('ycol', 'Y Variable', names(Data),
                selected=names(Data)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


server1=function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    Data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}
shinyApp(ui = ui1, server = server1)


#c. Another shiny app that was not examplified in class (only for groups of 3 students).

ui2 <- fluidPage(
  
  # App title
  titlePanel("Successful Apps"),
  
  # Sidebar layout 

    # Sidebar panel for inputs

    
    # Main panel for outputs
    mainPanel(
      
      # Output: Formatted text for caption
      h3(textOutput("caption")),
      
      # Output: plot by variable
      plotOutput("uiPlot")
      
    )
  )

library(dplyr)

server2 <- function(input, output) {
  
  output$uiPlot <- renderPlot({
    summary = table(Data$Success)
    str1 = paste("Failure",summary[1])
    str2 = paste("Success",summary[2])
    lbls = c(str1, str2)
    pie(summary, labels = lbls ,main = "Pie Chart")
    
  })
  
}

shinyApp(ui2, server2)

####Good Luck!! 
  