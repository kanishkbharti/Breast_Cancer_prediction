#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(kknn)
library(highcharter)
library(plotly)
library(ggplot2)
library(caret)
library(e1071)
require(shinyBS)

ui <- fluidPage(
    tags$head(HTML("<title>Breast Cancer</title>")),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #C0C0C0;}"))
    ),
    titlePanel(
        title=div(img(src="123.png"), "Seer Breast Cancer")
    ),
    sidebarLayout(sidebarPanel(
        width = 3,
        h2("About"),
        h4("This dataset of breast cancer patients was \
           obtained from the 2017 November update of the \
           SEER Program of the NCI, which provides information \
           on population-based cancer statistics. The dataset \
           involved female patients with infiltrating duct and \
           lobular carcinoma breast cancer (SEER primary cites \
           recode NOS histology codes 8522/3) diagnosed in 2006-2010. \
           Patients with unknown tumor size, examined regional LNs, regional \
           positive LNs, and patients whose survival months were less than 1 month \
           were excluded; thus, 4024 patients were ultimately included.")
    ),
                  mainPanel(
                      tabsetPanel(id = "tabset",
                                  
                                  tabPanel("Survey",
                                           br(),
                                           h4('Race having T3, T4 Stage:'),
                                           verbatimTextOutput("value", placeholder = TRUE),
                                           highchartOutput("plot_race_tstag"),
                                           hr(),
                                           h4(textOutput("survive")),
                                           splitLayout(
                                               highchartOutput("chart1"),
                                               highchartOutput("chart2")
                                           ),
                                           hr(),
                                           splitLayout(
                                               highchartOutput("chart3"),
                                               highchartOutput("chart4")
                                           ),
                                           hr(),
                                           splitLayout(
                                               highchartOutput("chart5"),
                                               highchartOutput("chart6")
                                           ),
                                           hr(),
                                            highchartOutput("chart7"),
                                           hr(),
                                            highchartOutput("chart8")
                                           
                                  ),#tabpanel 1 END
                                  
                                  
                                  tabPanel("Prediction Model",
                                           br(),
                                           h4("Input Data to Predict Stage"),
                                           numericInput(inputId = "inp_age", label = "Age",value = 12 ,min = 12,max = 80),
                                           selectizeInput(inputId = "inp_Astage", label = "A Stage", choices = "",options = list(create = TRUE)),
                                           numericInput(inputId = "inp_tumor_size", label = "Tumor Size", value = 5 ),
                                           selectizeInput(inputId = "n_stage", label = "N Stage", choices = "",options = list(create = TRUE)),
                                           selectizeInput(inputId = "x6th_stage", label = "6th Stage", choices = "",options = list(create = TRUE)),
                                           numericInput(inputId = "reg_nod_exam", label = "Regional Node Examined", value = 1 ),
                                           numericInput(inputId = "reg_nod_pos", label = "Regional Node Positive", value = 1 ),
                                           selectizeInput(inputId = "inp_Est_stat", label = "Estrogen Status", choices = "",options = list(create = TRUE)),
                                           selectizeInput(inputId = "inp_Pro_stat", label = "Progesterone Status", choices = "",options = list(create = TRUE)),
                                           numericInput(inputId = "surv_month", label = "Survival Months",value = 1),
                                           actionButton(inputId = "btn_model",label = "Predict"),
                                           br(),
                                           bsModal("bsmodal", "Results", "btn_model", size = "large",
                                                   h3(textOutput(outputId = "knn_model")),
                                                   h3(textOutput(outputId = "svm_model"))),
                                           br(),
                                           br()
                                  ),
                                  tabPanel("Result Analysis",
                                           br(),
                                           plotlyOutput("tree_chart"),
                                           hr(),
                                           numericInput(inputId = "bin_input",label = "Age Range",value = 5,min = 7),
                                           splitLayout(
                                               highchartOutput("alive_by_age"),
                                               highchartOutput("dead_by_age")),
                                           hr(),
                                           numericInput(inputId = "size_input",label = "Tumor Size Range",value = 7,min = 1),
                                           highchartOutput("alive_by_tumor"),
                                           highchartOutput("dead_by_tumor"),
                                           selectInput("inp_nstag_chrt","N Stage", choices = "",multiple = T),
                                           highchartOutput("nstag_chrt"),
                                           selectInput("inp_x6stag_chrt","6th Stage", choices = "",multiple = T),
                                           highchartOutput("x6stag_chrt")
                                  )
                  )
                  )
    
    ))
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    dataset <- read.csv(file = "SEER Breast Cancer Dataset .csv")
    # hideTab("tabset","Result Analysis")
    data <- dataset %>%
        select(Race,T.Stage,Status) %>%
        filter(T.Stage %in% c('T3','T4'))
    output$value <- renderPrint({
        
        race <- as.array(unique(data$Race))
        
        print(as.character(race), max.levels = 0)
    })
    output$survive <- renderText({
        data <- data %>%
            select(Status) %>%
            filter(Status %in% "Alive")
        paste("Survival of T3, T4 Stage: ",length(data$Status))
    })
    updateSelectInput(session,inputId = 'inp_Astage', choices = unique(dataset$A.Stage))
    updateSelectInput(session,inputId = 'inp_Est_stat', choices = unique(dataset$Estrogen.Status))
    updateSelectInput(session,inputId = 'inp_Pro_stat', choices = unique(dataset$Progesterone.Status))
    updateSelectizeInput(session,inputId = "x6th_stage", choices = unique(dataset$X6th.Stage))
    updateSelectizeInput(session,inputId = "n_stage", choices = unique(dataset$N.Stage))
    
    getdata <- eventReactive(input$btn_model,{
        
        err <- "Missing or Invalid Input"
        
        validate(
            need(input$inp_age != "", err)
        )
        
        validate(
            need(input$inp_Astage != "", err)
        )
        
        validate(
            need(input$inp_tumor_size != "", err)
        )
        
        validate(
            need(input$n_stage != "", err)
        )
        
        validate(
            need(input$x6th_stage != "", err)
        )
        
        validate(
            need(input$reg_nod_exam  != "", err)
        )
        
        validate(
            need(input$reg_nod_pos != "", err)
        )
        
        validate(
            need(input$inp_Est_stat != "", err)
        )
        
        validate(
            need(input$inp_Pro_stat != "", err)
        )
        
        validate(
            need(input$surv_month != "", err)
        )
        
        
        train <- dataset %>%
            select(Age, T.Stage, N.Stage,X6th.Stage, A.Stage, Tumor.Size,Estrogen.Status,Progesterone.Status,Regional.Node.Examined,Reginol.Node.Positive,Survival.Months)
        
        test1 <- data.frame(input$inp_age,"T3", input$n_stage, input$x6th_stage, input$inp_Astage, input$inp_tumor_size, input$inp_Est_stat, input$inp_Pro_stat, input$reg_nod_exam,input$reg_nod_pos, input$surv_month)
        colnames(test1) <- c("Age", "T.Stage", "N.Stage","X6th.Stage", "A.Stage", "Tumor.Size","Estrogen.Status","Progesterone.Status","Regional.Node.Examined","Reginol.Node.Positive","Survival.Months" )
        test <- train[1,]
        
        test <- rbind(test,test1)
        
        knn.fit <- kknn(T.Stage~., train ,test[nrow(test),],k = 5, distance = 2,scale = F)
        model.SVM <- svm(T.Stage~., train, kernel ='linear')
        svm.result <- predict(model.SVM, test[nrow(test),], type = "class")
        knn.result <- knn.fit$fitted.values
        return(c(as.character(knn.result),as.character(svm.result)))
        
    })
    output$knn_model <- renderText({
        if (getdata()[1]=="T3" ||getdata()[1]=="T4" ) {
            showTab("tabset","Result Analysis")
        }
        paste("T Stage using KNN: ", getdata()[1])
    })
    
    
    output$svm_model <- renderText({
        paste("T Stage using SVM: ", getdata()[2])
    })
    output$tree_chart <- renderPlotly({
        f <- list(
            family = "Courier New, monospace",
            size = 18,
            color = "#7f7f7f"
        )
        x <- list(
            title = "T Stage",
            titlefont = f
        )
        y <- list(
            title = "Status",
            titlefont = f
        )
        plot_ly(train, x=~train$T.Stage, y=~train$Status) %>%
            layout(xaxis = x, yaxis = y)
    })
    output$plot2 <- renderPlotly({
        dta <- train %>%
            select(T.Stage, Status) %>%
            filter(Status %in% "Alive")
        
        dta <-aggregate(x = dta, 
                            by = list(unique.dta = d$T.Stage), 
                            FUN = length)
        
        plot_ly(train,labels = ~dta$unique.d, values =~ dta$T.Stage)
    })
    
    output$plot_race_tstag <- renderHighchart({
        race_n_tstg <- dataset %>%
            select(Race,T.Stage) %>%
            filter(T.Stage %in% c("T3","T4"))
        
        dta <-aggregate(x = race_n_tstg,
                        by = list(unique.race_n_tstg = race_n_tstg$Race), 
                        FUN = length)
        dta %>%
            hchart(type = "pie", hcaes(x = unique.race_n_tstg, y = Race)) %>%
            hc_title(text = "Race with T3,T4 Stage Cancer") %>%
            hc_colors(colors = c("#FF1493","#FF69B4","#FFC0CB")) 
        
        
        # plot_ly(race_n_tstg,labels = ~dta$unique.race_n_tstg, values =~ dta$Race, type = "pie",colors = c("grey50", "blue", "red"))
        
    })
    
    output$chart1 <- renderHighchart({
        alive_tstag <- dataset %>%
            select(T.Stage,Status) %>%
            filter(Status %in% "Alive",T.Stage %in% c("T3","T4"))
        
        
        aliv_dta <-aggregate(x = alive_tstag,
                        by = list(aliv_tstag =  alive_tstag$T.Stage), 
                        FUN = length)
        
        aliv_dta %>%
            hchart(type = "pie", hcaes(x = aliv_tstag, y = T.Stage)) %>%
            hc_title(text = "Alive Person With T3, T4 stage") %>%
            hc_colors(colors = c("#FF1493","#FFC0CB"))
    })
    
    output$chart2 <- renderHighchart({
        dead_tstag <- dataset %>%
            select(T.Stage,Status) %>%
            filter(Status %in% "Dead",T.Stage %in% c("T3","T4"))
        
        
        dead_dta <-aggregate(x = dead_tstag,
                             by = list(dead_tstag =  dead_tstag$T.Stage), 
                             FUN = length)
        dead_dta %>%
            hchart(type = "pie", hcaes(x = dead_tstag, y = T.Stage)) %>%
            hc_title(text = "Dead Person With T3, T4 stage") %>%
            hc_colors(colors = c("#FFC0CB","#FF1493"))
    })
    
    group_data <- data.frame(dataset$Age,dataset$Status,dataset$Tumor.Size)
    
    
    
    output$alive_by_age <- renderHighchart({
        df <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(df) <- c("range","Alive","death")
        bin <- input$bin_input
        validate(
            need(input$bin_input != "", "Please enter a Range")
        )
        if (input$bin_input ==0 ) {
            bin <- 1
        }
        
        for (i in seq( min(group_data$dataset.Age),max(group_data$dataset.Age),bin)) {
            dta <- group_data %>%
                select(dataset.Age, dataset.Status) %>%
                filter(dataset.Age > (i-1) , dataset.Age < (i+bin+1))
            alive <- filter(dta,dataset.Status == "Alive")
            dead <- filter(dta,dataset.Status == "Dead")
            alive_perc <- round((length(alive$dataset.Age)/length(dta$dataset.Age))*100,2)
            dead_perc <- round((length(dead$dataset.Age)/length(dta$dataset.Age))*100,2)
            temp_row <- data.frame(paste(as.character(i),'-',i+bin),alive_perc , dead_perc)
            colnames(temp_row) <- c("range","Alive","death")
            df <- rbind(df,temp_row)
        }
        colnames(df) <- c("range","Alive","Dead")
        df %>%
            hchart(type = "bar", hcaes(x = range, y = Alive)) %>%
            hc_title(text = "Alive % of Diagnose") %>%
            hc_colors(colors = c("#FF69B4")) %>%
            hc_tooltip(text = "%")
    })
    
    
    
    
    output$dead_by_age <- renderHighchart({
        df <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(df) <- c("range","Alive","death")
        bin <- input$bin_input
        validate(
            need(input$bin_input != "", "Please enter a Range")
        )
        if (input$bin_input ==0 ) {
            bin <- 1
        }
        
        for (i in seq( min(group_data$dataset.Age),max(group_data$dataset.Age),bin)) {
            dta <- group_data %>%
                select(dataset.Age, dataset.Status) %>%
                filter(dataset.Age > (i-1) , dataset.Age < (i+bin+1))
            alive <- filter(dta,dataset.Status == "Alive")
            dead <- filter(dta,dataset.Status == "Dead")
            alive_perc <- round((length(alive$dataset.Age)/length(dta$dataset.Age))*100,2)
            dead_perc <- round((length(dead$dataset.Age)/length(dta$dataset.Age))*100,2)
            temp_row <- data.frame(paste(as.character(i),'-',i+bin),alive_perc , dead_perc)
            colnames(temp_row) <- c("range","Alive","death")
            df <- rbind(df,temp_row)
        }
        colnames(df) <- c("range","Alive","Dead")
        df %>%
            hchart(type = "bar", hcaes(x = range, y = Dead)) %>%
            hc_title(text = "Dead % of Diagnose") %>%
            hc_colors(colors = c("#FF69B4")) %>%
            hc_tooltip(text = "%")
    })
    
    
    
    # Tumor size and alive %
    output$alive_by_tumor <- renderHighchart({
        df <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(df) <- c("size","Alive","death")
        bin <- input$size_input
        validate(
            need(input$size_input != "", "Please enter a size Range")
        )
        if (input$size_input ==0 ) {
            bin <- 1
        }
        
        for (i in seq( min(group_data$dataset.Tumor.Size),max(group_data$dataset.Tumor.Size),bin)) {
            dta <- group_data %>%
                select(dataset.Tumor.Size, dataset.Status) %>%
                filter(dataset.Tumor.Size > (i-1) , dataset.Tumor.Size < (i+bin+1))
            alive <- filter(dta,dataset.Status == "Alive")
            dead <- filter(dta,dataset.Status == "Dead")
            alive_perc <- round((length(alive$dataset.Tumor.Size)/length(dta$dataset.Tumor.Size))*100,2)
            dead_perc <- round((length(dead$dataset.Tumor.Size)/length(dta$dataset.Tumor.Size))*100,2)
            temp_row <- data.frame(paste(as.character(i),'-',i+bin),alive_perc , dead_perc)
            colnames(temp_row) <- c("size","Alive","death")
            df <- rbind(df,temp_row)
        }
        colnames(df) <- c("size","Alive","Dead")
        df %>%
            hchart(type = "bar", hcaes(x = size, y = Alive)) %>%
            hc_title(text = "Alive % with Tumor Size") %>%
            hc_colors(colors = c("#FF69B4")) %>%
            hc_tooltip(text = "%")
    })
    
    
    
    # Tumor size and dead %
    output$dead_by_tumor <- renderHighchart({
        df <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(df) <- c("size","Alive","death")
        bin <- input$size_input
        validate(
            need(input$size_input != "", "Please enter a size Range")
        )
        if (input$size_input ==0 ) {
            bin <- 1
        }
        
        for (i in seq( min(group_data$dataset.Tumor.Size),max(group_data$dataset.Tumor.Size),bin)) {
            dta <- group_data %>%
                select(dataset.Tumor.Size, dataset.Status) %>%
                filter(dataset.Tumor.Size > (i-1) , dataset.Tumor.Size < (i+bin+1))
            alive <- filter(dta,dataset.Status == "Alive")
            dead <- filter(dta,dataset.Status == "Dead")
            alive_perc <- round((length(alive$dataset.Tumor.Size)/length(dta$dataset.Tumor.Size))*100,2)
            dead_perc <- round((length(dead$dataset.Tumor.Size)/length(dta$dataset.Tumor.Size))*100,2)
            temp_row <- data.frame(paste(as.character(i),'-',i+bin),alive_perc , dead_perc)
            colnames(temp_row) <- c("size","Alive","death")
            df <- rbind(df,temp_row)
        }
        colnames(df) <- c("size","Alive","Dead")
        df %>%
            hchart(type = "bar", hcaes(x = size, y = Dead)) %>%
            hc_title(text = "Dead % with Tumor Size") %>%
            hc_colors(colors = c("#FF69B4")) %>%
            hc_tooltip(text = "%")
    })

    
    # This is for chart using N Stage
    # ----------
        
    updateSelectInput(session,inputId = "inp_nstag_chrt",choices = unique(dataset$N.Stage))
    
    output$nstag_chrt <- renderHighchart({
        validate(
            need(input$inp_nstag_chrt != "", "Select N Stage")
        )
        
        dta <- dataset %>%
            select(N.Stage, Status) %>%
            filter(N.Stage %in% input$inp_nstag_chrt, )
        
        dta <-aggregate(x = dta,
                        by = list(uniq = dta$Status), 
                        FUN = length)
        dta %>%
            hchart(type = "pie", hcaes(x = uniq, y = Status)) %>%
            hc_title(text = "Alive/Dead using N Stage") %>%
            hc_colors(colors = c("#FF69B4","#FF1493"))
        
    })
    
    updateSelectInput(session,inputId = "inp_x6stag_chrt",choices = unique(dataset$X6th.Stage))
    
    output$x6stag_chrt <- renderHighchart({
        validate(
            need(input$inp_x6stag_chrt != "", "Select 6th Stage")
        )
        
        dta <- dataset %>%
            select(X6th.Stage, Status) %>%
            filter(X6th.Stage %in% input$inp_x6stag_chrt, )
        
        dta <-aggregate(x = dta,
                        by = list(uniq = dta$Status), 
                        FUN = length)
        dta %>%
            hchart(type = "pie", hcaes(x = uniq, y = Status)) %>%
            hc_title(text = "Alive/Dead using 6th Stage") %>%
            hc_colors(colors = c("#FF69B4","#FF1493"))
        
    })
     output$chart3 <- renderHighchart({
         
         mpgg <- dataset %>%
             filter(T.Stage %in% unique(dataset$T.Stage)) %>%
             group_by(T.Stage, Estrogen.Status) %>%
             summarize(count = n())
         
         categories_grouped <- mpgg %>%
             group_by(name = T.Stage) %>%
             do(categories = .$Estrogen.Status) %>%
             list_parse()
         
         highchart() %>%
             hc_xAxis(categories = categories_grouped) %>%
             hc_add_series(data = mpgg, type = "bar", hcaes(y = count,  colors = Estrogen.Status),
                           showInLegend = F) %>%
             hc_title(text = "T Stage With Estrogen Status")
         
     })
     
     
     output$chart4 <- renderHighchart({
         mpgg <- dataset %>%
             filter(T.Stage %in% unique(dataset$T.Stage)) %>%
             group_by(T.Stage, Progesterone.Status) %>%
             summarize(count = n())
         
         categories_grouped <- mpgg %>%
             group_by(name = T.Stage) %>%
             do(categories = .$Progesterone.Status) %>%
             list_parse()
         
         highchart() %>%
             hc_xAxis(categories = categories_grouped) %>%
             hc_add_series(data = mpgg, type = "bar", hcaes(y = count,  colors = Progesterone.Status),
                           showInLegend = F) %>%
             hc_title(text = "T Stage With Progesterone Status")
         
     })
     
     
     #  T.Stage with N.Stage
     output$chart5 <- renderHighchart({
         mpgg <- dataset %>%
             filter(T.Stage %in% unique(dataset$T.Stage)) %>%
             group_by(T.Stage, N.Stage) %>%
             summarize(count = n())
         
         categories_grouped <- mpgg %>%
             group_by(name = T.Stage) %>%
             do(categories = .$N.Stage) %>%
             list_parse()
         
         highchart() %>%
             hc_xAxis(categories = categories_grouped) %>%
             hc_add_series(data = mpgg, type = "bar", hcaes(y = count,  colors = N.Stage),
                           showInLegend = F) %>%
             hc_title(text = "T Stage With N Stage")
         
     })
     
     
     # T.Stage with X6th.Stage
     output$chart6 <- renderHighchart({
         mpgg <- dataset %>%
             filter(T.Stage %in% unique(dataset$T.Stage)) %>%
             group_by(T.Stage, X6th.Stage) %>%
             summarize(count = n())
         
         categories_grouped <- mpgg %>%
             group_by(name = T.Stage) %>%
             do(categories = .$X6th.Stage) %>%
             list_parse()
         
         highchart() %>%
             hc_xAxis(categories = categories_grouped) %>%
             hc_add_series(data = mpgg, type = "bar", hcaes(y = count,  colors = X6th.Stage),
                           showInLegend = F) %>%
             hc_title(text = "6th Stage With 6th Stage")
     })
     
     
     # T.Stage with Grade
     output$chart7 <- renderHighchart({
         mpgg <- dataset %>%
             filter(T.Stage %in% unique(dataset$T.Stage)) %>%
             group_by(T.Stage, Grade) %>%
             summarize(count = n())
         
         categories_grouped <- mpgg %>%
             group_by(name = T.Stage) %>%
             do(categories = .$Grade) %>%
             list_parse()
         
         highchart() %>%
             hc_xAxis(categories = categories_grouped) %>%
             hc_add_series(data = mpgg, type = "bar", hcaes(y = count,  colors = Grade),
                           showInLegend = F) %>%
             hc_title(text = "T Stage With Grade")
     })
     
     
     # T.Stage with A.Stage
     output$chart8 <- renderHighchart({
         mpgg <- dataset %>%
             filter(T.Stage %in% unique(dataset$T.Stage)) %>%
             group_by(T.Stage, A.Stage) %>%
             summarize(count = n())
         
         categories_grouped <- mpgg %>%
             group_by(name = T.Stage) %>%
             do(categories = .$A.Stage) %>%
             list_parse()
         
         highchart() %>%
             hc_xAxis(categories = categories_grouped) %>%
             hc_add_series(data = mpgg, type = "bar", hcaes(y = count,  colors = A.Stage),
                           showInLegend = F) %>%
             hc_title(text = "T Stage With 6th Stage")
     })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

