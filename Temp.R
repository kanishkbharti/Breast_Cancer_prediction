library(e1071)
plot(iris)

plot(iris$Sepal.Length,iris$Sepal.Width, col = iris$Species)

plot(iris$Petal.Length,iris$Petal.Width, col = iris$Species)

s <- sample(150,100)
col <- c("Petal.Length" ,"Petal.Width",  "Species")

iris_train <- iris[s,col]
iris_test <- iris[-s,col]

data(mtcars)

 # Sun Apr 14 22:06:16 2019 ------------------------------ This code is to split data into train and test 
 
 
## 75% of the sample size
smp_size <- floor(0.75 * nrow(dataset))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

### this code will generate list of variables that are important
### with respect to our target variabel

library(randomForest)
rm <- randomForest(T.Stage~., data=dataset)
rm$importance 

train <- dataset %>%
  select(Age, T.Stage, N.Stage,X6th.Stage, A.Stage, Tumor.Size,Estrogen.Status,Progesterone.Status,Regional.Node.Examined,Reginol.Node.Positive,Survival.Months,Status )

knn.fit <- kknn(T.Stage~., train ,train[45,],k = 5, distance = 2,scale = F)

x <- knn.fit$fitted.values

as.character(x)

colnames(train) <- c("Age", "T.Stage", "N.Stage","X6th.Stage", "A.Stage", "Tumor.Size","Estrogen.Status","Progesterone.Status","Regional.Node.Examined","Survival.Months","Status" )

#  input$inp_age, n_stage, 6th_stage, inp_Astage, inp_tumor_size, inp_Est_stat, inp_Pro_stat, reg_nod_exam, surv_month

model.SVM <- svm(T.Stage~., train, kernel ='linear')

summary(model.SVM)

pc <- predict(model.SVM, train[750,], type = "class")

pc

svmd <-  as.data.frame(pc)

head(svmd)

xx <- svmd %>%
  select(pc) %>%
  filter(pc %in% "T4")

xx

library(caret)

svmpro <- rfe(train, log)

model <- train

#  race with t3, t4 stage
head(data_set)

race_n_tstg <- dataset %>%
  select(Race,T.Stage) %>%
  filter(T.Stage %in% c("T3","T4"))

dta <-aggregate(x = race_n_tstg,
                by = list(unique.race_n_tstg = race_n_tstg$Race), 
                FUN = length)

dta
race_n_tstg

plot_ly(race_n_tstg,labels = ~dta$unique.race_n_tstg, values =~ dta$Race, type = "pie" )

library(ggplot2)

bp<- ggplot(dta, aes(x="", y=dta$Race, fill=dta$unique.race_n_tstg))+
  geom_bar(width = 1, stat = "identity")



pie <- bp + coord_polar("y", start=0)
pie 
pie + scale_fill_brewer(palette = "Blues")+
  theme_minimal()

dta %>%
  hchart(type = "pie", hcaes(x = unique.race_n_tstg, y = Race)) %>%
  hc_title(text = "Pie Chart") %>%
  hc_colors(colors = c("blue","gray","Coral"))

group_data <- data.frame(dataset$Age,dataset$Status)
head(group_data)

df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) <- c("range","Alive","death")
bin <- 12
distribute <- seq( min(group_data$dataset.Age),max(group_data$dataset.Age),bin)
for (i in distribute) {
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
  # if (as.character(i) == as.character(tail(distribute,1))) {
  #   print(i)
  #   dta <- group_data %>%
  #     select(dataset.Age, dataset.Status) %>%
  #     filter(dataset.Age >  min(group_data$dataset.Age))
  #   alive <- filter(dta,dataset.Status == "Alive")
  #   dead <- filter(dta,dataset.Status == "Dead")
  #   alive_perc <- round((length(alive$dataset.Age)/length(dta$dataset.Age))*100,2)
  #   dead_perc <- round((length(dead$dataset.Age)/length(dta$dataset.Age))*100,2)
  #   temp_row <- data.frame(paste('>',tail(distribute,1)),alive_perc , dead_perc)
  #   tail(distribute,1)
  #   colnames(temp_row) <- c("range","Alive","death")
  #   df <- rbind(df,temp_row)
  # }
}


df

df %>%
  hchart(type = "bar", hcaes(x = range, y = death)) %>%
  hc_title(text = "Race with T3,T4 Stage Cancer") %>%
  hc_colors(colors = c("#FF69B4")) %>%
  hc_tooltip(text = "%")

newtask <- dataset %>%
  select(N.Stage, Status) %>%
  filter(N.Stage %in% c("N1","N2") )

dta <-aggregate(x = newtask,
                by = list(uniq = newtask$Status), 
                FUN = length)

dta <- dataset %>%
  select(X6th.Stage, Status) %>%
  filter(X6th.Stage %in% "IIA" )

dta <-aggregate(x = dta,
                by = list(uniq = dta$Status), 
                FUN = length)

head(newtask)
head(dta)

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
                showInLegend = T) %>%
  hc_colors(colors = c("#00FFFF","#00FFFF"))

