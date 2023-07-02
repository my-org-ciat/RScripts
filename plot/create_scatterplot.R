library(hydroGOF)
library(shapviz)

setwd("C:\\Users\\ATilaye\\Documents\\01My_Docs\\01CIAT\\02Fertilizer\\rfe_fertilizer\\model_with_38_vars\\new_model_colrename\\data\\workspace")
model <- readRDS("model_colrename.rds")
training <- read.csv("training_colrename.csv", header = T, sep = ",")
regmat <- read.csv("regmat_colrename.csv", header = T, sep = ",")
testing <- read.csv("testing_colrename.csv", header = T, sep = ",")
setwd("../../shap/")

shap_final <- readRDS("shap_result_new.rds")

var_imp <- varImp(model)
ggplot(var_imp)

ggsave(filename =
         paste0(
           "./output/","variable_importance_38",".png"
         ),width = 20, height = 10)

pred_test <- predict(model, testing) %>% as.data.frame()
pred_train <- predict(model, training) %>% as.data.frame()

observed_train <- cbind(training$gr_yield) %>% as.data.frame()
observed_test <- cbind(testing$gr_yield) %>% as.data.frame()

train_plot <- cbind(pred_train, observed_train) %>% cbind("Train") %>% 
  as.data.frame()

test_plot <- cbind(pred_test, observed_test) %>% cbind("Test") %>% 
  as.data.frame()
colnames(train_plot) <- colnames(test_plot) <- c("Predicted", "Observed", "Set")

data_plot <- rbind(train_plot, test_plot)
dim(data_plot)

#scatter plot matrix
ggplot(data = data_plot, 
       aes(x = Observed, y = Predicted))  + 
  geom_point()+
  geom_smooth()+
  labs(x = "Observed",
       y = "Predicted",
       color = "Set") +
  scale_color_manual(values = colors)

p <- ggplot(data_plot, aes(x = Observed, y = Predicted)) +
  geom_point(aes(color = Set))+
  geom_smooth(colour = "black")

#final ggplot drawn
p + theme_bw() + scale_color_manual(values=c("#b35900", "#00ace6"))
p+theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank()
)        







  

