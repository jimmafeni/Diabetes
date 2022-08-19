#group 7
#diabetes dataset

library(readr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(data.table)
library(dplyr)
library(finalfit)
#load data

df <- read_csv("diabetes_downsampled.csv")


# DISTRIBUTION OF CLASS
percentage <- prop.table(table(df$Diabetes_012))*100
cbind(freq=table(df$Diabetes_012), percentage=percentage)

#delete diabetes rows
# df <- df[ !(df$Diabetes_012 %in% 2), ]
# summary(df$Age)
# ff_glimpse(df)

# Displaying the coralation matrix
df_predictors <-  subset(df, select = -c(df$Diabetes_012))
corr <- cor(df_predictors, use = "pairwise.complete.obs")
# Visualize the correlation matrix
corrplot(corr, type="upper", order="hclust",
         col=rev(brewer.pal(n=22, name="RdYlBu")))
#plot the number of people with prediabetes 
#change diabetes to factor 
df$Diabetes_012 <- as.factor(df$Diabetes_012)
table(df$Diabetes_012)

ggplot(df, aes(x=Diabetes_012, fill=Diabetes_012)) + 
                               geom_bar() +
                               xlab("Diabetes Type") +
                               ylab("Number of Participants") +
                               ggtitle("Analysis of 'No-diabetes , Pre-diabetes & Diabetes'") + theme(plot.title = element_text(hjust = 0.5))+
                               scale_fill_discrete(name = "", labels = c("No diabetes", "Pre-diabetes","Diabetes") 
                                                )


#Exploring Relationships Between Predictors and Response
#age, sex, prediabetes
ggplot(df, aes(x= factor(Age), y=Sex, colour= Diabetes_012)) + 
  geom_boxplot(stat = "boxplot",
               position = "dodge") +
  geom_boxplot() + 
  geom_jitter(width = 0.2) +
  xlab("Age") +
  ylab("Sex") +
  ggtitle("Analysis of gender with different age groups") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_colour_discrete(name = "", labels = c("No diabetes", "Pre-diabetes","Diabetes"))


#age 
ggplot(df, aes(Age, fill = Diabetes_012))+
  geom_histogram(bins = 10, binwidth = .5)+
  labs(title = "",
       y = "Number of Participants",
       x = "Age")+
  scale_fill_discrete(name = "", labels = c("No-Diabetes","Pre-Diabetes",'Diabetes'))+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  theme(legend.position = "top") + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = "Distribution according to Age groups") 


#correlation with categorical variables - 0/1
a <- ggplot(df, aes(x = Diabetes_012, fill = factor(HighBP)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
        labels = c("No","Yes"))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  labs(subtitle = "High BP") 
plot(a)

b <- ggplot(df, aes(x = Diabetes_012, fill = factor(HighChol)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "HighChol")
  plot(b)

c <- ggplot(df, aes(x = Diabetes_012, fill = factor(CholCheck)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "CholCheck")


d <- ggplot(df, aes(x = Diabetes_012, fill = factor(Smoker)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Smoker")
e <- ggplot(df, aes(x = Diabetes_012, fill = factor(Stroke)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Stroke")

f <- ggplot(df, aes(x = Diabetes_012, fill = factor(HeartDiseaseorAttack)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "HeartDiseaseorAttack")

g <- ggplot(df, aes(x = Diabetes_012, fill = factor(PhysActivity)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "PhysActivity")

h <- ggplot(df, aes(x = Diabetes_012, fill = factor(Fruits)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Fruits") 

i <- ggplot(df, aes(x = Diabetes_012, fill = factor(Veggies)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Veggies")

j <-  ggplot(df, aes(x = Diabetes_012, fill = factor(HvyAlcoholConsump)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "HvyAlcoholConsump")

k <- ggplot(df, aes(x = Diabetes_012, fill = factor(AnyHealthcare)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "AnyHealthcare") 

l <- ggplot(df, aes(x = Diabetes_012, fill = factor(NoDocbcCost)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+ theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "NoDocbcCost") 
plot(l)

m <- ggplot(df, aes(x = Diabetes_012, fill = factor(DiffWalk)))+
  geom_bar(position = "fill")+
  theme_fivethirtyeight()+  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels  = c("No-diabetes","Pre-diabetes","Diabetes"))+
  scale_fill_manual(values = c("#999999", "#E69F00"), name = "",
                    labels = c("No","Yes"))+
  labs(subtitle = "DiffWalk") + theme(plot.subtitle = element_text(hjust = 0.5))
plot(m)

grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m)
#general health 
My_Theme <- theme(
  axis.title.x = element_text(size = 10, face = "bold"),
  axis.text.x = element_text(size = 10, face = "bold"),
  axis.title.y = element_text(size = 10, face = "bold"),
  axis.text.y = element_text(size = 8.5, face = "bold.italic",),
  plot.title = element_text(size = 20)) 

ggplot(data = df, mapping = aes(x = GenHlth, fill = Diabetes_012)) +
  geom_histogram( alpha=0.5, position="dodge", bins = 30, binwidth = 1)+
  ggtitle( label = "General Health")+
  My_Theme+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +  scale_fill_discrete(name = "", labels = c("No diabetes", "Pre-diabetes","Diabetes")) +
theme(legend.position = "right")


#education 
ggplot(data = df, mapping = aes(x = Education, fill = Diabetes_012)) +
  geom_histogram( alpha=0.5, position="dodge", bins = 20, binwidth = 1)+
  ggtitle( label = "Education")+
  My_Theme+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +  scale_fill_discrete(name = "", labels = c("No diabetes", "Pre-diabetes","Diabetes") 
    )  + theme(plot.title = element_text(hjust = 0.5))

  theme(legend.position = "right")
#income
ggplot(data = df, mapping = aes(x = Income, fill = Diabetes_012)) +
  geom_histogram( alpha=0.5, position="dodge", bins = 20, binwidth = 1)+
  ggtitle( label = "Income")+
  My_Theme+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +  scale_fill_discrete(name = "", labels = c("No diabetes", "Pre-diabetes","Diabetes") 
    )   + theme(plot.title = element_text(hjust = 0.5))

  theme(legend.position = "right")
#mental health
ff_glimpse(df)
df$MentHlth <- as.numeric(as.integer(df$MentHlth))
levels(df$MentHlth)
# BMI density plot 
summary(df$BMI)
ggplot(df, aes(x = BMI, fill = Diabetes_012)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "", labels = c("No-diabetes", "Pre-diabetes","Diabetes"))+
  ggtitle( label = " BMI")+ 
  theme(plot.title = element_text(hjust = 0.5))
# mental health
ggplot(df, aes(x =MentHlth, fill = Diabetes_012)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "", labels = c("No-diabetes", "Pre-diabetes","Diabetes"))+
  ggtitle( label = "Mental Health")+
  theme(plot.title = element_text(hjust = 0.5))
#physical health 
ggplot(df, aes(x = PhysHlth, fill = Diabetes_012)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "", labels = c("No-diabetes", "Pre-diabetes","Diabetes"))+
  ggtitle( label = "Physical Health")+
  theme(plot.title = element_text(hjust = 0.5))

