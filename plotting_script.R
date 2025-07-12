library(tidyverse)
participants_data <- read.csv('data/participants_data.csv') 

participants_data <- select(participants_data, batch, gender, age)

participants_data <- filter(participants_data, batch == 2019)

mean_age <- mean(participants_data$age)

participants_data <- mutate(participants_data, 
       age_cass = ifelse(age > mean_age,
                         yes = 'old',
                         no = 'young'))

participants_data <- read.csv('data/participants_data.csv')
participants_data_sub <- participants_data %>% 
  select(batch, gender, age) %>% 
  filter(batch == 2019) %>% 
  mutate(mean_age = mean(age),
         age_class = ifelse(age > mean_age,
                            yes = 'old',
                            no = 'young'))

write.csv(participants_data_sub, file = 'data/participants_data_sub.csv',
          row.names = FALSE)


is.na(participants_data$km_home_to_office)

mean(participants_data$km_home_to_office,na.rm=TRUE)
median(participants_data$km_home_to_office,na.rm=TRUE)

participants_data %>% 
  group_by(batch, gender) %>% 
  summarise(mean_age = mean(age))


participants_data$gender[participants_data$gender == ''] <- NA

participants_data_test <- participants_data %>% 
  mutate(gender_fixed = ifelse(gender == '', yes = 'diverse', no = gender))



participants_data_test <-participants_data %>% 
  replace_na(list(km_home_to_office = 0))





participants_data %>% 
  na.omit()

?ifelse()
participants_data


plot(1:10, 1:10, type = 'l')


library(tidyverse)

# Change the barplot by creating a table of gender 
participants_barplot <- table(participants_data$academic_parents)

barplot(participants_barplot)

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
ggplot(data = participants_data, 
       aes(x = age, 
           y = number_of_siblings)) + 
  geom_point()

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
# with colors representing binary data 
# related to academic parents (color) 
# and working hours per day as bubble sizes (size).
ggplot(data = participants_data, 
       aes(x = age, 
           y = batch, 
           color = gender, 
           size = number_of_siblings)) + 
  geom_point()


# Create a scatterplot of iris petal length (y) 
# as a function of sepal length (x) 
# with colors representing iris species (color) 
# and petal width as bubble sizes (size).
ggplot(data = iris, 
       aes(x = Sepal.Width, 
           y = Sepal.Length, 
           color = Species, 
           size = Petal.Length))+ 
  geom_point()

# Create a plot with the diamonds data 
# of the carat (x) and the price (y)
plot1 <- ggplot(data = diamonds, 
                aes(x = cut, y = clarity, 
                    alpha = 0.2)) +
  geom_point()


# Create a plot with the diamonds data 
# of the log of carat (x) 
# and the log of price (y)
ggplot(data = diamonds,
       aes(x = log(depth),
           y = log(table),
           alpha = 0.2)) +
  geom_point()


# Create a smaller diamonds data set (top 100 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the color of the diamond as the color of the points. 
dsmall <- top_n(diamonds, n = 10)

ggplot(data = dsmall, aes(x = depth, 
                          y = price, 
                          color = cut)) + 
  geom_point()

# Create a smaller diamonds data set (top 40 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the cut of the diamond as the shapes for the points. 
dsmall <- top_n(diamonds, 
                n = 10)

ggplot( data = dsmall, 
        aes(x = carat, 
            y = depth, 
            shape = clarity)) + 
  geom_point()

# Create a plot of the diamonds data 
# with carat on the x-axis, price on the y-axis. 
# Use the inhibit function to set the alpha to 0.1 
# and color to blue.
ggplot(data = diamonds, 
       aes(x = depth, 
           y = price, 
           alpha = I(0.4), 
           color = I("green"))) + 
  geom_point()

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
dsmall <- top_n(diamonds, 
                n = 10)

ggplot(data = dsmall, 
       aes(x = depth, 
           y = price))+
  geom_point()+
  geom_smooth()


# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
# Use 'glm' as the option for the smoothing
dsmall <- top_n(diamonds, 
                n = 10)

ggplot(data = dsmall, 
       aes(x = depth, 
           y = price))+ 
  geom_point()+ 
  geom_smooth(method = 'glm')


# Change the boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = color, 
           y = carat)) + 
  geom_boxplot()

# Change the jittered boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = color, 
           y = carat)) + 
  geom_boxplot()+ 
  geom_jitter()


# Change the alpha to 0.4 to make 
# the scatter less transparent
ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat, 
           alpha = I(0.1))) + 
  geom_boxplot()+ 
  geom_jitter()


# Change the density plot so that the x-axis is carat 
# and the color is the diamond color
ggplot(data = diamonds, 
       aes(x = carat)) +
  geom_density()


# Change the density plot so that the x-axis is carat 
# the color is the diamond color
# and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds, 
       aes(x = price, 
           fill = cut, 
           alpha = I(0.5))) +
  geom_density()


# Create a plot of the mpg data with 
# manufacturer as the color and a linear model 'lm'
# as the smooth method
ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = class)) + 
  geom_point() +
  geom_smooth(method = "glm")



# subset the data to numeric only with select_if
part_data <- select_if(participants_data, 
                       is.numeric, na.rm = TRUE)
# use 'cor' to perform pearson correlation
# use 'round' to reduce correlation 
# results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
# use 'as.data.frame.table' to build a table
# with correlation values
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()


# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")


# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000")


# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


png(file = "cortile.png", width = 7, height = 6, units = "in", res = 300)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()



ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "cortile.png", device = 'png', height = 10, width = 15, units = 'cm')