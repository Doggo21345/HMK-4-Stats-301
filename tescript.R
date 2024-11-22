#Spotify playlists slide 1 
pacman::p_load(tidyverse, moderndive,mosaic, ggplot2,effectsize)
#Most frequnt artists
most_frequnt_artisits <-  holiday %>%  
  group_by(artist) %>%  
  summarise(count = n()) %>%  
  arrange(desc(count)) %>%  
  slice(1:3)
most_frequnt_artisits
#Most frequnt tracks 
most_frequnt_songs <-  holiday %>%  
  group_by(track) %>%  
  summarise(count = n()) %>%  
  arrange(desc(count)) %>%  
  slice(1:3)
most_frequnt_songs

#Highest energy level 

holiday %>%  
  group_by(artist, track)  %>% 
  summarise(max_energy = max(energy)) %>% 
  arrange(desc(max_energy))

highest_enery_level <- holiday %>%  
  arrange(desc(energy)) %>% 
  slice(1)

highest_enery_level
#lowest valecnce 
holiday %>%  
  group_by(artist, track)  %>% 
  summarise(min_valence = min(valence)) %>% 
  arrange((min_valence))

lowest_valence <- holiday %>%  
  arrange(valence) %>% 
  slice(1) %>%  
  select(artist, track)
lowest_valence



#Slide 2 of the Holiday 
ggplot(aes(x = key, y = danceability), data = holiday) + 
  geom_boxplot(fill = "#4CAF50", color = "red") +  #
  theme_minimal(base_size = 15) +  
  labs(
    title = "Danceability by Key",
    x = "Key of the Song",
    y = "Danceability of the Song"
  ) +
  theme(
    plot.title = element_text(face= "bold", size = 18, hjust = .5),
    axis.title.x = element_text(face = "bold", size = 14),             
    axis.title.y = element_text(face = "bold", size = 14),             
    axis.text.x = element_text(size = 12),                             
    axis.text.y = element_text(size = 12)                              
  )

B <-  holiday %>%  
  filter(key == "B") 

  
median(B$danceability)




# Dallas shelter 

#least amount of beagles
beagles <-  shelter %>%  
  group_by(month) %>%  
  filter(animal_breed == "BEAGLE") %>%  
  summarise(count = n()) %>%  
  arrange(count) %>%  
  slice(1:2)
beagles 
#Most amount of wildlie
wildlife <-  shelter %>%  
  group_by(month) %>%  
  filter(animal_type == "WILDLIFE") %>%  
  summarise(count = n()) %>%  
  arrange(desc(count)) %>%  
  slice(1:2)
wildlife

#Next thing 
breeds <- shelter %>%
  group_by(animal_breed) %>%
  summarize(
    animals = n(),
    adoption_rate = sum(outcome_type == 'ADOPTION') / animals
  ) %>% 
  filter(animals >= 25) %>%  
  arrange(desc(adoption_rate)) %>%  
  slice(1:10)
breeds

#Slide 2 cats and Dogs 
shelter = shelter %>%
  mutate(dog = ifelse(animal_type == 'DOG', 'dog', 'not_dog'),
         cat = ifelse(animal_type == 'CAT', 'cat', 'not_cat'),
         stray = ifelse(intake_type == 'STRAY', 'stray', 'other_intake'),
         surrendered = ifelse(intake_type == 'OWNER SURRENDER', 'surrendered', 'other_intake'))

#proportiom of dog
proportion_dog <- mean(shelter$dog == "dog")
proportion_dog
proportion_neither <- mean(shelter$animal_type != "DOG" & shelter$animal_type != "CAT")
proportion_neither
total_not_stray <- sum(shelter$intake_type != "STRAY")
total_not_stray_not_dog <- sum(shelter$intake_type != "STRAY" & shelter$animal_type != "DOG")
total_not_stray_not_dog / total_not_stray


total_cats <- mean(shelter$animal_type == "CAT")
total_animals <- nrow(shelter)
p_cat <- total_cats / total_animals

total_cats

#surrendered
surrenderd <-  mean(shelter$intake_type == "OWNER SURRENDER")

surrendered_data <- shelter %>%
  filter(intake_type == "OWNER SURRENDER") %>%
  group_by(animal_type) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

surrendered_data

# Calculate the proportion of owner-surrendered animals that are cats
owner_surrendered <- shelter %>% 
  filter(intake_type == "OWNER SURRENDER")
total_owner_surrendered <- nrow(owner_surrendered)
surrendered_cats <- sum(owner_surrendered$animal_type == "CAT")
p_cat_given_owner_surrender <- surrendered_cats / total_owner_surrendered

p_cat_surrendered <-  mean(shelter$intake_type == "OWNER SURRENDER" & shelter$animal_type == "CAT" )

# Compare the two proportions
p_cat*surrenderd
p_cat_surrendered


p_cat_given_owner_surrender





ggplot(shelter, aes(x = cat, fill = surrendered)) +
  geom_bar(position = "fill") +
  labs(
    title = "Association Between Being Surrendered and Being a Cat",
    x = "Animal Type (Cat or Not)",
    y = "Proportion",
    fill = "Surrendered Status"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

#Summer is coming 

#Slide 1

Summer <- lm(power ~ temperature + weekday + temperature:weekday, data = ERCOT ) 
get_regression_table(Summer, conf.level = .95) %>%  
  select(-std_error, -statistic, -p_value)

#Slide 2 Scatter plot 
ggplot(ERCOT) + 
  geom_point(aes(x = temperature, y = power), alpha = 0.1, color = '#bf5700') + 
  geom_smooth(aes(x = temperature, y = power), method = 'lm', color = 'blue') + 
  facet_wrap(~weekday) +
  labs(
    title = "Power Consumption vs. Temperature by Day Type",
    x = "Temperature (°C)",
    y = "Power Consumption (MW)"
  ) +
  theme_minimal()

#Would it want me to take the R squared value of the thing that I fit or something else 
rsquared(Summer)

summary(Summer)
residual_standard_error <- summary(Summer)$sigma 
residual_standard_error_rounded <- round(residual_standard_error)
model_results <- broom::tidy(Summer, conf.int = TRUE)







#4 Redlining how does it actually want me to do this is this the correct way for slide 1?
redline <-  lm(FAIR ~ minority, data = redlining)
coef(redline)
get_regression_table(redline)
rsquared(redline)

ggplot(redlining) + 
  geom_point(aes(x = minority, y = FAIR, color = zip)) + 
  geom_smooth(aes(x = minority, y = FAIR), method = 'lm') +
  labs(
    title = "FAIR Rating vs. Minority Proportion",
    x = "Minority Proportion",
    y = "FAIR Rating",
    color = "ZIP Code"
  ) +
  theme_minimal()

#Slide 2 
redline2 <- lm(FAIR ~ minority + fires + age + income, data = redlining)
get_regression_table(redline2, conf.level = 0.95, digits=2)

standardize_parameters(redline2)
