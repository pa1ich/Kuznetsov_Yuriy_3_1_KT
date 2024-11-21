# Kuznetsov_Yuriy_3_1_KT
D <- read.csv("HairEyeColor.csv")
View(D)

summary_table <- table(D$Sex)

# Вывод сводной таблицы
View(summary_table)
#-------------------------------------------------------------------------------------
# Создание сводной таблицы для подсчета мужчин и женщин по цвету волос
summary_m_w_hair <- xtabs(~ Hair + Sex, data = D)
View(summary_m_w_hair)

green_eyes <- D[D$Eye == "Green", ]
#-------------------------------------------------------------------------------------
# Создание сводной таблицы для подсчета мужчин и женщин с зелеными глазами
summary_green_eyes <- table(green_eyes$Sex)
View(summary_green_eyes)
#-------------------------------------------------------------------------------------
# Подсчет общего количества участников опроса
total_participants <- nrow(D)
#-------------------------------------------------------------------------------------
# Создание таблицы с общим количеством участников
total_table <- data.frame(Total_Participants = total_participants)
#-------------------------------------------------------------------------------------
# Вывод таблицы с общим количеством участников
View(total_table)
#-------------------------------------------------------------------------------------
install.packages("ggplot2")
library(ggplot2)
p <- ggplot(D, aes(x = Hair, fill = Hair)) +
  geom_bar() +  
  theme_minimal() +
  labs(title = "Распределение людей по цвету волос",
       x = "Цвет волос",
       y = "Количество людей")
print(p)
#-------------------------------------------------------------------------------------
#Распределение мужчин по цвету волос
p <- ggplot(D[D$Sex == "Male", ], aes(x = Hair, fill = Hair)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение мужчин по цвету волос",
       x = "Цвет волос",
       y = "Количество мужчин")
print(p)
#-------------------------------------------------------------------------------------
#Модальное значение для мужчин
hair_count_male <- table(D[D$Sex == "Male", ]$Hair)
mode_hair <- names(hair_count_male)[which.max(hair_count_male)]
print(paste("Модальное значение цвета волос среди мужчин:", mode_hair))
#-------------------------------------------------------------------------------------
#Распределения женщин по цвету волос
p_female <- ggplot(D[D$Sex == "Female", ], aes(x = Hair, fill = Hair)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение женщин по цвету волос",
       x = "Цвет волос",
       y = "Количество женщин")
print(p_female)
#-------------------------------------------------------------------------------------
#Модальное значение для женщин
hair_count_female <- table(D[D$Sex == "Female", ]$Hair)
mode_hair_female <- names(hair_count_female)[which.max(hair_count_female)]
print(paste("Модальное значение цвета волос среди женщин:", mode_hair_female))
#-------------------------------------------------------------------------------------
#Распределение людей по цвету глаз
p_eye <- ggplot(D, aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение людей по цвету глаз",
       x = "Цвет глаз",
       y = "Количество людей")
print(p_eye)
#-------------------------------------------------------------------------------------
#Модальное значение для цвета глаз
eye_count <- table(D$Eye)
mode_eye <- names(eye_count)[which.max(eye_count)]
print(paste("Модальное значение цвета глаз среди людей:", mode_eye))
#-------------------------------------------------------------------------------------
#Модальное значение для цвета глаз среди мужчин
eye_count_male <- table(D[D$Sex == "Male", ]$Eye)
mode_eye_male <- names(eye_count_male)[which.max(eye_count_male)]
print(paste("Модальное значение цвета глаз среди мужчин:", mode_eye_male))
#-------------------------------------------------------------------------------------
#Распределения женщин по цвету глаз
p_eye_female <- ggplot(D[D$Sex == "Female", ], aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение женщин по цвету глаз",
       x = "Цвет глаз",
       y = "Количество женщин")
print(p_eye_female)
#-------------------------------------------------------------------------------------
#Модальное значение для цвета глаз среди женщин
eye_count_female <- table(D[D$Sex == "Female", ]$Eye)
mode_eye_female <- names(eye_count_female)[which.max(eye_count_female)]
print(paste("Модальное значение цвета глаз среди женщин:", mode_eye_female))
#-------------------------------------------------------------------------------------
install.packages("dplyr")
install.packages("magrittr")  # Для использования оператора %>%
library(dplyr)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
D <- read.csv("CanPop.csv")
str(D)
#-------------------------------------------------------------------------------------
#Расчет средних значений
population_summary <- D %>%
  summarise(
    Mean = mean(population, na.rm = TRUE),
    Harmonic_Mean = ifelse(sum(population > 0) > 0, sum(population > 0) / sum(1/population[population > 0]), NA),  
    Geometric_Mean = exp(mean(log(population[population > 0]), na.rm = TRUE)),  
    Median = median(population, na.rm = TRUE)
  )
print(population_summary)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
ggplot(D, aes(x = year, y = population)) +
  geom_bar(stat = "identity", fill = "blue") +  
  theme_minimal() +
  labs(title = "Численность населения Канады по годам",
       x = "Год",
       y = "Численность населения") +
  scale_y_continuous(labels = scales::comma) 

q1 <- quantile(D$population, 0.25, na.rm = TRUE)
q3 <- quantile(D$population, 0.75, na.rm = TRUE)
#-------------------------------------------------------------------------------------
#Перечень годов, находящихся между первым и третьим квартилями
years_between_q1_q3 <- D %>%
  filter(population >= q1 & population <= q3) %>%
  select(year)  # Предполагается, что название колонки с годами "Year"
print(years_between_q1_q3)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#Скользящее среднее по три года
D <- D %>%
  arrange(year) %>%  
  mutate(Moving_Average = zoo::rollmean(population, 3, fill = NA, align = "center"))
#-------------------------------------------------------------------------------------
#Построение графика
p <- ggplot(D, aes(x = year)) +
  geom_line(aes(y = population, color = "Фактическое население"), size = 1) +
  geom_line(aes(y = Moving_Average, color = "Скользящее среднее"), size = 1) +
  theme_minimal() +
  labs(title = "Численность населения и Скользящее Среднее (по 3 года)",
       x = "Год",
       y = "Численность населения") +
  scale_color_manual(values = c("Фактическое население" = "blue", "Скользящее среднее" = "red")) +
  theme(legend.title = element_blank())
D <- read.csv("HairEyeColor.csv")
View(D)

summary_table <- table(D$Sex)

# Вывод сводной таблицы
View(summary_table)
#-------------------------------------------------------------------------------------
# Создание сводной таблицы для подсчета мужчин и женщин по цвету волос
summary_m_w_hair <- xtabs(~ Hair + Sex, data = D)
View(summary_m_w_hair)

green_eyes <- D[D$Eye == "Green", ]
#-------------------------------------------------------------------------------------
# Создание сводной таблицы для подсчета мужчин и женщин с зелеными глазами
summary_green_eyes <- table(green_eyes$Sex)
View(summary_green_eyes)
#-------------------------------------------------------------------------------------
# Подсчет общего количества участников опроса
total_participants <- nrow(D)
#-------------------------------------------------------------------------------------
# Создание таблицы с общим количеством участников
total_table <- data.frame(Total_Participants = total_participants)
#-------------------------------------------------------------------------------------
# Вывод таблицы с общим количеством участников
View(total_table)
#-------------------------------------------------------------------------------------
install.packages("ggplot2")
library(ggplot2)
p <- ggplot(D, aes(x = Hair, fill = Hair)) +
  geom_bar() +  
  theme_minimal() +
  labs(title = "Распределение людей по цвету волос",
       x = "Цвет волос",
       y = "Количество людей")
print(p)
#-------------------------------------------------------------------------------------
#Распределение мужчин по цвету волос
p <- ggplot(D[D$Sex == "Male", ], aes(x = Hair, fill = Hair)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение мужчин по цвету волос",
       x = "Цвет волос",
       y = "Количество мужчин")
print(p)
#-------------------------------------------------------------------------------------
#Модальное значение для мужчин
hair_count_male <- table(D[D$Sex == "Male", ]$Hair)
mode_hair <- names(hair_count_male)[which.max(hair_count_male)]
print(paste("Модальное значение цвета волос среди мужчин:", mode_hair))
#-------------------------------------------------------------------------------------
#Распределения женщин по цвету волос
p_female <- ggplot(D[D$Sex == "Female", ], aes(x = Hair, fill = Hair)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение женщин по цвету волос",
       x = "Цвет волос",
       y = "Количество женщин")
print(p_female)
#-------------------------------------------------------------------------------------
#Модальное значение для женщин
hair_count_female <- table(D[D$Sex == "Female", ]$Hair)
mode_hair_female <- names(hair_count_female)[which.max(hair_count_female)]
print(paste("Модальное значение цвета волос среди женщин:", mode_hair_female))
#-------------------------------------------------------------------------------------
#Распределение людей по цвету глаз
p_eye <- ggplot(D, aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение людей по цвету глаз",
       x = "Цвет глаз",
       y = "Количество людей")
print(p_eye)
#-------------------------------------------------------------------------------------
#Модальное значение для цвета глаз
eye_count <- table(D$Eye)
mode_eye <- names(eye_count)[which.max(eye_count)]
print(paste("Модальное значение цвета глаз среди людей:", mode_eye))
#-------------------------------------------------------------------------------------
#Модальное значение для цвета глаз среди мужчин
eye_count_male <- table(D[D$Sex == "Male", ]$Eye)
mode_eye_male <- names(eye_count_male)[which.max(eye_count_male)]
print(paste("Модальное значение цвета глаз среди мужчин:", mode_eye_male))
#-------------------------------------------------------------------------------------
#Распределения женщин по цвету глаз
p_eye_female <- ggplot(D[D$Sex == "Female", ], aes(x = Eye, fill = Eye)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(title = "Распределение женщин по цвету глаз",
       x = "Цвет глаз",
       y = "Количество женщин")
print(p_eye_female)
#-------------------------------------------------------------------------------------
#Модальное значение для цвета глаз среди женщин
eye_count_female <- table(D[D$Sex == "Female", ]$Eye)
mode_eye_female <- names(eye_count_female)[which.max(eye_count_female)]
print(paste("Модальное значение цвета глаз среди женщин:", mode_eye_female))
#-------------------------------------------------------------------------------------
install.packages("dplyr")
install.packages("magrittr")  # Для использования оператора %>%
library(dplyr)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
D <- read.csv("CanPop.csv")
str(D)
#-------------------------------------------------------------------------------------
#Расчет средних значений
population_summary <- D %>%
  summarise(
    Mean = mean(population, na.rm = TRUE),
    Harmonic_Mean = ifelse(sum(population > 0) > 0, sum(population > 0) / sum(1/population[population > 0]), NA),  
    Geometric_Mean = exp(mean(log(population[population > 0]), na.rm = TRUE)),  
    Median = median(population, na.rm = TRUE)
  )
print(population_summary)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
ggplot(D, aes(x = year, y = population)) +
  geom_bar(stat = "identity", fill = "blue") +  
  theme_minimal() +
  labs(title = "Численность населения Канады по годам",
       x = "Год",
       y = "Численность населения") +
  scale_y_continuous(labels = scales::comma) 

q1 <- quantile(D$population, 0.25, na.rm = TRUE)
q3 <- quantile(D$population, 0.75, na.rm = TRUE)
#-------------------------------------------------------------------------------------
#Перечень годов, находящихся между первым и третьим квартилями
years_between_q1_q3 <- D %>%
  filter(population >= q1 & population <= q3) %>%
  select(year)  # Предполагается, что название колонки с годами "Year"
print(years_between_q1_q3)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#Скользящее среднее по три года
D <- D %>%
  arrange(year) %>%  
  mutate(Moving_Average = zoo::rollmean(population, 3, fill = NA, align = "center"))
#-------------------------------------------------------------------------------------
#Построение графика
p <- ggplot(D, aes(x = year)) +
  geom_line(aes(y = population, color = "Фактическое население"), size = 1) +
  geom_line(aes(y = Moving_Average, color = "Скользящее среднее"), size = 1) +
  theme_minimal() +
  labs(title = "Численность населения и Скользящее Среднее (по 3 года)",
       x = "Год",
       y = "Численность населения") +
  scale_color_manual(values = c("Фактическое население" = "blue", "Скользящее среднее" = "red")) +
  theme(legend.title = element_blank())

print(p)
D <- read.csv("HairEyeColor.csv")
View(D)


# Kuznetsov_Yuriy_3_1_KT
