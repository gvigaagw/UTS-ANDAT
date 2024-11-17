setwd("D:/Praktikum_Viga")
#readdata
data=read.csv2("depression.CSV", sep=";", header = TRUE)
data
#check the packaging
str(data)
nrow(data)
ncol(data)
summary(data)
#Look at the top and the bottom of your data
head(data$bmi)
head(data$depression_diagnosis)
tail(data$bmi)
tail(data$depression_diagnosis)
#checkyour"n"s
head(table(data$bmi))
head(table(data$depression_diagnosis))
library(dplyr)
# Menghapus baris yang memiliki nilai NA pada kolom 'bmi' dan 'depression_diagnosis'
cleaned_data <- data %>%
  filter(!is.na(bmi), !is.na(depression_diagnosis), !is.na(phq_score))
# Melihat data setelah missing values dihilangkan
print(cleaned_data$depression_diagnosis)
print(cleaned_data$phq_score)
print(cleaned_data$bmi)

#f.	Validate with at least one external data source
summary(cleaned_data$bmi)
quantile(cleaned_data$bmi, seq(0,1, 0.1))
#datadepression_diagnosis
confusion_matrix <- table(cleaned_data$depression_diagnosis)
# Menampilkan tabel kontingensi
print("Tabel Kontingensi (Confusion Matrix):")
print(confusion_matrix)
print("Ringkasan Data (Depression_Diagnosis):")
summary(cleaned_data$depression_diagnosis)
#G MAKE A PLOT
cleaned_data <- cleaned_data %>%
  mutate(depression_logical = depression_diagnosis == 1)  # Adjust as needed if 1 indicates diagnosed

# Create a boxplot of depression diagnosis by BMI
ggplot(cleaned_data, aes(x = bmi, y = depression_diagnosis)) +
  geom_boxplot() +
  labs(title = "Boxplot of Depression Diagnosis by BMI",
       x = "BMI",
       y = "Depression Diagnosis") +
  theme_minimal()

# FOLLOWUP
cleaned_data <- cleaned_data %>%
  mutate(depression_logical = depression_diagnosis == 1) %>%  # TRUE jika diagnosed
  filter(!is.na(bmi) & !is.na(depression_logical))  # Menghilangkan missing data

# Melakukan regresi logistik biner
model <- glm(depression_logical ~ bmi, data = cleaned_data, family = "binomial")

# Melihat ringkasan hasil model
summary(model)

# Menghitung odds ratio untuk interpretasi
exp(coef(model))

#3 A

# Mengonversi variabel dependen menjadi bentuk logical
cleaned_data$depression_diagnosis <- cleaned_data$depression_logical == 1  # TRUE jika diagnosis depresi

# Membuat model regresi logistik
model <- glm(depression_logical ~ bmi, data = cleaned_data, family = "binomial")

# Melihat ringkasan model
summary(model)

#3B
# Plot histogram untuk PHQ Score berdasarkan diagnosis depresi dengan kurva normal overlay
bmi_plot <- ggplot(cleaned_data, aes(x = bmi, fill = depression_diagnosis)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 1, alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(cleaned_data$bmi, na.rm = TRUE), 
                                         sd = sd(cleaned_data$bmi, na.rm = TRUE)), 
                color = "red", size = 1) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink")) +
  labs(title = "Distribusi BMI dengan Kurva Normal dan Diagnosis Depresi",
       x = "BMI",
       y = "Kepadatan",
       fill = "Diagnosis Depresi") +
  theme_minimal()
bmi_plot

# Membuat data distribusi normal untuk perbandingan
normal_data <- rnorm(n = nrow(cleaned_data), mean = mean(cleaned_data$depression_logical, na.rm = TRUE), 
                     sd = sd(cleaned_data$depression_logical, na.rm = TRUE))

# Plot histogram distribusi normal untuk perbandingan
ggplot(data.frame(normal_data), aes(x = normal_data)) +
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram Distribusi Normal Simulasi",
       x = "Simulasi Nilai (Distribusi Normal)",
       y = "Frekuensi") +
  theme_minimal()


#ULANG (2)
#check your n
cleaned_data <- cleaned_data %>%
  mutate(depression_logical = depression_diagnosis == 1) %>%  # TRUE jika diagnosed
  filter(!is.na(bmi) & !is.na(depression_logical) & !is.na(phq_score))  # Menghilangkan missing data

#validate 2
summary(cleaned_data$phq_score)
quantile(cleaned_data$phq_score, seq(0,1, 0.1))

#MAKE A PLOT (2)
library(ggplot2)
library(tidyr)

# Install dan load library reshape2 jika belum
# install.packages("reshape2")
library(reshape2)
library(ggplot2)

# Menggunakan melt untuk merubah data menjadi bentuk panjang
cleaned_data_long <- melt(cleaned_data, id.vars = "depression_diagnosis", 
                          measure.vars = c("bmi", "phq_score"),
                          variable.name = "variable", value.name = "value")

# Membuat boxplot dengan dua variabel (BMI dan PHQ score) pada sumbu X
ggplot(cleaned_data_long, aes(x = value, y = depression_diagnosis)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Boxplot of Depression Diagnosis by BMI and PHQ Score",
       x = "BMI and PHQ Score",
       y = "Depression Diagnosis") +
  theme_minimal()

# Menghitung statistik deskriptif untuk BMI dan PHQ score berdasarkan diagnosis depresi
library(dplyr)

# Menghitung statistik deskriptif untuk BMI dan PHQ score
descriptive_stats <- cleaned_data %>%
  group_by(depression_diagnosis) %>%
  summarise(
    mean_bmi = mean(bmi, na.rm = TRUE),
    median_bmi = median(bmi, na.rm = TRUE),
    mean_phq = mean(phq_score, na.rm = TRUE),
    median_phq = median(phq_score, na.rm = TRUE),
    min_bmi = min(bmi, na.rm = TRUE),
    max_bmi = max(bmi, na.rm = TRUE),
    min_phq = min(phq_score, na.rm = TRUE),
    max_phq = max(phq_score, na.rm = TRUE)
  )

# Menampilkan statistik deskriptif
print(descriptive_stats)


# Membuat model regresi logistik
logistic_model <- glm(depression_diagnosis ~ phq_score + bmi, 
                      data = cleaned_data, 
                      family = binomial)

# Menampilkan ringkasan hasil regresi logistik
summary(logistic_model)
# Menghitung odds ratio untuk interpretasi
exp(coef(logistic_model))

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Pastikan kolom depression_diagnosis sudah dalam bentuk faktor untuk visualisasi
cleaned_data$depression_diagnosis <- as.factor(cleaned_data$depression_diagnosis)


# Plot histogram untuk PHQ Score berdasarkan diagnosis depresi dengan kurva normal overlay
phq_plot <- ggplot(cleaned_data, aes(x = phq_score, fill = depression_diagnosis)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 1, alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(data$phq_score, na.rm = TRUE), 
                                         sd = sd(data$phq_score, na.rm = TRUE)), 
                color = "red", size = 1) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink")) +
  labs(title = "Distribusi PHQ Score dengan Kurva Normal dan Diagnosis Depresi",
       x = "PHQ Score",
       y = "Kepadatan",
       fill = "Diagnosis Depresi") +
  theme_minimal()

# Plot histogram untuk BMI berdasarkan diagnosis depresi dengan kurva normal overlay
bmi2_plot <- ggplot(cleaned_data, aes(x = bmi, fill = depression_diagnosis)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 1, alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(data$bmi, na.rm = TRUE), 
                                         sd = sd(data$bmi, na.rm = TRUE)), 
                color = "red", size = 1) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink")) +
  labs(title = "Distribusi BMI dengan Kurva Normal dan Diagnosis Depresi",
       x = "BMI",
       y = "Kepadatan",
       fill = "Diagnosis Depresi") +
  theme_minimal()

# Tampilkan plot
phq_plot
bmi2_plot
