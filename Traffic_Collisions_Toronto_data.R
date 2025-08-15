# =========================================================
# Load Data
# =========================================================
library(tidyverse)
library(car)
library(FSA)
library(MASS)
library(broom)
library(scales)

df <- read_csv("C:/Users/komma/OneDrive/Desktop/archive/Traffic_Collisions_Toronto_data.csv")

# =========================================================
# Handle Missing Values
# =========================================================
df <- df %>%
  drop_na(Hour, Day_of_Week, Year, Injury_Collisions, Fatalities, Division, OccurrenceDate)

# =========================================================
# Remove Duplicates
# =========================================================
df <- df %>%
  distinct(EventUniqueId, .keep_all = TRUE)

# =========================================================
# Convert Variables to Appropriate Types
# =========================================================
df <- df %>%
  mutate(
    Injury_Collisions = toupper(trimws(as.character(Injury_Collisions))),
    FTR_Collisions    = toupper(trimws(as.character(FTR_Collisions))),
    OccurrenceDate    = sub("\\+00$|Z$", "", OccurrenceDate),
    Occ_DT            = suppressWarnings(as.POSIXct(OccurrenceDate, tz = "UTC")),
    Occ_Date          = as.Date(Occ_DT),
    Severity = ifelse(Fatalities > 0 | Injury_Collisions == "YES", "Severe", "Not Severe"),
    Hour        = as.integer(Hour),
    Day_of_Week = factor(Day_of_Week,
                         levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
    Year        = as.integer(Year),
    Division    = as.factor(Division),
    Severity    = factor(Severity)
  )

# =========================================================
# Remove Outliers (Example: Injury Count > 99th percentile)
# =========================================================
df <- df %>%
  mutate(INJURY_COUNT = ifelse(Injury_Collisions == "YES", 1L, 0L)) %>%
  filter(INJURY_COUNT <= quantile(INJURY_COUNT, 0.99))

# =========================================================
# Key Descriptive Statistics
# =========================================================
summary_stats <- df %>%
  summarise(
    mean_hour = mean(Hour, na.rm = TRUE),
    median_hour = median(Hour, na.rm = TRUE),
    sd_hour = sd(Hour, na.rm = TRUE),
    mean_fatalities = mean(Fatalities, na.rm = TRUE),
    median_fatalities = median(Fatalities, na.rm = TRUE),
    sd_fatalities = sd(Fatalities, na.rm = TRUE)
  )

freq_tables <- list(
  Severity = table(df$Severity),
  Day_of_Week = table(df$Day_of_Week),
  Division = table(df$Division)
)

print(summary_stats)
print(freq_tables$Severity)
print(freq_tables$Day_of_Week)
print(freq_tables$Division)

# =========================================================
# Exploratory Data Analysis (EDA)
# =========================================================
# Histogram for Hour
eda_hour <- df %>%
  count(Hour) %>%
  mutate(perc = n / sum(n))

ggplot(eda_hour, aes(x = Hour, y = perc, fill = factor(Hour))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Collision Distribution by Hour of Day",
       x = "Hour of Day",
       y = "Percentage of Collisions") +
  theme_minimal(base_size = 14)

#Severity proportion
eda_severity <- df %>%
  count(Severity) %>%
  mutate(perc = n / sum(n))

ggplot(eda_severity, aes(x = Severity, y = perc, fill = Severity)) +
  geom_col() +
  geom_text(aes(label = paste0(round(perc * 100, 1), "%")),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Collisions by Severity",
       x = "Severity", y = "Percentage") +
  theme_minimal()

# =========================================================
# Statistical Analyses (RQ1–RQ3)
# =========================================================

# ---------------------------------------------------------
# RQ1 — Do collision counts differ by police division?
# Tests: ANOVA (report) + Shapiro/Levene; Kruskal–Wallis + Dunn post-hoc
# ---------------------------------------------------------
anova_div <- aov(Collision_Count ~ Division, data = div_daily)
print(summary(anova_div))

# Assumption checks
res_div <- residuals(anova_div)
if (length(res_div) > 5000) { set.seed(123); res_div <- sample(res_div, 5000) }
sw_div  <- shapiro.test(res_div)
lev_div <- car::leveneTest(Collision_Count ~ Division, data = div_daily)

print(sw_div)
print(lev_div)

# Non-parametric global test (recommended for counts)
kw_div <- kruskal.test(Collision_Count ~ Division, data = div_daily)
print(kw_div)

# Post-hoc Dunn (Holm) if significant
if (kw_div$p.value < 0.05) {
  dunn_div <- FSA::dunnTest(Collision_Count ~ Division, data = div_daily, method = "holm")
  print(head(dunn_div$res, 10))
}

# Visual: mean daily collisions per Division (share labels)
div_means <- div_daily %>%
  group_by(Division) %>%
  summarise(mean_daily = mean(Collision_Count), .groups = "drop") %>%
  arrange(mean_daily) %>%
  mutate(share = mean_daily / sum(mean_daily))
print(head(div_means))

ggplot(div_means, aes(reorder(Division, mean_daily), mean_daily)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_text(aes(label = paste0(round(share * 100, 1), "%")), hjust = -0.15, size = 3.6) +
  coord_flip() +
  labs(title = "Mean Daily Collisions by Division (share %)", x = "Division (low → high)", y = "Mean Daily Collisions") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------
# RQ2 — Is collision severity associated with day of week?
# Test: Chi-square of independence + Cramér’s V
# ---------------------------------------------------------
df <- df %>%
  mutate(Severe_Status = factor(ifelse(Severity == "Severe", "Severe", "Non-Severe"),
                                levels = c("Non-Severe","Severe")))
severity_tab <- table(df$Day_of_Week, df$Severe_Status)
print(severity_tab)

chi_res <- chisq.test(severity_tab)
print(chi_res)
print(chi_res$expected)

# Cramér’s V
n_total <- sum(severity_tab); r <- nrow(severity_tab); c <- ncol(severity_tab)
cramersV <- sqrt(as.numeric(chi_res$statistic) / (n_total * min(r - 1, c - 1)))
print(cramersV)

# Visual: 100% stacked with % labels
severity_plot <- df %>%
  count(Day_of_Week, Severe_Status) %>%
  group_by(Day_of_Week) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(severity_plot, aes(Day_of_Week, prop, fill = Severe_Status)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 3.3) +
  scale_fill_manual(values = c("Non-Severe" = "tomato", "Severe" = "cyan3")) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, .02))) +
  labs(title = "Severe vs Non-Severe by Day of Week", x = "Day of Week", y = "Proportion", fill = "Severity") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# ---------------------------------------------------------
# RQ3 — Do injury collision counts vary by hour, day of week, and year?
# Model: Negative Binomial regression on daily injury counts
# ---------------------------------------------------------

# Robust injury count from Injury_Collisions (numeric counts OR YES/NO)
injury_num <- suppressWarnings(as.numeric(df$Injury_Collisions))
df <- df %>%
  mutate(INJURY_COUNT = if (all(is.na(injury_num))) {
    ifelse(Injury_Collisions == "YES", 1L, 0L)
  } else {
    as.integer(replace(injury_num, is.na(injury_num), 0L))
  })

# Aggregate to daily counts per Hour × Day_of_Week × Year
injury_daily <- df %>%
  filter(!is.na(Occ_Date), Hour >= 0, Hour <= 23) %>%
  group_by(Occ_Date, Hour, Day_of_Week, Year) %>%
  summarise(Injury_Count = sum(INJURY_COUNT), .groups = "drop") %>%
  mutate(
    Hour_f = factor(Hour),
    DOW_f  = factor(Day_of_Week,
                    levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
    Year_f = factor(Year)
  )
print(head(injury_daily))
print(summary(injury_daily$Injury_Count))

# Negative Binomial model
nb_mod  <- MASS::glm.nb(Injury_Count ~ Hour_f + DOW_f + Year_f, data = injury_daily)
print(summary(nb_mod))

# Dispersion check (Pearson χ² / df)
pearson_chisq <- sum(residuals(nb_mod, type = "pearson")^2)
dispersion    <- pearson_chisq / df.residual(nb_mod)
print(dispersion)

# IRR table (exponentiated coefficients with 95% CI)
irr_tab <- broom::tidy(nb_mod, exponentiate = TRUE, conf.int = TRUE)
print(head(irr_tab, 12))

# Visual A: Mean daily injury counts by Hour
hour_means <- injury_daily %>%
  group_by(Hour) %>%
  summarise(mean_injury = mean(Injury_Count), .groups = "drop") %>%
  mutate(share = mean_injury / sum(mean_injury))
print(head(hour_means))

ggplot(hour_means, aes(Hour, mean_injury)) +
  geom_col(fill = "tomato", color = "black") +
  geom_text(aes(label = paste0(round(share * 100, 1), "%")), vjust = -0.4, size = 3.6) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Mean Daily Injury Collision Count by Hour", x = "Hour of Day", y = "Mean Daily Injury Count") +
  theme_minimal()

# Visual B: Hour × Day heatmap of mean daily injury counts
rate_grid <- injury_daily %>%
  group_by(Day_of_Week, Hour) %>%
  summarise(mean_injury = mean(Injury_Count), .groups = "drop")
print(head(rate_grid))

ggplot(rate_grid, aes(Hour, Day_of_Week, fill = mean_injury)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_injury, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Mean Daily Injury Count by Hour × Day", x = "Hour of Day", y = "Day of Week", fill = "Mean injuries") +
  theme_minimal(base_size = 12)
