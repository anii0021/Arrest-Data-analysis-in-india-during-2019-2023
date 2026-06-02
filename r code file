

# LOAD  LIBRARIES 

library(tidyverse)   # dplyr, ggplot2, tidyr, readr
library(readxl)      # read_excel()
library(scales)      # comma(), percent()
library(ggrepel)     # geom_label_repel()
library(patchwork)   # side-by-side plots
library(RColorBrewer)

# Set a clean global theme
theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40", size = 11),
      plot.caption  = element_text(colour = "grey50", size = 9),
      axis.title    = element_text(size = 11),
      legend.position = "bottom"
    )
)

getwd()
setwd("C:/Users/anii0/OneDrive/Desktop/Darp Project")
getwd()

# 1. DATA LOADING

raw_2019     <- read_excel("NCRB_2019_Table19A2.xlsx",  skip = 3)
raw_2020_ipc <- read_excel("NCRB_2020_IPC_Table19A2.xlsx", skip = 3)
raw_2020_sll <- read_excel("NCRB_2020_SLL_Table19A4.xlsx", skip = 3)
raw_2021     <- read_excel("NCRB_2021_Table19A2.xlsx",  skip = 3)
raw_2023     <- read_excel("NCRB_2023_Table19A4.xlsx",  skip = 3)


# 2. DATA CLEANING  — applied identically to all five datasets

clean_ncrb <- function(df, year_label) {

  # ── Step 1: Rename columns to a unified schema 
  # Adjust the position numbers (1, 2, 3 …) to match column order in your file
  names(df) <- c(
    "sl_no", "state",
    "juv_boys", "juv_girls",
    "m_18_30", "f_18_30",
    "m_30_45", "f_30_45",
    "m_45_60", "f_45_60",
    "m_60p",   "f_60p",
    "total_male", "total_female", "grand_total"
  )

  df <- df %>%

    #Step 2: Type correction —
    mutate(across(juv_boys:grand_total, ~ suppressWarnings(as.numeric(.)))) %>%

    #Step 3: Row filtering — keep only individual state rows (sl_no 1–28) ─
    # Removes "Total States", "Total UTs", "Total All India" subtotal rows
    filter(!is.na(sl_no), sl_no == as.integer(sl_no),
           as.integer(sl_no) >= 1, as.integer(sl_no) <= 28) %>%

    # ── Step 4:
    mutate(
      state = str_trim(state),
      state = case_when(
        str_detect(state, regex("jammu", ignore_case = TRUE)) ~ "Jammu & Kashmir",
        str_detect(state, regex("odisha|orissa", ignore_case = TRUE)) ~ "Odisha",
        str_detect(state, regex("uttarakhand|uttaranchal", ignore_case = TRUE)) ~ "Uttarakhand",
        TRUE ~ state
      )
    ) %>%

    # Step 5: Derived variables
    mutate(
      year          = year_label,
      juv_total     = juv_boys + juv_girls,
      t_18_30       = m_18_30  + f_18_30,
      t_30_45       = m_30_45  + f_30_45,
      t_45_60       = m_45_60  + f_45_60,
      t_60p         = m_60p    + f_60p,
      female_pct    = total_female / grand_total * 100,
      mf_ratio      = total_male / total_female,
      pct_18_30     = t_18_30  / grand_total * 100,
      pct_30_45     = t_30_45  / grand_total * 100,
      pct_45_60     = t_45_60  / grand_total * 100,
      pct_60p       = t_60p    / grand_total * 100,
      pct_juv       = juv_total / grand_total * 100
    ) %>%

    select(year, state, everything(), -sl_no)
}

df_2019     <- clean_ncrb(raw_2019,     "2019")
df_2020_ipc <- clean_ncrb(raw_2020_ipc, "2020_IPC")
df_2020_sll <- clean_ncrb(raw_2020_sll, "2020_SLL")
df_2021     <- clean_ncrb(raw_2021,     "2021")
df_2023     <- clean_ncrb(raw_2023,     "2023")

# Combined long dataset (primary years only — excludes 2020_SLL for most plots)
df_primary <- bind_rows(df_2019, df_2020_ipc, df_2021, df_2023)

# All five datasets combined
df_all <- bind_rows(df_2019, df_2020_ipc, df_2020_sll, df_2021, df_2023)

cat("✔ Data loaded and cleaned.\n")
cat("  Rows per dataset — 2019:", nrow(df_2019),
    "| 2020 IPC:", nrow(df_2020_ipc),
    "| 2020 SLL:", nrow(df_2020_sll),
    "| 2021:", nrow(df_2021),
    "| 2023:", nrow(df_2023), "\n")


# 3. NATIONAL SUMMARY TABLE


national_summary <- df_all %>%
  group_by(year) %>%
  summarise(
    total_arrests  = sum(grand_total,   na.rm = TRUE),
    total_male     = sum(total_male,    na.rm = TRUE),
    total_female   = sum(total_female,  na.rm = TRUE),
    female_pct     = total_female / total_arrests * 100,
    mf_ratio       = total_male / total_female,
    juv_arrests    = sum(juv_total,     na.rm = TRUE),
    juv_pct        = juv_arrests / total_arrests * 100,
    share_18_30    = sum(t_18_30, na.rm = TRUE) / total_arrests * 100,
    share_30_45    = sum(t_30_45, na.rm = TRUE) / total_arrests * 100,
    share_45_60    = sum(t_45_60, na.rm = TRUE) / total_arrests * 100,
    share_60p      = sum(t_60p,   na.rm = TRUE) / total_arrests * 100,
    .groups = "drop"
  )

print(national_summary)



# 4. FIGURE 1 — National Total Arrests 2019–2023 (Bar + Trend Line)


fig1_data <- national_summary %>%
  mutate(
    year_label  = year,
    arrests_lakh = total_arrests / 1e5,
    is_primary  = year != "2020_SLL"          # SLL excluded from trend line
  )

# Trend line through primary years only
trend_data <- fig1_data %>% filter(is_primary) %>%
  mutate(x_num = as.numeric(factor(year_label,
                                   levels = c("2019","2020_IPC","2021","2023"))))

trend_model <- lm(arrests_lakh ~ x_num, data = trend_data)

fig1 <- ggplot(fig1_data,
               aes(x = year_label, y = arrests_lakh,
                   fill = ifelse(year == "2020_SLL", "SLL", "Primary"))) +
  geom_col(width = 0.6, colour = "white") +
  geom_text(aes(label = paste0(round(arrests_lakh, 1), "L")),
            vjust = -0.4, size = 3.8, fontface = "bold") +
  geom_smooth(data = filter(fig1_data, is_primary),
              aes(group = 1), method = "lm", se = FALSE,
              colour = "orange", linetype = "dashed", linewidth = 1.1) +
  scale_fill_manual(values = c("Primary" = "darkblue", "SLL" = "blue"),
                    name = "Dataset type") +
  scale_y_continuous(labels = function(x) paste0(x, "L"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Figure 1: National Total Arrests in India (2019–2023)",
    subtitle = "Orange dashed line = OLS trend through primary datasets\n2020 shown twice — IPC (blue) and SLL (light blue) are separate crime categories",
    x        = "Year / Dataset",
    y        = "Arrests (Lakhs)",
    caption  = "Source: NCRB Crime in India, Table 19A(2) & 19A(4)"
  )

print(fig1)
ggsave("Figure1_National_Arrests.png", fig1, width = 10, height = 6, dpi = 180)


# 5. FIGURE 2 — Gender Gap Evolution (Dual Panel)


gender_data <- national_summary %>%
  select(year, female_pct, mf_ratio, total_male, total_female) %>%
  mutate(year = factor(year, levels = c("2019","2020_IPC","2020_SLL","2021","2023")))

# Left panel: Female % of total arrests
p2_left <- ggplot(gender_data, aes(x = year, y = female_pct, group = 1)) +
  geom_line(colour = "red", linewidth = 1.2) +
  geom_point(colour = "red", size = 3.5) +
  geom_text(aes(label = paste0(round(female_pct, 2), "%")),
            vjust = -0.8, size = 3.4, fontface = "bold", colour = "red") +
  scale_y_continuous(limits = c(4, 8), labels = function(x) paste0(x, "%")) +
  labs(title = "Female Share of Total Arrests (%)",
       x = "Year", y = "Female %")

# Right panel: Male vs Female volume with M:F ratio label
gender_long <- gender_data %>%
  select(year, total_male, total_female, mf_ratio) %>%
  pivot_longer(cols = c(total_male, total_female),
               names_to = "gender", values_to = "arrests") %>%
  mutate(gender = recode(gender, total_male = "Male", total_female = "Female"),
         arrests_lakh = arrests / 1e5)

p2_right <- ggplot(gender_long, aes(x = year, y = arrests_lakh, fill = gender)) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(data = filter(gender_long, gender == "Male"),
            aes(label = paste0("M:F\n",
                               round(filter(gender_data, year == year)$mf_ratio, 1), ":1")),
            position = position_dodge(0.65), vjust = -0.3, size = 2.8) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  scale_y_continuous(labels = function(x) paste0(x, "L"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Male vs Female Arrest Volume",
       x = "Year", y = "Arrests (Lakhs)", fill = "")

fig2 <- p2_left + p2_right +
  plot_annotation(
    title   = "Figure 2: Gender Gap in Arrests — 2019 to 2023",
    caption = "Source: NCRB Crime in India | M:F ratio = Total Male / Total Female arrests"
  )

print(fig2)
ggsave("Figure2_Gender_Gap.png", fig2, width = 13, height = 6, dpi = 180)


# 6. FIGURE 3 — Age Group Composition: 100% Stacked Bar


age_data <- df_all %>%
  group_by(year) %>%
  summarise(
    Juveniles = sum(juv_total, na.rm = TRUE),
    `18-30`   = sum(t_18_30,  na.rm = TRUE),
    `30-45`   = sum(t_30_45,  na.rm = TRUE),
    `45-60`   = sum(t_45_60,  na.rm = TRUE),
    `60+`     = sum(t_60p,    na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  pivot_longer(-year, names_to = "age_group", values_to = "arrests") %>%
  group_by(year) %>%
  mutate(pct = arrests / sum(arrests) * 100,
         age_group = factor(age_group,
                            levels = c("Juveniles","18-30","30-45","45-60","60+"))) %>%
  ungroup()

fig3 <- ggplot(age_data, aes(x = year, y = pct, fill = age_group)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(data = filter(age_data, age_group %in% c("18-30","30-45")),
            aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", colour = "white") +
  scale_fill_brewer(palette = "Set2", name = "Age Group") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title    = "Figure 3: Age Group Composition of National Arrests (2019–2023)",
    subtitle = "100% stacked bars — each bar sums to 100% of that year's arrests",
    x        = "Year / Dataset",
    y        = "Share of Total Arrests (%)",
    caption  = "Source: NCRB Crime in India | Labels shown for 18–30 and 30–45 groups only"
  )

print(fig3)
ggsave("Figure3_Age_Composition.png", fig3, width = 11, height = 6, dpi = 180)


# 7. FIGURE 4 — State-Level Heatmap (Top 10 States)


# Rank states by 2021 totals; take top 10
top10_states <- df_2021 %>%
  arrange(desc(grand_total)) %>%
  slice_head(n = 10) %>%
  pull(state)

heatmap_data <- df_primary %>%
  filter(state %in% top10_states) %>%
  mutate(
    state       = factor(state, levels = rev(top10_states)),
    arrests_k   = grand_total / 1000,
    year        = factor(year, levels = c("2019","2020_IPC","2021","2023"))
  )

fig4 <- ggplot(heatmap_data, aes(x = year, y = state, fill = arrests_k)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(arrests_k, 1), "K")),
            size = 3.2, fontface = "bold",
            colour = ifelse(heatmap_data$arrests_k > 250, "white", "black")) +
  scale_fill_gradient(low = "yellow", high = "red",
                      name = "Arrests (thousands)") +
  labs(
    title    = "Figure 4: State-Level Arrest Heatmap — Top 10 States (2019–2023)",
    subtitle = "Top 10 ranked by 2021 totals. Yellow = lower; Deep red = higher.",
    x        = "Year / Dataset",
    y        = NULL,
    caption  = "Source: NCRB Crime in India | Values in thousands (K)"
  ) +
  theme(axis.text.y = element_text(size = 11),
        legend.key.width = unit(1.8, "cm"))

print(fig4)
ggsave("Figure4_State_Heatmap.png", fig4, width = 10, height = 7, dpi = 180)


# 8. FIGURE 5 — Regression Scatter Plots


# Helper: get top-3 states by grand_total for labelling
get_top3 <- function(df) df %>% arrange(desc(grand_total)) %>% slice_head(n = 3)

make_scatter <- function(df, yr) {
  top3   <- get_top3(df)
  model  <- lm(grand_total ~ t_18_30, data = df)
  r2     <- summary(model)$r.squared
  slope  <- coef(model)[["t_18_30"]]
  intcpt <- coef(model)[["(Intercept)"]]

  ggplot(df, aes(x = t_18_30 / 1000, y = grand_total / 1000)) +
    geom_point(colour = "blue", size = 2.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "red",
                linetype = "dashed", linewidth = 1, fill = "lightpink") +
    geom_label_repel(data = top3,
                     aes(label = state),
                     size = 2.8, max.overlaps = 10,
                     box.padding = 0.3, point.padding = 0.2,
                     colour = "purple") +
    annotate("text", x = Inf, y = Inf,
             label = paste0("R² = ", round(r2, 3),
                            "\nSlope = ", round(slope, 3), "x"),
             hjust = 1.1, vjust = 1.3, size = 3.5,
             colour = "red", fontface = "bold") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    labs(title = yr,
         x = "18–30 Arrests (thousands)",
         y = "Total Arrests (thousands)")
}

p5a <- make_scatter(df_2019,     "2019")
p5b <- make_scatter(df_2020_ipc, "2020 (IPC)")
p5c <- make_scatter(df_2021,     "2021")
p5d <- make_scatter(df_2023,     "2023")

fig5 <- (p5a | p5b) / (p5c | p5d) +
  plot_annotation(
    title    = "Figure 5: Regression — 18–30 Age Group Predicts Total Arrests",
    subtitle = "Simple OLS per year. Each dot = one state. Dashed line = fitted regression.",
    caption  = "Source: NCRB Crime in India | Top-3 states labelled per panel"
  )

print(fig5)
ggsave("Figure5_Regression_Scatter.png", fig5, width = 13, height = 10, dpi = 180)

# 9. FIGURE 6 — R² and Slope Trend Across Years


reg_results <- map_dfr(
  list("2019" = df_2019, "2020_IPC" = df_2020_ipc,
       "2021" = df_2021, "2023" = df_2023),
  function(df) {
    m <- lm(grand_total ~ t_18_30, data = df)
    tibble(
      r2    = summary(m)$r.squared,
      slope = coef(m)[["t_18_30"]],
      intcp = coef(m)[["(Intercept)"]]
    )
  },
  .id = "year"
) %>%
  mutate(year = factor(year, levels = c("2019","2020_IPC","2021","2023")))

# Print regression table
cat("\n Regression Summary\n")
print(reg_results %>%
        mutate(equation = paste0("Total = ", round(intcp), " + ",
                                 round(slope, 3), " × (18–30)")))

p6_r2 <- ggplot(reg_results, aes(x = year, y = r2, group = 1)) +
  geom_hline(yintercept = mean(reg_results$r2),
             colour = "orange", linetype = "dashed", linewidth = 1) +
  geom_line(colour = "blue", linewidth = 1.2) +
  geom_point(colour = "blue", size = 4) +
  geom_text(aes(label = round(r2, 3)), vjust = -0.8, size = 3.5, fontface = "bold") +
  scale_y_continuous(limits = c(0.90, 1.00)) +
  labs(title = "R² Across Years (Model Strength)",
       x = "Year", y = "R²",
       subtitle = "Orange dashed = mean R²")

p6_slope <- ggplot(reg_results, aes(x = year, y = slope, group = 1)) +
  geom_hline(yintercept = mean(reg_results$slope),
             colour = "orange", linetype = "dashed", linewidth = 1) +
  geom_line(colour = "red", linewidth = 1.2) +
  geom_point(colour = "red", size = 4) +
  geom_text(aes(label = round(slope, 3)), vjust = -0.8, size = 3.5, fontface = "bold") +
  scale_y_continuous(limits = c(1.9, 2.9)) +
  labs(title = "Regression Slope Across Years (Multiplier Effect)",
       x = "Year", y = "Slope (× per 18–30 arrest)",
       subtitle = "Orange dashed = mean slope")

fig6 <- p6_r2 + p6_slope +
  plot_annotation(
    title   = "Figure 6: Regression Model Strength and Slope Over Time",
    caption = "Source: NCRB Crime in India | OLS: Grand_Total ~ T18_30"
  )

print(fig6)
ggsave("Figure6_Regression_Trend.png", fig6, width = 12, height = 6, dpi = 180)



# 10. FIGURE 7 — Regional Comparison (Grouped Bar + 100% Stacked)


# State-to-region mapping
region_map <- c(
  # North
  "Uttar Pradesh"       = "North",
  "Rajasthan"           = "North",
  "Haryana"             = "North",
  "Punjab"              = "North",
  "Himachal Pradesh"    = "North",
  "Uttarakhand"         = "North",
  "Jammu & Kashmir"     = "North",
  "Delhi"               = "North",

  # South
  "Tamil Nadu"          = "South",
  "Karnataka"           = "South",
  "Andhra Pradesh"      = "South",
  "Telangana"           = "South",
  "Kerala"              = "South",

  # East
  "Bihar"               = "East",
  "Jharkhand"           = "East",
  "West Bengal"         = "East",
  "Odisha"              = "East",
  "Chhattisgarh"        = "East",

  # West & Central
  "Maharashtra"         = "West & Central",
  "Gujarat"             = "West & Central",
  "Madhya Pradesh"      = "West & Central",
  "Goa"                 = "West & Central"
)

regional_data <- df_primary %>%
  mutate(region = region_map[state]) %>%
  filter(!is.na(region)) %>%
  group_by(year, region) %>%
  summarise(arrests_lakh = sum(grand_total, na.rm = TRUE) / 1e5,
            .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = arrests_lakh / sum(arrests_lakh) * 100) %>%
  ungroup() %>%
  mutate(
    year   = factor(year, levels = c("2019","2020_IPC","2021","2023")),
    region = factor(region, levels = c("North","South","East","West & Central"))
  )

region_colours <- c(
  "North"          = "#2E5FAC",
  "South"          = "#27AE60",
  "East"           = "#E07B00",
  "West & Central" = "#C00000"
)

p7_left <- ggplot(regional_data,
                  aes(x = year, y = arrests_lakh, fill = region)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(arrests_lakh, 1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3) +
  scale_fill_manual(values = region_colours, name = "Region") +
  scale_y_continuous(labels = function(x) paste0(x, "L"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Regional Arrest Totals (Lakhs)",
       x = "Year", y = "Arrests (Lakhs)")

p7_right <- ggplot(regional_data,
                   aes(x = year, y = share, fill = region)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(aes(label = paste0(round(share, 0), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, colour = "white", fontface = "bold") +
  scale_fill_manual(values = region_colours, name = "Region") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Regional Share of National Arrests (%)",
       x = "Year", y = "Share (%)")

fig7 <- p7_left + p7_right +
  plot_annotation(
    title    = "Figure 7: Regional Crime Arrest Comparison (2019–2023)",
    subtitle = "Left: Absolute volume (Lakhs). Right: Share of national total.",
    caption  = "Source: NCRB Crime in India | North India consistently dominates"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(fig7)
ggsave("Figure7_Regional_Comparison.png", fig7, width = 14, height = 7, dpi = 180)


# 11. GENDER ARREST RATES PER 1,000 POPULATION


# India 2021 Census projected population (millions)
pop_male   <- 714e6
pop_female <- 666e6

gender_rate_table <- national_summary %>%
  filter(year %in% c("2019","2020_IPC","2021","2023")) %>%
  mutate(
    male_rate_per1000   = total_male   / pop_male   * 1000,
    female_rate_per1000 = total_female / pop_female * 1000
  ) %>%
  select(year, total_male, male_rate_per1000,
         total_female, female_rate_per1000, mf_ratio)

cat("\nGender Arrest Rates per 1,000 Population \n")
print(gender_rate_table)



# 12. RESIDUAL DIAGNOSTICS FOR REGRESSION MODELS


cat("\n Residual Diagnostics \n")

check_residuals <- function(df, yr) {
  m   <- lm(grand_total ~ t_18_30, data = df)
  res <- tibble(state = df$state,
                fitted = fitted(m),
                resid  = residuals(m))

  cat(sprintf("\n%s | Max |resid|: %s (%s)\n",
              yr,
              comma(round(max(abs(res$resid)))),
              res$state[which.max(abs(res$resid))]))

  # Breusch–Pagan-style: check if |resid| correlates with fitted values
  bp <- cor(res$fitted, abs(res$resid))
  cat(sprintf("  Cor(fitted, |resid|) = %.3f  [near 0 = homoscedastic]\n", bp))
}

check_residuals(df_2019,     "2019")
check_residuals(df_2020_ipc, "2020 IPC")
check_residuals(df_2021,     "2021")
check_residuals(df_2023,     "2023")



# 13. EXPORT ALL CLEANED DATA TO CSV

write_csv(df_2019,     "cleaned_2019.csv")
write_csv(df_2020_ipc, "cleaned_2020_IPC.csv")
write_csv(df_2020_sll, "cleaned_2020_SLL.csv")
write_csv(df_2021,     "cleaned_2021.csv")
write_csv(df_2023,     "cleaned_2023.csv")
write_csv(df_primary,  "cleaned_primary_combined.csv")
write_csv(national_summary, "national_summary_table.csv")
write_csv(reg_results,      "regression_results.csv")
write_csv(gender_rate_table,"gender_rate_table.csv")
