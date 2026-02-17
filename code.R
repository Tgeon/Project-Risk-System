library(tidyverse)
library(lubridate)
library(janitor)
library(igraph)
library(broom)
library(readxl)

#Primary excel and csv setup for analysis
RAW_DIR <- "data/raw"
CLEAN_DIR <- "data/clean"
FIG_DIR <- file.path("data", "clean", "figures")

TASKS_CSV <- file.path(RAW_DIR, "Construction_Data_PM_Tasks_All_Projects.csv")
FORMS_CSV <- file.path(RAW_DIR, "Construction_Data_PM_Forms_All_Projects.csv")

tasks_raw <- readr::read_csv(file.path(RAW_DIR, "Construction_Data_PM_Tasks_All_Projects.csv"),
                             show_col_types = FALSE) %>% clean_names()
forms_raw <- readr::read_csv(file.path(RAW_DIR, "Construction_Data_PM_Forms_All_Projects.csv"),
                             show_col_types = FALSE) %>% clean_names()

tasks_raw
forms_raw

#Cleaning
parse_dmy_safe <- function(x) suppressWarnings(dmy(x))

tasks <- tasks_raw %>%
  mutate(
    project_id = as.character(project),
    created_dt = parse_dmy_safe(created),
    status_changed_dt = parse_dmy_safe(status_changed),
    cycle_time_days = as.numeric(difftime(status_changed_dt, created_dt, units = "days")),
    is_open = str_detect(tolower(status), "open"),
    is_closed = str_detect(tolower(status), "closed")
  ) %>%
  filter(!is.na(project_id), !is.na(created_dt))

forms <- forms_raw %>%
  mutate(
    project_id = as.character(project),
    created_dt = parse_dmy_safe(created),
    status_changed_dt = parse_dmy_safe(status_changed),
    cycle_time_days = as.numeric(difftime(status_changed_dt, created_dt, units = "days")),
    is_open = str_detect(tolower(status), "open"),
    is_closed = str_detect(tolower(status), "closed")
  ) %>%
  filter(!is.na(project_id), !is.na(created_dt))

write_csv(tasks, file.path(CLEAN_DIR, "tasks_clean.csv"))
write_csv(forms, file.path(CLEAN_DIR, "forms_clean.csv"))

#QA Checks
tasks %>% summarise(
  n = n(),
  pct_cycle_missing = mean(is.na(cycle_time_days)),
  pct_negative_cycle = mean(!is.na(cycle_time_days) & cycle_time_days < 0)
)

forms %>% summarise(
  n = n(),
  pct_cycle_missing = mean(is.na(cycle_time_days)),
  pct_negative_cycle = mean(!is.na(cycle_time_days) & cycle_time_days < 0)
)

#For negative numbers
tasks <- tasks %>% mutate(cycle_time_days = if_else(cycle_time_days < 0, NA_real_, cycle_time_days))
forms <- forms %>% mutate(cycle_time_days = if_else(cycle_time_days < 0, NA_real_, cycle_time_days))

#Risk KPI
task_kpis <- tasks %>%
  group_by(project_id) %>%
  summarise(
    task_total = n(),
    task_open = sum(is_open, na.rm = TRUE),
    task_closed = sum(is_closed, na.rm = TRUE),
    task_overdue = sum(over_due %in% TRUE, na.rm = TRUE),  # if exists
    task_cycle_p50 = median(cycle_time_days, na.rm = TRUE),
    task_cycle_p80 = quantile(cycle_time_days, 0.80, na.rm = TRUE),
    .groups = "drop"
  )

form_kpis <- forms %>%
  group_by(project_id) %>%
  summarise(
    form_total = n(),
    form_open = sum(is_open, na.rm = TRUE),
    form_closed = sum(is_closed, na.rm = TRUE),
    form_overdue = sum(over_due %in% TRUE, na.rm = TRUE),
    form_cycle_p50 = median(cycle_time_days, na.rm = TRUE),
    form_cycle_p80 = quantile(cycle_time_days, 0.80, na.rm = TRUE),
    avg_open_actions = mean(open_actions, na.rm = TRUE),
    avg_total_actions = mean(total_actions, na.rm = TRUE),
    .groups = "drop"
  )

safe_scale <- function(x) {
  x <- tidyr::replace_na(x, 0)
  if (length(unique(x)) <= 1) return(rep(0, length(x)))
  as.numeric(scale(x))
}

project_kpis <- project_kpis %>%
  mutate(
    raw_score =
      safe_scale(task_open) +
      safe_scale(form_open) +
      0.5 * safe_scale(task_cycle_p80) +
      0.5 * safe_scale(form_cycle_p80) +
      0.5 * safe_scale(avg_open_actions)
  ) %>%
  mutate(
    risk_score = scales::rescale(raw_score, to = c(0, 100))
  )

write_csv(project_kpis, file.path(CLEAN_DIR, "project_kpis.csv"))

#Time series and backlogs
tasks_ts <- tasks %>%
  mutate(week = floor_date(created_dt, "week")) %>%
  group_by(project_id, week) %>%
  summarise(tasks_created = n(), .groups = "drop")

tasks_closed_ts <- tasks %>%
  filter(!is.na(status_changed_dt), is_closed) %>%
  mutate(week = floor_date(status_changed_dt, "week")) %>%
  group_by(project_id, week) %>%
  summarise(tasks_closed = n(), .groups = "drop")

tasks_flow <- full_join(tasks_ts, tasks_closed_ts, by = c("project_id","week")) %>%
  replace_na(list(tasks_created = 0, tasks_closed = 0)) %>%
  arrange(project_id, week) %>%
  group_by(project_id) %>%
  mutate(backlog = cumsum(tasks_created - tasks_closed)) %>%
  ungroup()

write_csv(tasks_flow, file.path(CLEAN_DIR, "tasks_flow_weekly.csv"))

#Monte Carlo Forecast

#historical weekly throughput
throughput <- tasks_flow %>%
  group_by(project_id) %>%
  summarise(
    mean_closed = mean(tasks_closed, na.rm = TRUE),
    sd_closed = sd(tasks_closed, na.rm = TRUE),
    current_backlog = last(backlog),
    .groups = "drop"
  ) %>%
  mutate(
    sd_closed = if_else(is.na(sd_closed) | sd_closed == 0, 1, sd_closed),
    current_backlog = pmax(current_backlog, 0)
  )

#Simulation
N_SIMS <- 10000
set.seed(4206)

simulate_clear_weeks <- function(backlog, mu, sigma, n_sims = 10000) {
  weeks <- numeric(n_sims)
  for (i in seq_len(n_sims)) {
    remaining <- backlog
    w <- 0
    while (remaining > 0 && w < 520) { # cap at 10 years
      closed <- round(rnorm(1, mean = mu, sd = sigma))
      closed <- max(closed, 0)
      remaining <- remaining - closed
      w <- w + 1
      if (mu <= 0) { w <- 520; break }
    }
    weeks[i] <- w
  }
  weeks
}

clear_sims <- throughput %>%
  mutate(sim_weeks = pmap(list(current_backlog, mean_closed, sd_closed),
                          ~ simulate_clear_weeks(..1, ..2, ..3, n_sims = N_SIMS)))

#Summarize clearance time
clear_summary <- clear_sims %>%
  transmute(
    project_id,
    backlog = current_backlog,
    mu_closed = mean_closed,
    p50_weeks = map_dbl(sim_weeks, ~ quantile(.x, 0.50, na.rm = TRUE)),
    p80_weeks = map_dbl(sim_weeks, ~ quantile(.x, 0.80, na.rm = TRUE)),
    p90_weeks = map_dbl(sim_weeks, ~ quantile(.x, 0.90, na.rm = TRUE))
  )

write_csv(clear_summary, file.path(CLEAN_DIR, "backlog_clearance_summary.csv"))

#Driver Analysis - causes of risk
tasks_model <- tasks %>%
  filter(!is.na(cycle_time_days), cycle_time_days >= 0) %>%
  mutate(log_cycle = log1p(cycle_time_days))

m_task <- lm(log_cycle ~ priority + cause + task_group + type, data = tasks_model)
summary(m_task)
write_csv(broom::tidy(m_task), file.path(CLEAN_DIR, "task_cycle_drivers.csv"))

#Project Model
project_model <- project_kpis %>%
  left_join(clear_summary, by = "project_id") %>%
  filter(!is.na(p80_weeks))

m_proj <- lm(p80_weeks ~ task_open + task_cycle_p80 + form_open + form_cycle_p80 + form_overdue, data = project_model)
summary(m_proj)
write_csv(broom::tidy(m_proj), file.path(CLEAN_DIR, "project_clearance_drivers.csv"))

#Data Visualizations
TOP_PROJECT <- project_kpis %>%
  filter(!is.na(risk_score), is.finite(risk_score)) %>%
  arrange(desc(risk_score)) %>%
  slice(1) %>%
  pull(project_id) %>%
  as.character()

#Risk Ranking
p_risk <- project_kpis %>%
  filter(!is.na(risk_score), !is.nan(risk_score), is.finite(risk_score)) %>%
  mutate(project_id = as.character(project_id)) %>%
  arrange(desc(risk_score)) %>%
  ggplot(aes(x = reorder(project_id, risk_score),
             y = risk_score,
             fill = risk_score)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_gradient(low = "#2ECC71", high = "#E74C3C") +
  labs(
    title = "Project Execution Risk Index",
    subtitle = "Composite score based on backlog, P80 cycle time, and workflow friction",
    x = "Project",
    y = "Risk Index (0–100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#Cause/Drivers of Risk 
top_proj <- TOP_PROJECT
driver_vars <- c("task_open", "form_open", "task_cycle_p80", "form_cycle_p80", "avg_open_actions")

p_drivers_top <- project_kpis %>%
  filter(project_id == top_proj) %>%
  select(all_of(driver_vars)) %>%
  pivot_longer(cols = everything(), names_to = "driver", values_to = "value") %>%
  mutate(driver = recode(driver,
                         task_open = "Open tasks",
                         form_open = "Open forms",
                         task_cycle_p80 = "Task cycle time (P80)",
                         form_cycle_p80 = "Form cycle time (P80)",
                         avg_open_actions = "Avg open actions")) %>%
  ggplot(aes(x = reorder(driver, value), y = value)) +
  geom_col(fill = "#2C3E50", width = 0.7) +
  coord_flip() +
  labs(
    title = paste("Top Project Risk Drivers — Project", top_proj),
    subtitle = "Raw metric levels behind the composite risk score",
    x = "",
    y = "Metric value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


#Prediction Overlay
p_risk_clear <- project_kpis %>%
  left_join(clear_summary %>% mutate(project_id = as.character(project_id)),
            by = "project_id") %>%
  filter(!is.na(p80_weeks), is.finite(p80_weeks),
         !is.na(risk_score), is.finite(risk_score)) %>%
  mutate(
    project_id = as.character(project_id),
    label_y = p80_weeks + 0.8  # vertical offset for labels
  ) %>%
  ggplot(aes(x = risk_score, y = p80_weeks)) +
  geom_point(size = 3, color = "#2C3E50",
             position = position_jitter(width = 1.5, height = 0)) +
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", linewidth = 1) +
  geom_text(
    aes(y = label_y, label = project_id),
    size = 4,
    vjust = 0,
    check_overlap = TRUE
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "Risk Index vs Clearance Time (P80)",
    subtitle = "Each point represents a project (labels shown where space allows)",
    x = "Risk Index (0–100)",
    y = "Weeks to Clear Backlog (P80)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#Backlog trend for one project
show_proj <- TOP_PROJECT

p_backlog <- tasks_flow %>%
  filter(project_id == show_proj) %>%
  mutate(net_change = tasks_created - tasks_closed) %>%
  ggplot(aes(x = week)) +
  geom_line(aes(y = backlog), linewidth = 1.1) +
  geom_line(aes(y = tasks_created), linetype = "dashed", alpha = 0.5) +
  geom_line(aes(y = tasks_closed),  linetype = "dotted", alpha = 0.5) +
  geom_col(aes(y = net_change), alpha = 0.25) +
  labs(
    title = paste("Backlog Trend and Weekly Flow:", show_proj),
    subtitle = "Line = backlog; bars = weekly net change (created − closed); dashed/dotted = created/closed volume",
    x = "Week",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#Clearance forecast dist for one project
one <- clear_sims %>%
  mutate(project_id = as.character(project_id)) %>%
  filter(project_id == TOP_PROJECT) %>%
  pull(sim_weeks) %>%
  .[[1]]

dist_df <- tibble(weeks = one) %>% filter(is.finite(weeks))

p50 <- as.numeric(quantile(dist_df$weeks, 0.50, na.rm = TRUE))
p80 <- as.numeric(quantile(dist_df$weeks, 0.80, na.rm = TRUE))
p90 <- as.numeric(quantile(dist_df$weeks, 0.90, na.rm = TRUE))
mu  <- mean(dist_df$weeks, na.rm = TRUE)
binw <- max(1, round((max(dist_df$weeks) - min(dist_df$weeks)) / 30))

p_clear_hist <- dist_df %>%
  ggplot(aes(x = weeks)) +
  geom_histogram(binwidth = binw, fill = "grey35", color = "white", alpha = 0.95) +
  geom_histogram(
    data = dist_df %>% filter(weeks >= p80),
    aes(x = weeks),
    binwidth = binw,
    fill = "#E74C3C",
    color = "white",
    alpha = 0.85
  ) +
  geom_vline(xintercept = mu,  linewidth = 1.1, linetype = "solid",  color = "#2C3E50") +
  geom_vline(xintercept = p50, linewidth = 1.1, linetype = "dashed", color = "#2C3E50") +
  geom_vline(xintercept = p80, linewidth = 1.2, linetype = "solid",  color = "#E74C3C") +
  geom_vline(xintercept = p90, linewidth = 1.1, linetype = "dotted", color = "#E74C3C") +
  annotate(
    "text",
    x = Inf, y = Inf,
    hjust = 1.05, vjust = 1.2,
    label = paste0(
      "Project: ", top_proj, "\n",
      "Mean: ", round(mu, 1), " wks\n",
      "P50:  ", round(p50, 1), " wks\n",
      "P80:  ", round(p80, 1), " wks\n",
      "P90:  ", round(p90, 1), " wks"
    ),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    title = paste("Monte Carlo Forecast: Weeks to Clear Backlog — Project", top_proj),
    subtitle = "Red region shows the risk tail (≥ P80). Use P80 as a conservative planning target.",
    x = "Weeks to Clear Backlog",
    y = "Simulation Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#Saving Visualizations
ggsave(
  filename = file.path(FIG_DIR, "01_risk_ranking.png"),
  plot = p_risk,
  width = 9, height = 5, dpi = 300
)

ggsave(
  filename = file.path(FIG_DIR, "02_top_risk_drivers.png"),
  plot = p_drivers_top,
  width = 9, height = 5, dpi = 300
)

ggsave(
  filename = file.path(FIG_DIR, "03_risk_vs_clearance.png"),
  plot = p_risk_clear,
  width = 9, height = 5, dpi = 300
)

ggsave(
  filename = file.path(FIG_DIR, "04_backlog_trend.png"),
  plot = p_backlog,
  width = 9, height = 5, dpi = 300
)

ggsave(
  filename = file.path(FIG_DIR, "05_monte_carlo_clearance.png"),
  plot = p_clear_hist,
  width = 9, height = 5, dpi = 300
)

pdf(file.path(FIG_DIR, "00_Project_Risk_Dashboard.pdf"), width = 9, height = 5)
print(p_risk)
print(p_drivers_top)
print(p_risk_clear)
print(p_backlog)
print(p_clear_hist)
dev.off()