library(toolboxH)
library(ggplot2)
library(ggthemes)
library(here)
files <- c(
    "data/SUPPLEMENT_census/2019-12-31.csv",
    "data/SUPPLEMENT_census/2020-12-31.csv",
    "data/SUPPLEMENT_census/2021-12-31.csv",
    "data/SUPPLEMENT_census/2022-12-31.csv",
    "data/SUPPLEMENT_census/2023-12-31.csv"
  )


# Read and combine all files
census_data <- rbindlist(lapply(files, function(f) {
  dt <- fread(here(f), sep = ";", header = FALSE)
  setnames(dt, c("age", "population"))
  dt[, year := as.integer(substr(basename(f), 1, 4))]
  return(dt)
}))

# Create age groups
census_data[, age_group := fcase(
  age <= 14, "A00-A14",
  age <= 34, "A15-A34", 
  age <= 59, "A35-A59",
  age <= 79, "A60-A79",
  age >= 80, "A80+",
  default = "Unknown"
)]

# Aggregate by age groups and year
age_summary <- census_data[, .(total_pop = sum(population)), by = .(year, age_group)]
age_summary[, all := sum(total_pop), by = year]


age_order1 <- c("A80+", "A60-A79", "A35-A59", "A15-A34", "A00-A14") %>% rev()
  
# Barplot by year and age group
p1 <- ggplot(age_summary, aes(x = factor(year), y = total_pop, 
                              fill = factor(age_group, levels = age_order1))) +
  geom_col(position = "dodge") +
  # scale_fill_manual(values = c("A00-A14" = "#d62728", "A15-A34" = "#1f77b4", 
                               # "A35-A59" = "#ff7f0e", "A60-A79" = "#2ca02c", "A80+" = "#ff7f0e")) +
  labs(
    # title = "Population by Age Group Across Years",
       x = "", 
       y = "Population", fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous( breaks = pretty_breaks(10))+
  theme_minimal(base_size = 16) +
  scale_fill_tableau(direction = -1)+
  theme(axis.text.x =  element_text(angle  = 45, hjust = 1))

print(p1)

# Consistency analysis - coefficient of variation
consistency <- age_summary[, .(
  mean_pop = mean(total_pop),
  sd_pop = sd(total_pop),
  cv = sd(total_pop) / mean(total_pop)
), by = age_group]


# Proportional view
age_summary[, proportion := total_pop / all]

age_order <- c("A80+", "A60-A79", "A35-A59", "A15-A34", "A00-A14")
p3 <- ggplot(age_summary, aes(x = factor(year), y = proportion, 
                              fill = factor(age_group, levels = age_order))) +
  geom_col(position = "stack") +
 
  scale_fill_manual(values = c("A00-A14" = "#d62728", "A15-A34" = "#1f77b4", 
                               "A35-A59" = "#ff7f0e", "A60-A79" = "#2ca02c", "A80+" = "#ff7f0e")) +
  labs(
    # title = "Age Group Proportions Across Years",
       x = "",
       y = "Proportion", fill = "Age Group") +
  theme_minimal(base_size = 16) +
  scale_y_continuous(labels = label_percent(accuracy = 1), breaks = pretty_breaks(10))+
  scale_fill_tableau() +
  theme(axis.text.x = element_text(angle  = 45, hjust = 1))

print(p3)


require(patchwork)
p_both = (p1 + guides(fill = "none")) + p3 + plot_layout(width = c (2,1))
ggsave(  here("results/SUPPLEMENT_census.pdf"), p_both, width = 9,height = 6)
ggsave(  here("results/SUPPLEMENT_census.png"), p_both, width = 9,height = 6)


sessionInfo()
