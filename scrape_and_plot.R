library(rvest)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)

########### scrape webpage

url <- "https://en.wikipedia.org/wiki/List_of_natural_disasters_by_death_toll"

tables <- url %>%
  read_html() %>%
  html_nodes("table")

table_20th <- html_table(tables[[3]], fill = TRUE)
table_21st <- html_table(tables[[4]], fill = TRUE)

df_20th <- as.data.frame(table_20th)
df_21st <- as.data.frame(table_21st)

########### convert death toll
conversion_fun <- function(x){
  x <- str_remove_all(x, "\\[.*?\\]")
  x <- str_replace_all(x, ",", "")
  x <- str_trim(x)
  
  nums <- str_extract_all(x, "\\d+(?:\\.\\d+)?")[[1]]
  
  if (length(nums) == 2) {
    a <- as.numeric(nums[1])
    b <- as.numeric(nums[2])
    return(mean(c(a, b)))
  } else {
    return(as.numeric(nums))
  }
}

conversed_death_toll_20th = rep(0, nrow(df_20th))
for (i in 1:nrow(df_20th)){
  conversed_death_toll_20th[i] = conversion_fun(df_20th$`Death tolls`[i])
}
df_20th$`Death tolls` = conversed_death_toll_20th

conversed_death_toll_21st = rep(0, nrow(df_21st))
for (i in 1:nrow(df_21st)){
  conversed_death_toll_21st[i] = conversion_fun(df_21st$`Death toll`[i])
}
df_21st$`Death toll` = conversed_death_toll_21st

########### merge and plot
colnames(df_21st) = colnames(df_20th)
df_all <- bind_rows(df_20th, df_21st)
df_all$Type <- gsub("^Floods$", "Flood", df_all$Type)
df_all$Type <- as.factor(df_all$Type)
df_bar <- df_all %>%
  group_by(Year, Type) %>%
  summarise(Total_Deaths = sum(`Death tolls`, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(df_bar, aes(x = Year, y = log10(Total_Deaths + 1), fill = Type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Death Toll by Disaster Type and Year",
    x = "Year",
    y = "Total Death Toll (log10)",
    fill = "Disaster Type"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

print(p1)
