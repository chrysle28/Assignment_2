##*************************
## BINF 6210
##
## Assignment 1
##
## Sharon Tsukernik
##
## Monday Oct 13, 2025

# ---- Packages used -----

library(tidyverse)
library(ggplot2)
library(vegan)
library(styler)

# ---- Loading Data ----

df_phoronida <- read_tsv("../data/Phoronida.tsv")

# ---- Exploring Data ----

view(df_phoronida)
class(df_phoronida)
names(df_phoronida)

# Filtering out which columns I need
df_phoronida2 <- df_phoronida[, c(1, 8, 15:19, 21:22, 25:28, 31, 33, 41, 48, 50:52, 55)]
summary(df_phoronida2)
names(df_phoronida2)

# Filtering out by processid and bin_uri
df_test <- df_phoronida2[, c("processid", "bin_uri")]
df_test
dim(df_test)
# 293 rows, 2 columns

# Filtering out by species and bin_uri
df_test2 <- df_phoronida2[, c("species", "bin_uri")]
df_test2

# Renaming the countries/ocean column to country
#df_phoronida2 <- df_phoronida2 %>%
 # rename(country = "country/ocean")

#EDIT: changing this step to filtering out oceans, unrecoverable, unspecified, N/A if not needed for downstream analysis
df_phoronida2$`country/ocean`
df_phoronida2 <- df_phoronida2 %>%
  filter(!is.na(`country/ocean`),
         `country/ocean` != "Unspecified country",
         `country/ocean` != "Unrecoverable",
         !grepl("ocean", `country/ocean`, ignore.case = T))
table(df_phoronida2$"country/ocean")

#EDIT: now if desired, can rename countries/ocean column to country
df_phoronida2 <- df_phoronida2 %>%
  rename(country = "country/ocean")
table(df_phoronida2$country)

# Looking at BIN column
df_phoronida2$bin_uri

# Looking at country column
df_phoronida2$country

# Counting how many samples taken from each country
df_phoronida2 %>%
  group_by(country) %>%
  unique()

# kept the NA/unspecified countries/unrecoverable countries because my main comparison is between 2 specific countries, so those unknown samples don't impact the final result, and instead I'm able to see how many total samples were taken worldwide

# Checking to see how many samples were taken from Panama and Canada
sum(df_phoronida2$country == "Panama", na.rm = TRUE)
# Answer: 140
sum(df_phoronida2$country == "Canada", na.rm = TRUE)
# Answer: 15

# Sampling bias present as there are significanlty more samples taken from Panama in comparison to Canada

## ---- Exploring Species and BIN Diversity ----

# Finding unique countries
df_coun <- unique(df_phoronida2$country)
df_coun

# Finding unique BINs
df_bin <- unique(df_phoronida2$bin_uri)
df_bin

# Finding how many unique taxonomic species are in the dataset
df_spec <- unique(df_phoronida2$species)
df_spec
length(df_spec)
# Answer: 17

# how many BINs are in the sample (taxon richness of this dataset as defined by BINS)
length(df_bin)
# Answer: 37 unique BINs in this sample

# What is the ratio of BINs to species?
length(df_bin) / length(df_spec)
# Answer: 2.17 - suggest that there are more distinctive clusters or mitochondrial lineages than named species in this dataset

# How many specimen records bear a BIN identifier?
sum(!is.na(df_phoronida2$bin_uri))
# Answer: 235

# Filtering for Panama and Canada
df_PC <- df_phoronida2[df_phoronida2$country %in% c("Panama", "Canada"), ]
df_PC

# Finding unique bins in Panama and Canada
df_PC %>%
  filter(!is.na(bin_uri)) %>%
  group_by(country) %>%
  summarise(unique_BINs = n_distinct(bin_uri))
# 6 in Canada, 9 in Panama
#EDIT: previous code did not remove rows where bin_uri was missing, so N/A was counted as a unique BIN

# ---- Plotting Graphs ----
## ---- Bar Plots ----
#EDIT: since this is a global comparison, would be nice to know the average number of samples taken from each country
mean_samples <- mean(table(df_phoronida2$country))

#EDIT: made a horizontal bar graph as there were many data columns, ordered it from largest to smallest, and added line representing avereage number of samples
# Bar plot to compare how many samples were taken from each country
df_phoronida2 %>%
  count(country) %>%
  mutate(country = fct_reorder(country, n)) %>%
  ggplot(aes(x = n, y = country, fill = country)) +
  geom_col() +
  labs(
    title = "Count of Phoronida Samples in Different Countries",
    x = "Number of samples",
    y = "Country"
  ) +
  geom_vline(xintercept = mean_samples,
             color = "black",
             linewidth = 0.7,
             linetype = "dashed") +
  annotate(
    "text",
    x = 80,
    y = 10,
    label = paste0("Average number of samples = ", round(mean_samples, 1))
  ) +
  geom_curve(aes(x = 80, y = 9.5, xend = mean_samples, yend = 7),
                 curvature = -0.3,
                 size = 0.2,
                 arrow = arrow(length = unit(0.05, "npc"), type = "open")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# Bar plot to compare how many samples were taken from Panama and Canada
PC_samples <- df_phoronida2 %>%
  filter(country %in% c("Panama", "Canada")) %>%
  count(country) %>%
  ggplot(aes(x = country, y = n, fill = country)) +
  geom_col() +
  scale_fill_manual(values = c("Panama" = "darkorange", "Canada" = "steelblue")) +
  labs(
    title = "Phoronida Samples from Panama and Canada",
    x = "Country",
    y = "Number of Samples"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

PC_samples

ggsave("../figs/boxplot_PC_samples.png", plot = PC_samples, width = 6, height = 4, dpi = 300)


# Bar plot to compare BIN richness between Panama and Canada
PC_bins <- df_phoronida2 %>%
  filter(country %in% c("Panama", "Canada")) %>%
  group_by(country) %>%
  summarise(unique_BINs = n_distinct(bin_uri)) %>%
  ggplot(aes(x = country, y = unique_BINs, fill = country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Panama" = "darkorange", "Canada" = "steelblue")) +
  labs(
    title = "Unique Phoronida BINs: Canada vs Panama",
    x = "Country",
    y = "Number of Unique BINs (Alpha Richness)"
  ) +
  theme(legend.position = "none")

PC_bins

ggsave("../figs/boxplot_PC_bins.png", plot = PC_bins, width = 6, height = 4, dpi = 300)


## ---- Accumulation Curve ----

# Subset of Panama
df_panama <- subset(df_phoronida2, country == "Panama")

#EDIT: new function create_mat to streamline formatting of other datasets for accumulation curve creation (e.g. for different countries, taxa, subsets, etc)
create_mat <- function(df) {
  df_x <- df %>%
    filter(!is.na(bin_uri)) %>%
    distinct(processid, bin_uri) %>%
    mutate(presence = as.integer(1)) %>%
    pivot_wider(
      names_from = bin_uri,
      values_from = presence,
      values_fill = list(presence = 0)
    ) %>% as.data.frame() 
  rownames(df_x) <- make.unique(df_x$processid)
  df_x <- df_x %>% select(-processid)
  return(df_x)
}
mat_phoronida2 <- create_mat(df_phoronida2) #EDIT: example usage of create_matrix function

# Create matrix
mat_panama <- df_panama %>%
  distinct(processid, bin_uri) %>%
  mutate(presence = as.integer(1)) %>%
  pivot_wider(
    names_from = bin_uri,
    values_from = presence,
    values_fill = list(presence = 0)
  )

# Convert tibble into dataframe
mat_panama <- mat_panama %>% as.data.frame()

rownames(mat_panama) <- mat_panama$processid
mat_panama <- mat_panama %>% select(-processid)

# Subset of Canada
df_canada <- subset(df_phoronida2, country == "Canada")

# Create matrix
mat_canada <- df_canada %>%
  distinct(processid, bin_uri) %>%
  mutate(presence = as.integer(1)) %>%
  pivot_wider(
    names_from = bin_uri,
    values_from = presence,
    values_fill = list(presence = 0)
  )

# Convert tibble into dataframe
mat_canada <- mat_canada %>% as.data.frame()

rownames(mat_canada) <- mat_canada$processid
mat_canada <- mat_canada %>% select(-processid)

# Calculate accumulation curve
accum_panama <- specaccum(mat_panama, method = "random")
accum_canada <- specaccum(mat_canada, method = "random")

# Plotting the accumulation curves together

png("../figs/accumulation_curve.png", width = 3600, height = 2400, res = 300)

plot(accum_panama,
  col = "darkorange", lwd = 2,
  xlab = "Number of Samples", ylab = "Unique BINs",
  main = "BIN Accumulation: Panama vs Canada"
)
plot(accum_canada, add = TRUE, col = "steelblue", lwd = 2)
legend("bottomright",
  legend = c("Panama", "Canada"),
  col = c("darkorange", "steelblue"), lwd = 2
)

dev.off()


## ---- Running a Statistical Test ----

# Wilxocon test

# Filter data and remove missing values
df_coordp <- df_phoronida2 %>%
  filter(country == "Panama", !is.na(coord))

df_coordc <- df_phoronida2 %>%
  filter(country == "Canada", !is.na(coord))

# Clean and separate coordinate data
clean_coords <- function(df_phoronida2, coord_col = "coord") {
  df_phoronida2 %>%
    mutate(
      coord = str_trim(.data[[coord_col]]),
      coord = gsub("\\[|\\]", "", coord),
      coord = gsub(" ", "", coord)
    ) %>%
    separate(coord, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
    filter(!is.na(latitude) & !is.na(longitude))
}

df_coordp <- clean_coords(df_coordp, "coord")

df_coordc <- clean_coords(df_coordc, "coord")

# Inspect cleaned data
str(df_coordp)
str(df_coordc)

# Perform the wilxocon tests for latitude and longitude
wilcox_latitude <- wilcox.test(df_coordp$latitude, df_coordc$latitude)
wilcox_longitude <- wilcox.test(df_coordp$longitude, df_coordc$longitude)

wilcox_latitude
capture.output(wilcox_latitude, file = "../output/wilcox_test_lat.txt")
# Wilcoxon rank sum test with continuity correction

# data:  coord_panama$latitude and coord_canada$latitude
# W = 0, p-value = 2.808e-12
# alternative hypothesis: true location shift is not equal to 0


# small p-value means the difference is highly statistically significant
# interpretation: latitudes of samples from Panama and Canada differ significantly - which makes sense since Panama in near the equator (low latitudes) and Canada is much farther north (high latitudes)
wilcox_longitude
capture.output(wilcox_longitude, file = "../output/wilcox_test_lon.txt")
# Wilcoxon rank sum test with continuity correction

# data:  coord_panama$longitude and coord_canada$longitude
# W = 2100, p-value = 2.808e-12
# alternative hypothesis: true location shift is not equal to 0

# small p-value, so the difference in longitudes between the two countries is statistically significant
# sampling sites in Panama and Canada are geographically distinct along the east–west axis (longitude)

#EDIT: if instead the goal was to explore the difference in latitudinal/longitudinal ranges of BINs, then calculate the lat/lon ranges of the BINs per country
bin_ranges_p <- df_coordp %>%
  filter(!is.na(bin_uri)) %>%
  group_by(bin_uri) %>%
  summarize(lat_range = max(latitude) - min(latitude),
            lon_range = max(longitude) - min(longitude),
            .groups = "drop")

bin_ranges_c <- df_coordc %>%
  filter(!is.na(bin_uri)) %>%
  group_by(bin_uri) %>%
  summarise(
    lat_range = max(latitude) - min(latitude),
    lon_range = max(longitude) - min(longitude),
    .groups = "drop"
  )

#EDIT: merging into one dataframe for Wilcoxon testing
bin_ranges_p$country <- "Panama"
bin_ranges_c$country <- "Canada"
bin_ranges_all <- bind_rows(bin_ranges_p, bin_ranges_c)

#EDIT: before even running the Wilcoxon test, can predict that the results will not be statistically significant by looking at the dataframe; most of the BINs were taken from one sampling site, so this dataset is not suitable for seeing if there is actually a real difference in lat/lon ranges of BINs between the two countries
view(bin_ranges_all)
wilcox.test(lat_range ~ country, data = bin_ranges_all)
wilcox.test(lon_range ~ country, data = bin_ranges_all)

#EDIT: violin + boxplots to visualize distribution - majority of BINs are one-site samples
ggplot(bin_ranges_all, aes(x = country, y = lat_range, fill = country)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1) +
  theme_minimal() +
  labs(
    title = "Latitudinal Range Distribution per BIN",
    x = "Country",
    y = "Latitudinal range"
  ) +
  theme(legend.position = "none")

ggplot(bin_ranges_all, aes(x = country, y = lon_range, fill = country)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1) +
  theme_minimal() +
  labs(
    title = "Longitudinal Range Distribution per BIN",
    x = "Country",
    y = "Longitudinal range"
  ) +
  theme(legend.position = "none")


