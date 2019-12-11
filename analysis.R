
# gapminder analysis repository

# first lets download the gapminder data

download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv", destfile = "data/gapminder_data.csv")

gapminder <- read.csv("data/gapminder_data.csv")

summary(gapminder)

# data cleaning

se_asia <- c("India", "China", "Japan", "Vietnam", "Thailand")

gapminder_se_asia <- gapminder[gapminder$country %in% se_asia, ]
write.csv(gapminder_se_asia, "processed-data/gapminder_se_asia.csv", row.names = FALSE)

gapminder_2007 <- gapminder[gapminder$year == 2007, ]
write.csv(gapminder_2007, "processed-data/gapminder_2007.csv", row.names = FALSE)

# simple analysis

gapminder$gdp <- gapminder$gdpPercap * gapminder$pop

mod <- lm(pop ~ lifeExp, data = gapminder)
summary(mod)
str(mod)
attributes(mod)

# extract from the mod the residuals
# extract from the mod the coefficients


# calculating summary statistics in base R

sum(c(1, 2, 3))

sum(gapminder$gdp)

sum(gapminder$gdp[gapminder$year == 2007 & gapminder$continent == "Asia"])
mean(gapminder$gdp[gapminder$year == 2007 & gapminder$continent == "Asia"])

tapply(gapminder$gdp, gapminder$continent, sum)
tapply(gapminder$gdp, gapminder$year, sum)


tapply(gapminder$gdp, list(gapminder$continent, gapminder$year), sum)


## notes on different packages
# apply family functions -> plyr -> purrr
# dplyr
# tydr


# base-r graphics: `plot`, `points`, `boxplot`
# ggplot

library(ggplot2)

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp)) +
  geom_point()

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country)) +
  geom_line()

# lines for countries, colors by continent
ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country, color = continent)) +
  geom_line()

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country, color = continent)) +
  geom_line() + geom_point()

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country)) +
  geom_line(mapping = aes(color = continent)) + geom_point()

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country)) +
  geom_point() +
  geom_line(mapping = aes(color = continent))

# custom color for points

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country)) +
  geom_line(mapping = aes(color = continent)) + geom_point(color = "magenta1")

colors()

# try this out:
ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp))

basic <- ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp))

basic + geom_point()


# gpdPercap vs life expectancy

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  scale_x_log10() +
  geom_point() +
  geom_smooth(method = "lm", size = 1.5)

# aesthetic modifications using themes

pdf("results/figure1.pdf", height = 5, width = 7)

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  scale_x_log10() +
  geom_point(alpha = 0.1, size = 2) +
  geom_smooth(method = "lm", size = 1.5) +
  theme_minimal()

dev.off()


# eps()    <- classic vector format
# png()    <- lossless bitmap format
# tiff()   <- lossless bitmap format

# png version of figure 1

png("results/figure1.png", height = 5, width = 7, res = 300, units = "in")

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  scale_x_log10() +
  geom_point(alpha = 0.1, size = 2) +
  geom_smooth(method = "lm", size = 1.5) +
  theme_minimal()

dev.off()





# facet_wrap countries in the americas

americas <- gapminder[gapminder$continent == "Americas",]

# previously on swc...
ggplot(data = americas, mapping = aes(x = year, y = lifeExp, by = country)) +
  geom_line()

# with facet_wrap
fig2 <- ggplot(data = americas, mapping = aes(x = year, y = lifeExp, by = country)) +
  geom_line() +
  facet_wrap(~ country)

ggsave(filename = "results/figures2.png", plot = fig2, width = 12, height = 10, dpi = 300, units = "in")

# boxplots and violin plots

ggplot(data = gapminder, mapping = aes(x = continent, y = lifeExp, fill = continent)) +
  geom_boxplot() + 
  facet_wrap(~ year)

ggplot(data = gapminder, mapping = aes(x = continent, y = lifeExp, fill = continent)) +
  geom_violin()


# advanced data munging

library(dplyr)

gapminder <- read.csv("data/gapminder_data.csv")

x <- select(gapminder, year, country, gdpPercap)
y <- gapminder %>% select(year, country, gdpPercap)
gapminder %>% select(year, country, gdpPercap) -> y

reduced <- gapminder %>%
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap)


# average gdpPercap by continent
gapminder %>% group_by(continent) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))

# average life expectancy by country
lifeExp_bycountry <- gapminder %>%
  group_by(country) %>%
  summarize(mean_lifeExp = mean(lifeExp))

lifeExp_bycountry %>%
  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))

gapminder %>%
  group_by(continent) %>%
  summarize(se_le = sd(lifeExp)/sqrt(n()))

# mutate()

continent_year_summaries <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_gdp = mean(gdp),
            sd_gdp = sd(gdp))


# ggplot and dplyr

# using base-R to filter, then pass to ggplot:

starts.with <- substr(gapminder$country, start = 1, stop = 1)
az.countries <- gapminder[starts.with %in% c("A", "Z"), ]
ggplot(data = az.countries, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + facet_wrap(~ country)

# using dplyr and ggplot all in one call

gapminder %>%
  mutate(startsWith = substr(country, start = 1, stop =1)) %>%
  filter(startsWith %in% c("A", "Z")) %>%
  ggplot(aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + facet_wrap(~ country)


# Calculate the average life expectancy in 2002 of 2 randomly selected countries for each continent. Then arrange the continent names in reverse order. Hint: Use the dplyr functions arrange() and sample_n(), they have similar syntax to other dplyr functions.

set.seed(1234)
# R version 3.6.1 (2019-07-05)

gapminder %>%
  filter(year == 2002) %>%
  group_by(continent) %>%
  sample_n(2) %>%
  summarize(mean_lifeExp = mean(lifeExp)) %>%
  arrange(desc(continent))

# http://shinyapps.org/apps/RGraphCompendium/index.php


# "normal" version of gapminder data
# one-row-per-country-year
gap_normal <- data.frame(
  continent = c("Asia", "Asia", "Asia", "Asia"),
  country = c("China", "China", "China", "China"),
  year = c(1992, 1997, 2002, 2007),
  pop = c(1000, 1200, 1300, 1500),
  gdpPercap = c(100, 120, 300, 500),
  lifeExp = c(40, 50, 60, 70)
)

# one-row-per-country
gap_wide <- data.frame(
  continent = c("Asia"),
  country = c("China"),
  pop1992 = 1000,
  pop1997 = 1200,
  pop2002 = 1300,
  pop2007 = 1500,
  gdpPercap1992 = 100,
  gdpPercap1997 = 120,
  gdpPercap2002 = 300,
  gdpPercap2007 = 500,
  lifeExp1992 = 40,
  lifeExp1997 = 50,
  lifeExp2002 = 60,
  lifeExp2007 = 70
)

# longest possible version of this data:
# long table = element / attribute / value
gap_long <- data.frame(
  # identifying information
  country = c("China", "China", "China"),
  year = c(1992, 1992, 1992),
  # name of the attribute
  attribute = c("pop", "gdpPercap", "lifeExp"),
  # value of the attribute
  value = c(1000, 100, 40)
)


# we don't want factors! 
gap_wide <- read.csv("data/gapminder_wide.csv", stringsAsFactors = FALSE)

# install.packages("tidyr")
library(tidyr)
library(dplyr)

# pivot_longer on gap_wide to get to gap_long

gap_long <- gap_wide %>%
  pivot_longer(
    cols = c(starts_with("pop"), starts_with("lifeExp"), starts_with("gdpPercap")),
    names_to = "obstype_year", values_to = "obs_value"
  ) %>%
  separate(obstype_year, into = c("obs_type", "year"), sep = "_")

gap_long$year <- as.numeric(gap_long$year)

gap_long %>%
  group_by(continent, obs_type) %>%
  summarize(means=mean(obs_value))

# pivot_wider on gap_long to get to gap_normal

gap_normal <- gap_long %>%
  pivot_wider(names_from = obs_type, values_from = obs_value)

gapminder <- read.csv("data/gapminder_data.csv", stringsAsFactors = FALSE)

gap_normal <- gap_normal[,colnames(gapminder)]

# prep for the pivot
gap_temp <- gap_long %>%
  unite(var_ID, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_")

gap_wide_again <- gap_long %>%
  unite(var_ID, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_") %>%
  pivot_wider(names_from = var_names, values_from = obs_value) %>%
  separate(var_ID, into = c("continent", "country"), sep = "_")



# pivot_longer on gap_normal to get to gap_long

gap_long_test <- gap_normal %>%
  pivot_longer(
    cols = c("pop", "lifeExp", "gdpPercap"),
    names_to = "obs_type", values_to = "obs_value"
  )


