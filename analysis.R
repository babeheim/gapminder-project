
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
