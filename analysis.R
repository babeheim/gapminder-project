
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



# facet_wrap countries in the americas

americas <- gapminder[gapminder$continent == "Americas",]


















# boxplots and violin plots



# custom color palettes


