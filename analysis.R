
# gapminder analysis repository

# first lets download the gapminder data

download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv", destfile = "data/gapminder_data.csv")

gapminder <- read.csv("data/gapminder_data.csv")
