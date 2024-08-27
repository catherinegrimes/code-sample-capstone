library(webshot)
library(fastDummies)
library(biglm)
library(tidyverse)
library(dplyr)
library(jtools)
library(stargazer)

dfold <- read_csv('../working.csv')

#drop Jan 1 releases
df <- dfold %>% 
  filter(!(release_day == 1 & release_month == 1))

#new superstar binary
df <- df %>%
  arrange(artist, year) %>%
  group_by(artist) %>%
  mutate(superstar2 = ifelse(charted == 1 | lag(charted) == 1 & year - lag(year) <= 5, 1, 0)) %>%
  ungroup()


#log variable, make sure not -inf
df$weeks_on_chart[is.na(df$weeks_on_chart)] <- 0
df$weeks_on_chart[df$weeks_on_chart == 0] <- 0.0001
df$lnweeks <- log(df$weeks_on_chart)

#fix NAs --- attempt to get rid of two colors in January bar
df$superstar2[is.na(df$superstar2)] <- 0
df$peak_position[is.na(df$peak_position)] <- 0
df <- subset(df, select = c(song, artist, release_date, charted, energy, valence,
                            weeks_on_chart, peak_position, superstar2, genre, 
                            release_month, release_day, year, season, weekday,
                            lnweeks))


write_csv(df, 'finalclean.csv') #to be used in visualizations


#catergorical dummies
month_dummies <- dummy_cols(df, select_columns = 'release_month')
season_dummies <- dummy_cols(df, select_columns = 'season')

#x = month, y = weeks on chart
month.model <- lm(weeks_on_chart ~ release_month_1 + release_month_2 + 
                       release_month_3 + release_month_4 + release_month_5 + 
                       release_month_6 + release_month_7 + release_month_8 +
                       release_month_9 + release_month_10 + release_month_11 +
                      energy + valence + superstar2 + genre, data = month_dummies)
summ(month.model)

stargazer(month.model, type = 'text', out = "month")


#x = month, y = binary charted
month.logit <- glm(charted ~ release_month_1 + release_month_2 + release_month_3 
                 + release_month_4 + release_month_5 + release_month_6 + 
                   release_month_7 + release_month_8 + release_month_9 + 
                   release_month_10 + release_month_11 + energy + valence + 
                   superstar2 + genre, data = month_dummies, family = binomial)
summ(month.logit)

stargazer(month.logit, type = 'text', out = 'month_logit')

#x = season, y = log weeks on chart
season_ln <- lm(lnweeks ~ season_Fall + season_Winter + season_Summer + release_month +
                 energy + valence + superstar2 + genre, data = season_dummies)
summ(season_ln)

#export season, lnweeks
stargazer(season_ln, type = "text", out = "season_ln") 


#x = month, y = log weeks on chart
month_ln <- lm(lnweeks ~ release_month_1 + release_month_2 + 
                 release_month_3 + release_month_4 + release_month_5 + 
                 release_month_6 + release_month_7 + release_month_8 +
                 release_month_9 + release_month_10 + release_month_11 +
                 energy + valence + superstar2 + genre, data = month_dummies)
summ(month_ln)

#export month, lnweeks
stargazer(month_ln, type = "text", out = "month_ln", covariate.labels = 
            c('January', 'February', 'March', 'April', 'May', 'June', 'July',
              'August', 'September', 'October', 'November', 'Energy (low to high)', 
              'Mood (sad to happy)', 'Superstar'), 
          keep = seq(1, 14)) 

month_peak <- lm(peak_position ~ release_month_1 + release_month_2 + 
                    release_month_3 + release_month_4 + release_month_5 + 
                    release_month_6 + release_month_7 + release_month_8 +
                    release_month_9 + release_month_10 + release_month_11 +
                    energy + valence + superstar2 + genre, data = month_dummies)

stargazer(month_peak, type = 'text', out = 'month_peak')



#extra
#releases on the first by month
df %>%
  group_by(release_month) %>%
  summarize(Month_Total = n(), 
            First_Total = sum(release_day == 1)) %>%
  mutate(Percent = First_Total / Month_Total * 100)



### EXPORTING / CLEAN TABLES --- old
html <- as.character(summary_table)

# Export the table as an image
webshot(html, file = "lnmonthsummary.png")


flitz <- data.frame(
  answer = c('yes', 'yes', 'yes', 'yes', 'yes', 'yes', 'yes', 'yes', 'yes',
             'yes in green', 'yes', 'yes', 'yes', 'yes')
)
fcounts = table(flitz)

pie(fcounts, labels = '', main = '', 
    col = c("#619CFF", "#64E572"), border = "white", cex = 0.8)

# Add a legend
legend(x = -2.4, y = 0.5, legend = names(fcounts), fill = c("#619CFF", "#64E572"))
title(main = "What should you say?", adj = 0)
