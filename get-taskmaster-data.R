library(tidyverse)
library(rvest)
library(here)
library(janitor)
library(lubridate)

## Get Series Data ----
getSeriesData <- function(series) {
    
    url <- paste0("https://taskmaster.fandom.com/wiki/Series_", series)
    
    html <- read_html(url)
    
    # read in raw
    df <- html %>% 
        html_element(".tmtable") %>% 
        html_table(header = T)
    
    rawTable <- df %>% 
        # remove totals, count episodes
        filter(!str_detect(Task, "^Total|Grand Total")) %>% 
        mutate(temp = str_detect(Task, "^Episode")) %>% 
        mutate(episode = cumsum(temp), .before = Task) %>% 
        select(-temp)
    
    episodes <- rawTable %>% 
        group_by(episode) %>% 
        slice_head(n = 1) %>% 
        select(episode, episodeRawString = Task)
    
    dataScores <- rawTable %>% 
        filter(!str_detect(Task, "^Episode")) %>% 
        pivot_longer(
            cols = -c(episode, Task, Description),
            names_to = "player",
            values_to = "score"
        ) %>% 
        clean_names() %>% 
        mutate(series = series, .before = everything())
    
    dataScores %>% 
        left_join(episodes)
}

taskmaster0 <- map_dfr(seq(1, 13), ~getSeriesData(.x))

## Scores ----

# clean scores
cleanScores <- suppressWarnings(
    taskmaster0 %>% 
    # remove citations
    mutate(score = str_replace(score, "\\[[0-9]\\]", "")) %>% 
    # filter out the dashes (did not participate in version of task)
    filter(!str_detect(score, "^–$|^-$")) %>% 
    # remove the tiebreakers
    filter(!str_detect(score, "✔|✘")) %>% 
    # convert the disqualified values to NA
    # score as a number in R
    mutate(score = as.numeric(score))
)
    
taskmaster1 <- cleanScores

## Episode Strings ----
rawEpisodes <- taskmaster1 %>% 
    select(series, episode, episodeRawString) %>% 
    distinct()

cleanEpisodes <- rawEpisodes %>% 
    # remove episode number
    mutate(episodeString = str_trim(str_replace(episodeRawString, "^Episode [0-9]+:", ""))) %>%
    # separate into date and name
    separate(episodeString, into = c("name", "dateRaw"), remove = F, sep = " \\(") %>% 
    # formatting
    mutate(
        name = str_trim(name),
        dateRaw = (str_replace(dateRaw, "\\)", "")),
        date = parse_date(dateRaw, "%d %B %Y")
    ) %>% 
    select(series, episode, name, date)

taskmaster2 <- taskmaster1 %>% 
    left_join(cleanEpisodes) %>% 
    select(-episodeRawString) %>% 
    relocate(name, .after = episode)

# repeats
taskmaster2 %>% 
    group_by(series, episode, task, player) %>% 
    filter(n() > 1) %>% 
    arrange(series, episode, task, player) %>% view()


# Checking totals ----

seriesTotals <- taskmaster2 %>% 
    group_by(series, player) %>% 
    summarise(total = sum(score, na.rm = T)) %>% 
    arrange(series, total)

testingTotals <- taskmaster2 %>% 
    filter(series == 1) %>% 
    group_by(episode, player) %>% 
    summarise(total = sum(score, na.rm = T)) %>% 
    arrange(episode, total)

testingTotals %>% 
    print(n = nrow(.))