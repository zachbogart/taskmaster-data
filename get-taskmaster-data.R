#' Getting Taskmaster Data Wrangled
#' - Scrape wiki data, export results
#' 
#' Notes:
#' - 2022-10-15: Exported episode player totals, task-level tables within
#'   each episode is more challenging, need to handle corner case somehow
#'

library(tidyverse)
library(rvest)
library(here)
library(janitor)
library(lubridate)

## Get Totals ----

getSeriesTotals <- function(series) {
    url <- paste0("https://taskmaster.fandom.com/wiki/Series_", series)
    
    html <- read_html(url)
    
    # read in raw
    df <- html %>% 
        html_element(".tmtable") %>% 
        html_table(header = T)
    
    rawTable <- df %>% 
        # remove totals, count episodes
        filter(str_detect(Task, "^Total|^Episode")) %>% 
        mutate(temp = str_detect(Task, "^Episode")) %>% 
        mutate(episode = cumsum(temp), .before = Task) %>% 
        select(-temp)
    
    episodes <- rawTable %>% 
        group_by(episode) %>% 
        slice_head(n = 1) %>% 
        select(episode, episodeRawString = Task)
    
    dataTotals <- rawTable %>% 
        filter(!str_detect(Task, "^Episode")) %>% 
        pivot_longer(
            cols = -c(episode, Task, Description),
            names_to = "player",
            values_to = "score"
        ) %>% 
        clean_names() %>% 
        select(-c(task, description)) %>% 
        mutate(series = series, .before = everything())
    
    dataTotals %>% 
        left_join(episodes)
}

rawTotals <- map_dfr(seq(1, 13), ~getSeriesTotals(.x))

totals <- rawTotals %>% 
    mutate(total = as.numeric(str_replace(score, "\\[[0-9]\\]", ""))) %>% 
    select(-score) %>% 
    arrange(series, episode, total) %>% 
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
    select(-c(ends_with("String"), dateRaw)) %>% 
    select(series, episode, name, date, everything())

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

### Scores ----

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

### Episode Strings ----
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

### TODO: Checking totals ----
#' There are some row entries that have subparts
#' even though they actually just show one value
#' (multiple rows for text, one for points)
#' --> Need a way to get rid of these

episodeTotals <- taskmaster2 %>% 
    group_by(series, episode, player) %>% 
    summarise(total = sum(score, na.rm = T)) %>% 
    arrange(series, episode, total)

checkTotals <- episodeTotals %>% 
    left_join(
        totals, 
        by = c("series", "episode", "player"),
        suffix = c("", "_table")
    )
mismatchedEpisodes <- checkTotals %>% 
    filter(total != total_table)

# duplicates, trying to filter out subtask repeats
dupes <- taskmaster2 %>% 
    group_by(series, episode, task, player) %>% 
    arrange(series, episode, task, player) %>% 
    filter(n() > 1)

# Export ----
totals %>% write_csv(here("outputs/taskmaster_episode_totals.csv"))
