#' Testing flow for getting Taskmaster data
#' - used as functions in data script

series <- 1
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

rawPicks <- rawTable %>% 
    filter(!str_detect(Task, "^Episode"))

rawPicks %>% 
    group_by(episode, Task) %>% 
    filter(n() > 1)
    
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