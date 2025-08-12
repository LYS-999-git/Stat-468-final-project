prep_shiny_data <- function() {
  Olympics_data <- read_csv("/Users/yushiliu/Stat 468 final project/data/track and field data/olympics_data.csv", show_col_types = F) %>%
    janitor::clean_names() %>% 
    separate_wider_delim(date, delim = "–", names = c("start", "end")) %>%
    separate_wider_delim(cols = event, names = c("gender", "event"), delim = " ",
                         too_many = "merge") %>%
    filter(str_detect(event, "Wheelchair", negate = T)) %>% 
    mutate(
      # Manually fixed Aminata CAMARA's age
      birth_date = if_else(name == "Aminata CAMARA", "06 DEC 1973", birth_date),
      gender = if_else(gender == "Men's", "Men", "Women"),
      across(c(birth_date, end),  ~as.Date(format(
        as.Date(., format = "%d %b %Y"), "%Y-%m-%d"))),
      age = year(end) - year(birth_date),
      games = factor(games, ordered = T,
                     levels = c("The XXXII Olympic Games",
                                "The XXXI Olympic Games",
                                "The XXX Olympic Games",
                                "The XXIX Olympic Games",
                                "The XXVIII Olympic Games",
                                "The XXVII Olympic Games",
                                "The XXVI Olympic Games"
                     ),
                     labels = c("Tokyo '20", "Rio '16", "London '12", 
                                "Beijing '08", "Athens '04", "Sydney '00",
                                "Atlanta '96")),
      event_type = case_match(
        event,
        c("100 Metres", "200 Metres", "400 Metres", "400 Metres Hurdles", "100 Metres Hurdles", "110 Metres Hurdles") ~ "Sprints",
        c("800 Metres", "1500 Metres", "3000 Metres Steeplechase") ~ "Middle Distance",
        c("5000 Metres", "10,000 Metres") ~ "Long Distance",
        c("Heptathlon", "Decathlon") ~ "Combined Events",
        c("High Jump", "Long Jump", "Triple Jump", "Pole Vault") ~ "Jumps",
        c("Shot Put", "Discus Throw", "Hammer Throw", "Javelin Throw", "Javelin Throw (old)") ~ "Throws",
        c("10 Kilometres Race Walk", "20 Kilometres Race Walk", "50 Kilometres Race Walk", "Marathon") ~ "Road Races",
        .default = "Other"
      ),
      event_category = if_else(str_detect(event, "Metres|Walk|Wheelchair") |event %in% c("Marathon"), "Track", "Field")
    ) %>% 
    mutate(event = if_else(event == "Javelin Throw (old)", "Javelin Throw", event),
           games_year = year(end)) %>% rename("nationality" = nat)
  
  # load career progression data and identify athletes who are still active
  Career_progression <- read_csv("/Users/yushiliu/Stat 468 final project/data/track and field data/career_progression (2).csv", show_col_types = F) %>% 
    distinct() |> 
    mutate(date = parse_date_time(date, orders = c("%d %b %Y", "%d-%b-%y"))) %>%
    mutate(retired = if_else(max(year) <= 2022, T, F),
           training_age = year - min(year) + 1,
           .by = c("athlete_link", "event")) 
  
  # find the seasons best performances of each athlete
  athlete_bests <-
    Career_progression %>%  inner_join(
      Olympics_data %>%  filter(!is.na(birth_date)) %>%
        distinct(birth_date, athlete_links, event, event_type, .keep_all = T) %>%
        select(name, birth_date, athlete_links, event, nationality, event_type, 
               event_category, gender, games_year),
      by = c("athlete_link" = "athlete_links", "event")
    ) %>%  
    mutate(age_years = year(date) - year(birth_date), 
           age_days = as.double(
             difftime(
               date, birth_date,units = "days"
             )
           ),
           performance = str_remove_all(performance, "h") # I will count hand timed results as legitimate
    ) %>%
    mutate(mark = if_else(
      event_category == "Track",
      as.numeric(
        difftime(
          lubridate::parse_date_time2(performance, orders = c("%H:%M:%S", "%M:%S:00", "%M:%OS", "%OS"), exact = T),
          lubridate::parse_date_time2("0", orders ="S"),
          units = "secs"
        )
      ), parse_number(performance)),
      # the below accounts for edge cases that lubridate can't parse, like a 62s 400MH
      mark = if_else(is.na(mark), parse_number(performance), mark)
    ) %>%
    mutate(
      best_performance = case_when(
        event_category == "Track" & mark == min(mark, na.rm = T) ~ T, # we want the lowest time
        event_category == "Field" & mark == max(mark, na.rm = T) ~ T, # we want the furthest/highest performance
        .default = F
      ),
      percent_off_peak = if_else(event_category == "Track", abs((mark - min(mark, na.rm = T))/mark), 
                                 abs((mark - max(mark, na.rm = T))/mark)),
      olympic_year = if_else(year %in% c(seq(1980, 2016, 4), 2021), T, F),
      .by = c("athlete_link", "event")
    ) %>% 
    # remove duplicate seasons bests
    slice_max(with_ties = F, n = 1, order_by = age_days, by = c("event", "year",  "athlete_link"))
  
  raw_json <- read_json("/Users/yushiliu/Stat 468 final project/data/track and field data/iaaf-2025.json", simplifyVector = FALSE)
  
  # Convert list of lists to a flat tibble
  iaaf_table <- tibble::tibble(
    gender = sapply(raw_json, `[[`, "gender"),
    event = sapply(raw_json, `[[`, "event"),
    mark = sapply(raw_json, `[[`, "mark"),
    points = sapply(raw_json, `[[`, "points")
  )
  athlete_bests_clean <- athlete_bests |> 
    mutate(
      gender = tolower(gender),
      event = case_when(
        event == "100 Metres" ~ "100m",
        event == "200 Metres" ~ "200m",
        event == "400 Metres" ~ "400m",
        event == "800 Metres" ~ "800m",
        event == "1500 Metres" ~ "1500m",
        event == "5000 Metres" ~ "5000m",
        event == "10,000 Metres" ~ "10000m",
        event == "Marathon" ~ "Road Marathon",
        event == "Triple Jump" ~ "TJ",
        event == "Long Jump" ~ "LJ",
        event == "High Jump" ~ "HJ",
        event == "Shot Put" ~ "SP",
        event == "Discus Throw" ~ "DT",
        event == "Hammer Throw" ~ "HT",
        event == "Javelin Throw" ~ "JT",
        event == "Decathlon" ~ "Dec.",
        event == "Heptathlon" ~ "Hept.",
        event == "Pole Vault" ~ "PV",
        event == "10 Kilometres Race Walk" ~ "10,000mW",
        event == "50 Kilometres Race Walk" ~ "50,000mW",
        event == "20 Kilometres Race Walk" ~ "20,000mW",
        event == "100 Metres Hurdles" ~ "100mH",
        event == "110 Metres Hurdles" ~ "110mH",
        event == "400 Metres Hurdles" ~ "400mH",
        event == "3000 Metres Steeplechase" ~ "3000m SC",
        TRUE ~ event
      )
    )
  
  # join iaaf_table with athlete_bests and get full dataset with result points
  round_decimals <- function(x, digits = 2) {
    round(x * 10^digits) / 10^digits}
  
  athlete_bests_points <- 
    athlete_bests_clean |>
    left_join(iaaf_table, join_by(gender, event, closest(mark <= mark))) |> 
    replace_na(list(points = 0))
  
  # Filter out missing points athletes and rename for clarity
  athlete_bests_points |> 
    filter(!is.na(points), !is.na(age_years)) 
}
print(1)
fd_data_shiny <- prep_shiny_data()
fd_data <- fd_data_shiny |> 
  mutate(id = paste(athlete_link, event)) |> 
  select(athlete_id = id,
         performance_age = age_years,
         iaaf_score = points)
print(2)
id <- fd_data$athlete_id
face_input <- data.frame(
  y = fd_data$iaaf_score,
  argvals = fd_data$performance_age,
  subj = id)
print(3)
fit_face <- face.sparse(face_input, argvals.new = seq(11, 58, by = 1))


