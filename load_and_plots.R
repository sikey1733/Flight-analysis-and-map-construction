# Подключение библиотек
library(dplyr)
library(ggplot2)
library(readr)
library(jsonlite)
library(tidyr)
library(lubridate)

# Функция загрузки данных, очистки, преобразование и обьединения
load_data <- function(df = "flights_df.csv",
                      airports_url = "https://raw.githubusercontent.com/mwgg/Airports/master/airports.json") {
  
  # Чтение данных перелетов
  if (!file.exists(df)) {
    stop("Файл с перелётами не найден: ", df)
  }
  
  flights_df <- read_csv(df, show_col_types = FALSE)
  message("Данные о перелетах загружены!")
  
  # Очистка пропусков
  if (anyNA(flights_df)) {
    flights_df <- drop_na(flights_df)
    message("NA удалены из данных перелетов.")
  } else {
    message("В данных перелетов нет пропусков.")
  }
  
  # Загрузка аэропортов
  airports_list <- fromJSON(airports_url)
  if (is.null(airports_list)) stop("Не удалось загрузить данные аэропортов.")
  
  airports_df <- bind_rows(airports_list, .id = "icao") %>%
    filter(iata != "")
  
  if (anyNA(airports_df)) {
    message("Обнаружены NA в данных аэропортов. Удаляю...")
    airports_df <- drop_na(airports_df)
  } else {
    message("Данные аэропортов чисты.")
  }
  
  # Объединение
  flights_geo <- flights_df %>%
    left_join(airports_df, by = c("origin" = "iata"), suffix = c("", "_origin"), relationship = "many-to-many") %>%
    rename(origin_lat = lat, origin_lon = lon) %>%
    left_join(airports_df, by = c("destination" = "iata"), suffix = c("", "_dest"), relationship = "many-to-many") %>%
    rename(dest_lat = lat, dest_lon = lon) %>% 
    drop_na()
  
  # Добавление колонки
  transform_data <- flights_geo %>% 
    mutate(
      flight_type = case_when(
        country == "RU" & country_dest == "RU" ~ "Внутренний",
        country == "RU" & country_dest != "RU" ~ "Из России за границу",
        country != "RU" & country_dest == "RU" ~ "В Россию из-за границы",
        TRUE ~ "Иностранный"),
      season = case_when(
        month(depart_date) %in% c(12, 1, 2) ~ "Зима",
        month(depart_date) %in% c(3, 4, 5) ~ "Весна",
        month(depart_date) %in% c(6, 7, 8) ~ "Лето",
        month(depart_date) %in% c(9, 10, 11) ~ "Осень"
      )
    )
  
  message("Успешно объединено ", nrow(transform_data), " рейсов с координатами.")
  
  return(transform_data)
}



# Построение графиков
df <- load_data()

# Какие самые популярные направления?
df %>%
  filter(state != "" & state_dest != "") %>% 
  count(state, state_dest, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(paste(state, state_dest, sep = " → "), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "ТОП-15 направлений", x = "Направление", y = "Число предложений")

# Из каких городов чаще всего вылеты?
df %>%
  filter(city != "") %>% 
  count(city, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(city, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "ТОП-15 городов отправления", x = "Город", y = "Число вылетов")

# Какие источники лидируют в количестве проданных билетов?
df %>%
  filter(gate != "") %>% 
  count(gate, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  ggplot(aes(x = reorder(gate, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "ТОП-15 источников покупки билетов", x = "Источник", y = "Число покупок")

# В какие дни чаще всего покупают билет?
df %>%
  mutate(weekday = weekdays(as.Date(found_at), abbreviate = FALSE)) %>%
  count(weekday, sort = TRUE) %>%
  mutate(weekday = factor(weekday, 
                          levels = c("понедельник", "вторник", "среда", "четверг", "пятница", "суббота", "воскресенье"))) %>%
  ggplot(aes(x = reorder(weekday, n), y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Активность по дням недели",
    x = "День недели",
    y = "Количество билетов"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# В какое время чаще всего покупают билеты?
df %>%
  mutate(
    time_15min = floor_date(found_at, "15 minutes"),
    time_label = format(time_15min, "%H:%M")
  ) %>%
  count(time_label) %>%
  ggplot(aes(x = time_label, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Активность покупки билетов по времени",
    x = "Время",
    y = "Количество билетов"
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)]) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  

# Группировка, подсчет и сохранение для дальнейшего анализа в QGIS
df_lines <- df %>%
  group_by(origin_lon, origin_lat, dest_lon, dest_lat) %>%
  summarise(movers = n(), .groups = 'drop') 

write.csv(df_lines, "flights_od.csv", row.names = FALSE)