
# Подключение библиотек
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Загрузка списка аэропортов (с IATA-кодами)
airports_url <- "https://raw.githubusercontent.com/mwgg/Airports/master/airports.json"
airports_data <- fromJSON(airports_url)

# Фильтрация только аэропорты в России
russian_airports <- Filter(function(x) x$country == "RU", airports_data)
iata_codes <- sapply(russian_airports, function(x) x$iata)

# Удаление пустых или NA кодов
iata_codes <- iata_codes[!is.na(iata_codes) & iata_codes != ""]

# Функция запроса для одного города
get_prices <- function(origin, token) {
  url <- paste0(
    "https://api.travelpayouts.com/v2/prices/latest?",
    "currency=rub&origin=", origin,
    "&limit=1000&token=", token
  )
  
  res <- GET(url)
  if (status_code(res) == 200) {
    json_data <- content(res, "text")
    parsed <- fromJSON(json_data)$data
    return(parsed)
  } else {
    warning("Ошибка запроса для: ", origin)
    return(NULL)
  }
}

# Загрузка всех маршрутов через цикл
all_routes <- list()

for (origin in iata_codes) {
  cat("Загружаем:", origin, "\n")
  result <- get_prices(origin, token)
  if (!is.null(result)) {
    all_routes[[origin]] <- result
  }
  Sys.sleep(1)
}

# Объединение в один датафрейм
flights_df <- bind_rows(all_routes, .id = "origin")

# Просмотр
head(flights_df)

# Сохранение
write_csv(flights_df, "flights_df.csv")
