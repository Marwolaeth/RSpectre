required_packages <- c(
  'readr',
  'readxl',
  'haven',
  'data.table',
  'tidyr',
  'dplyr',
  'purrr',
  'ggplot2',
  'stringr',
  'stringdist',
  'forcats',
  'dtplyr',
  'lubridate',
  'httr',
  'rvest',
  'jsonlite',
  'tictoc',
  'tm',
  'tidytext',
  'R.temis',
  'topicmodels',
  'udpipe',
  'phrasemachine',
  'RNewsflow',
  'igraph'
)
installed_packages <- installed.packages()
if (!all(required_packages %in% installed_packages)) {
  install.packages(setdiff(required_packages, installed_packages))
}
rm(required_packages, installed_packages)

############ ВНЕШНИЕ РЕСУРСЫ ############
############ 
# MyStem
# © Яндекс
# https://yandex.ru/dev/mystem/
if (!dir.exists('tools')) dir.create('tools')

if (!file.exists('tools/mystem.exe')) {
  if (!file.exists('tools/mystem.zip')) {
    download.file(
      'http://download.cdn.yandex.net/mystem/mystem-3.1-win-64bit.zip',
      destfile = 'tools/mystem.zip',
      method = 'libcurl' # Метод, при котором скачивание не зависало
    )
  }
  unzip('tools/mystem.zip', exdir = 'tools')
}

############
# Словарь тональности Kartaslov
# Данные: https://github.com/dkulagin/kartaslov/
# Лицензия: https://creativecommons.org/licenses/by-nc-sa/4.0/
# Молодец: https://github.com/dkulagin
if (!dir.exists('tools')) dir.create('tools')

dest <- 'tools/emo_dict.csv'
if (!file.exists(dest)) {
  root <- 'https://raw.githubusercontent.com/dkulagin/kartaslov/master/dataset/'
  repo <- 'emo_dict'
  download.file(
    paste0(root, repo, '/emo_dict.csv'),
    destfile = dest
  )
  rm(root, repo, dest)
}
rm(dest)

############ СОБСТВЕННЫЕ ФОРМУЛЫ ############
# Аналоги getElement() для выбора подмножества или элемента
# Подходит для включения в пайплайн %>%
get_subset <- function(x, .what = sapply(x, length) > 0, exclude = NULL) {
  if (!is.null(exclude)) x <- x[setdiff(names(x), exclude)]
  '['(x, .what)
}

get_element <- function(x, .which = 1) {
  '[['(x, .which)
}

trim <- function(x, trim = .05){
  x[x > quantile(x, trim) & x < quantile(x, 1 - trim)]
}

# Аналог ВПР из Excel
return_match <- function(x, table, match_var, return_var) {
  table[[return_var]][intersect(x, table[[match_var]])]
}

# IF-Else для пайплайна %>%
conditionally <- function(data, condition, action, ...) {
  if(condition) do(data, data %>% action(...)) else data
}

# Функция для быстрого заключения строки в скобки/кавычки/и т.д.
str_enclose <- function(s, enclosure = c('(', ')')){
  if (enclosure[1] == '(')   enclosure <- c(enclosure, ')')
  if (enclosure[1] == '((')  enclosure <- c(enclosure, '))')
  if (enclosure[1] == '[')   enclosure <- c(enclosure, ']')
  if (enclosure[1] == '[[')  enclosure <- c(enclosure, ']]')
  if (enclosure[1] == '[[[') enclosure <- c(enclosure, ']]]')
  if (enclosure[1] == '{')   enclosure <- c(enclosure, '}')
  if (enclosure[1] == '{{')  enclosure <- c(enclosure, '}}')
  if (enclosure[1] == '<')   enclosure <- c(enclosure, '>')
  if (enclosure[1] == '<<')  enclosure <- c(enclosure, '>>')
  if (enclosure[1] == '>')   enclosure <- c(enclosure, '<')
  if (enclosure[1] == '«')   enclosure <- c(enclosure, '»')
  if (enclosure[1] == '‘')   enclosure <- c(enclosure, '’')
  if (enclosure[1] == '“')   enclosure <- c(enclosure, '”')
  paste0(enclosure[1], s, enclosure[length(enclosure)])
}

# Вместо отсутствующего значения/пустого множества (NULL)
# возвращает пропущенное значение (NA)
skip_null <- function(x, .else = NA) {
  x <- tryCatch(x, error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(.else) else return(x)
}

strip_html <- function(s) {
  require(rvest)
  html_text(read_html(paste0('<body>', s, '</body>')))
}

str_remove_punctuation <- function(s) {
  require(stringr)
  str_replace_all(s, '[\\,\\-——«»\\";\\?!\\(\\)]|[\\.|:‘’“”](?=\\s)', ' ') %>%
    str_squish()
}

get_last_file <- function(folder, .pattern = '.xlsx') {
  sort(
    list.files(
      folder,
      pattern = .pattern,
      full.names = TRUE
    ),
    decreasing = TRUE
  ) %>%
    get_element(1)
}

# Стеммер (лемматизатор) от Яндекса
# © Филипп Управителев (http://r.psylab.info/blog/author/konhis), 2015
# © Яндекс, 2019
.lemmatise <- function(x) {
  x <- enc2utf8(x)
  res <- system(
    'tools/mystem -cl -e cp1251',
    intern = TRUE,
    input = x
  )
  res <- gsub('[{}]', '', res)
  res <- gsub('(\\|[^ ]+)', '', res)
  res <- gsub('\\?', '', res)
  res <- gsub('\\s+', ' ', res)
  res
}
.lemmatise_wrap <- function(x) {
  require(stringr)
  skip_null(
    paste(
      .lemmatise(
        str_remove_punctuation(
          str_replace_all(x, '[\r\n]', ' ')
        )
      ),
      collapse = ' '
    )
  )
}
str_lemmatise <- function(s) {
  sapply(s, .lemmatise_wrap, USE.NAMES = FALSE)
}

# Функция, обратная as.formula()
str_from_formula <- function(f) {
  require(rlang)
  stopifnot(is_formula(f))
  Reduce(paste, deparse(f))
}

# Бутстреп — доверительный интервал для медианы (для отображения на диаграмме)
# Рассчитывается очень долго
# median_cl_boot <- function(x, conf.level = .95, na.rm = TRUE, nsim = 100000) {
#   y <- replicate(nsim, median(sample(x, replace = TRUE), na.rm = na.rm))
#   ymin = quantile(y, (1 - conf.level) / 2)
#   ymax = quantile(y, 1 - (1 - conf.level) / 2)
#   y    = median(y)
#   return(data.frame(y, ymin, ymax))
# }

############ ЗНАЧЕНИЯ ПО УМОЛЧАНИЮ ############
# Русские стоп-слова
stopwords_ru <- readLines('tools/stopwords_ru.txt', encoding = 'UTF-8')
# из разных источников
ru_stopwords <- Reduce(
  base::union,
  list(
    stopwords_ru,
    stopwords::data_stopwords_snowball$ru,
    stopwords::data_stopwords_stopwordsiso$ru
  )
)
rm(stopwords_ru)

# Для специфичных терминов (R.temis::specific_terms())
specific_term_vars <- c(
  # 'level',
  'term',
  'p_term_level',
  'p_level_term',
  'p_term_global',
  'n_term_level',
  'n_term_global',
  't.value',
  'p.value'
)