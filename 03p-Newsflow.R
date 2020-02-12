current_project <- 'Regions'
current_dir <- file.path('data', current_project)
out_dir <- file.path(current_dir, 'output')
in_dir <- file.path(current_dir, 'input')
rm(current_project, current_dir)

last_file <- get_last_file(in_dir)

news <- read_excel(
  last_file,
  sheet = 'Упоминания'
) %>%
  set_names(
    c(
      'date',
      'doc_id',
      'header',
      'text',
      'source',
      'url',
      'ci',
      'source.cat',
      'comments',
      'sentiment',
      'role',
      'country',
      'region',
      'language'
    )
  ) %>%
  select(
    date,
    doc_id,
    header,
    text,
    source,
    ci,
    source.cat,
    sentiment,
    role
  ) %>%
  mutate(
    date       = dmy_hm(date, tz = 'Europe/Moscow'),
    doc_id     = as.character(doc_id),
    text       = map_chr(paste(header, text), strip_html),
    source     = as.factor(source),
    source.cat = as.factor(source.cat),
    sentiment  = as.factor(sentiment),
    role       = as.factor(role)
  )
news
if (!dir.exists(out_dir)) dir.create(out_dir)
save(
  news,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'df.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'df.RData'))

tictoc::tic()
news <- mutate(news, text_lem = str_lemmatise(str_remove_punctuation(text)))
tictoc::toc()
save(
  news,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'lem.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'lem.RData'))

meta <- select(news, -text, -text_lem)
save(
  meta,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'meta.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'meta.RData'))

tictoc::tic()
txt <- news %>%
  unnest_tokens(
    input = text_lem,
    output = term,
    token = 'skip_ngrams',
    n = 3L,
    n_min = 1L,
    k = 1L,
    stopwords = union(
      ru_stopwords, c(tm::stopwords('en'), 'u', c('2019', '2020'))
    )
  ) %>%
  # Обходим юникод
  filter(!str_detect(term, '^[udbcf0-9\\s]+$')) %>%
  filter(!str_detect(term,'^\\d+$')) %>%
  select(
    doc_id, term
  ) %>%
  mutate(ngram = str_count(term, '\\w+')) %>%
  group_by(doc_id) %>%
  mutate(
    position = ngram == 1
  ) %>%
  mutate(
    position = as.numeric(cumsum(position))
  ) %>%
  mutate(
    position = if_else(
      ngram == 1,
      position,
      cummax(position) + ngram - 1
    )
  ) %>%
  ungroup()
tictoc::toc()
save(
  txt,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'tokens.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'tokens.RData'))
txt <- filter(txt, ngram > 1)

rm(news)

txt <- txt %>%
  mutate(weight = weight_terms(position)) %>%
  mutate(weight = if_else(ngram > 1, weight * ngram / (ngram - 1), weight))
summary(txt)
save(
  txt,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'weighted.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'weighted.RData'))

tictoc::tic()
txt <- txt %>%
  lazy_dt() %>%
  group_by(doc_id, term) %>%
  summarise(
    n = n(),
    weight = ceiling(sum(weight))
  ) %>%
  ungroup() %>%
  as_tibble()
tictoc::toc()
summary(txt)
save(
  txt,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'counted.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'counted.RData'))

dtm <- txt %>%
  cast_dtm(
    document = doc_id,
    term = term,
    value = weight
  ) %>%
  weightSMART('ntc')
dtm <- dtm[meta$doc_id,]
all(Docs(dtm) == meta$doc_id)
all(dtm$dimnames$Docs == meta$doc_id)
save(
  dtm,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'dtm.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'dtm.RData'))

dtm1 <- txt %>%
  cast_dtm(
    document = doc_id,
    term = term,
    value = n
  ) %>%
  weightTf()
dtm1 <- dtm1[meta$doc_id,]
all(Docs(dtm1) == meta$doc_id)

tictoc::tic()
tdd = term_day_dist(dtm1, meta)
tictoc::toc()
tdd[sample(1:nrow(tdd), 13),] # show 13 random rows
tdd[111:124, c(1, 4, 5)]
tdd[tdd$term %in% c('игорь сечин', 'сергей судариков', 'елена прохоров'), c(1, 4, 7)]
tdd %>%
  filter(days.entropy.norm > .2) %>%
  arrange(desc(days.entropy.norm)) %>%
  slice(1:111) %>%
  View()
quantile(tdd$days.entropy.norm)
hist(tdd$days.entropy.norm)
save(
  tdd,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'tdd.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'tdd.RData'))

select_terms <- tdd$term[tdd$days.entropy.norm <= .25]
dtm <- dtm[,select_terms]
all(Docs(dtm) == meta$doc_id)
all(dtm$dimnames$Docs == meta$doc_id)

save(
  dtm,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'dtm_sel.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'dtm_sel.RData'))
rm(dtm1, tdd, txt, select_terms)
gc()

g = newsflow.compare(
  dtm,
  meta = meta,
  # id.var = 'doc_id',
  hour.window = c(0, 72),
  min.similarity = .08,
  measure = 'cosine',
  # margin_attr = FALSE,
  verbose = TRUE
)

g1 <- as.undirected(g, mode = 'collapse')
# cl <- cluster_louvain(g1, weights = 1+E(g1)$weight)
tictoc::tic()
cl <- cluster_edge_betweenness(g1) # 3 min
tictoc::toc()
# cl <- cluster_fast_greedy(g1)

save(
  cl,
  file = file.path(
    out_dir,
    paste(Sys.Date()+1, 'clustering.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'clustering.RData'))
str(cl, 1)
cl$membership
summary(cl$membership)
v <- as_data_frame(g, 'vertices') %>% as_tibble()
setequal(names(V(g)), meta$doc_id)

v1 <- v %>%
  mutate(cluster = cl$membership) %>%
  # select(date, header, source, from_sum, to_sum, role, cluster) %>%
  arrange(cluster)

vlabels <- v1 %>%
  group_by(cluster) %>%
  arrange(desc(from_sum)) %>%
  mutate(nwords = str_count(header, '\\w+')) %>%
  filter(nwords >= min(5, max(nwords))) %>%
  slice(1) %>%
  mutate(label = header) %>%
  ungroup() %>%
  select(cluster, label)
vlabels

news <- v %>%
  mutate(date = ymd_hms(date), cluster = cl$membership) %>%
  left_join(vlabels) %>%
  select(doc_id, cluster, label) %>%
  left_join(meta) %>%
  add_count(cluster, name = 'n_cl')
news

save(
  news,
  file = file.path(
    out_dir,
    paste(Sys.Date()+2, 'clustered.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'clustered.RData'))

library(XLConnect)
output <- loadWorkbook(
  file.path(out_dir, get_last_file(in_dir, fullname = FALSE)),
  create = T
)
createSheet(output, 'Упоминания')
writeWorksheet(
  output,
  data = news %>%
    select(
      doc_id,
      date,
      header,
      source,
      ci,
      source.cat,
      sentiment,
      role,
      cluster,
      label,
      n_cl) %>%
    set_names(
      'ID сообщения',
      'Дата',
      "Заголовок",
      "Источник",
      "Индекс цитирования",
      "Категория источника",
      "Тональность",
      "Роль объекта",
      "Кластер",
      "Инфоповод",
      "Размер кластера"
    ),
  sheet = 1
)
saveWorkbook(
  output,
  file = file.path(out_dir, get_last_file(in_dir, fullname = FALSE))
)

news <- left_join(news, meta)
subjects <- news %>%
  group_by(label) %>%
  summarise(
    from = min(date),
    to = max(date),
    s = any(str_detect(tolower(header), 'сечин')),
    ci_sum = sum(ci, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(from) %>%
  mutate_at(vars(from, to), ~ format(., '%d.%m.%Y')) %>%
  unite('period', from, to, sep = '—', remove = FALSE) %>%
  filter(n >= 4 | s == TRUE)
news_tag <- select(news, doc_id, label)
load(get_last_file(out_dir, 'lem.RData'))
news_tag <- left_join(news, news_tag) %>%
  semi_join(subjects) %>%
  group_by(label) %>%
  summarise(
    st = any(str_detect(tolower(text), 'сечин'))
  )
sum(news_tag$st)
subjects <- subjects %>%
  left_join(select(news_tag, label, st)) %>%
  select(label, from, to, period, s, st, ci_sum, n) %>%
  mutate_at(vars(s, st), as.integer) %>%
  mutate_at(vars(from, to), dmy)
createSheet(output, 'Инфоповоды')
writeWorksheet(
  output,
  data = subjects %>%
    set_names(
      'Инфоповод',
      "С",
      "По",
      'Период',
      "Сечин в заголовке?",
      "Сечин в тексте?",
      "Индекс цитирования",
      "Количество публикаций"
    ),
  sheet = 2
)
saveWorkbook(
  output,
  file = file.path(out_dir, get_last_file(in_dir, fullname = FALSE))
)

hist(unique(news$n_cl), breaks = 20)
significant_clusters <- news %>%
  semi_join(subjects) %>%
  arrange(label)

dtm_tags <- dtm[significant_clusters$doc_id, ]
all(dtm_tags$dimnames$Docs == significant_clusters$doc_id)
any(is.na(dtm_tags$j))
any(duplicated(dtm_tags$dimnames$Terms))

tag_terms <- specific_terms(
  dtm_tags,
  variable = significant_clusters$label,
  p = .05,
  min_occ = 1,
  n = 100
) %>%
  map(as.data.frame) %>%
  map(tibble::rownames_to_column, 'term') %>%
  map(as_tibble) %>%
  bind_rows(.id = 'tag') %>%
  select(-10) %>%
  set_names(c('tag', specific_term_vars)) %>%
  filter(p_level_term >= 99 | str_detect(tolower(tag), 'сечин'))
tag_terms

tag_terms <- tag_terms %>%
  filter(p_level_term != 0) %>%
  mutate(ngram = str_count(term, '\\w+')) %>%
  mutate(tilda = if_else(ngram == 1, 0, ngram * (ngram - 1))) %>%
  mutate(
    query = if_else(
      ngram == 1,
      term,
      paste(str_enclose(term, '"'), tilda, sep = '~')
    )
  )
save(
  tag_terms,
  file = file.path(
    out_dir,
    paste(Sys.Date()+1, 'specific.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'specific.RData'))

tags <- tag_terms %>%
  group_by(tag) %>%
  arrange(desc(n_term_level)) %>%
  summarise(
    query = paste(query, collapse = ', ')
  ) %>%
  left_join(select(subjects, tag = label, from, period, ci_sum, st, n)) %>%
  mutate(from = dmy(from)) %>%
  arrange(from) %>%
  mutate(st = if_else(st == 1, 'Сечин!', ''))
tags
queries <- file(
  file.path(out_dir, paste(Sys.Date()+1, 'queries.txt', sep = '_')),
  open = 'w'
)
cat('Поисковые запросы:\n\n', file = queries)

tags %>%
  mutate(label = tag) %>%
  group_by(from, tag) %>%
  arrange(from) %>%
  group_walk(
    function(tg, ...) {
      cat(
        str_interp('${tg$label} (${tg$period}, кол-во: ${tg$n} ${tg$st}):'),
        file = queries,
        append = TRUE
      )
      cat('\n\n', file = queries, append = TRUE)
      cat(tg$query, file = queries, append = TRUE)
      cat('\n\n\n', file = queries, append = TRUE)
    }
  )
close.connection(queries)
