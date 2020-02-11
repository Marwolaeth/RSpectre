(a <- letters[1:10])
names(a) <- LETTERS[1:10]
a
get_element(a, 4)
get_element(a, 'D')
rm(a)

skip_null(NULL)
skip_null(NULL, 'fuck')

str_count('хуй', '\\w+')
str_count('хуй пойми', '\\w+')
str_count('хуй пойми что', '\\w+')

###############################
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
    stopwords = union(ru_stopwords, c(tm::stopwords('en'), 'u'))
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
tdd[tdd$term %in% c('сечин', 'судариков', 'елена прохоров'), c(1, 4, 7)]
tdd %>%
  filter(days.entropy.norm > .3) %>%
  arrange(desc(days.entropy.norm)) %>%
  slice(1:50) %>%
  View()
quantile(tdd$days.entropy.norm)
save(
  tdd,
  file = file.path(
    out_dir,
    paste(Sys.Date(), 'tdd.RData', sep = '_')
  )
)
# load(get_last_file(out_dir, 'tdd.RData'))

select_terms <- tdd$term[tdd$days.entropy.norm <= .3]
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
  hour.window = c(-48, 144),
  min.similarity = 0.03,
  measure = 'cosine',
  # margin_attr = FALSE,
  verbose = TRUE
)

g1 <- as.undirected(g, mode = 'collapse')
cl <- cluster_louvain(g1, weights = 1+E(g1)$weight)
cl <- cluster_edge_betweenness(g1)
cl <- cluster_fast_greedy(g1)
str(cl, 1)
cl$membership
v <- as_data_frame(g, 'vertices') %>% as_tibble()
setequal(names(V(g)), meta$doc_id)

v1 <- v %>%
  mutate(cluster = cl$membership) %>%
  select(date, header, source, from_sum, to_sum, role, cluster) %>%
  arrange(cluster)
