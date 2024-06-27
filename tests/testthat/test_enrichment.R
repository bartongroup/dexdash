# Set 100 features
N <- 100
features_all <- sprintf("gene_%03d", seq_len(N))
symbols_all <- sprintf("symbol_%03d", seq_len(N))
feat2name <- purrr::set_names(symbols_all, features_all)

# Three terms with 50, 10 and 3 features
term_ids <- c("term_1", "term_2", "term_3")
term_names <- c("name_1", "name_2", "name_3")
term_sizes <- c(50, 10, 3)

# Create terms
terms <- tibble::tibble(
  term_id = term_ids,
  term_name = term_names
)

# Create mapping
set.seed(666)
mapping <- purrr::map2(term_ids, term_sizes, function(tid, n) {
  tibble::tibble(
    term_id = tid,
    id = sample(features_all, n)
  )
}) |>
  purrr::list_rbind()

# Gene selection for testing
set.seed(42)
test_sel <- mapping |>
  dplyr::group_by(term_id) |>
  dplyr::sample_n(3) |>
  dplyr::pull(id)

# Prepare for fenr
fterms <- fenr::prepare_for_enrichment(terms, mapping, feature_name = "id")

# Expected enrichment from fenr
enr <- fenr::functional_enrichment(features_all, test_sel, fterms, feat2name = feat2name)

# Gene selection for testing
set.seed(42)
test_sel <- mapping |>
  dplyr::group_by(term_id) |>
  dplyr::sample_n(3) |>
  dplyr::pull(id)



test_that("make_functional_enrichment returns correct results", {
  returned <- make_functional_enrichment(test_sel, features_all, fterms, id2name = feat2name, fdr_limit = 1)
  expected <- enr |>
    dplyr::select(TermID = term_id, Name = term_name, N_with, n_with_sel, OR = odds_ratio, ids, p_value, FDR = p_adjust)
  expect_equal(returned, expected)
})

test_that("make_functional_enrichment returns NULL for one point", {
  res <- make_functional_enrichment(test_sel, features_all, fterms, id2name = feat2name, fdr_limit = 1, max_points = 1)
  expect_is(res, "data.frame")
  expect_setequal(names(res), c("Error"))
})
