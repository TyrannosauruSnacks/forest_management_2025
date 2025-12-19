# test_import


## Prerequisites

``` r
library(tidyverse)
library(readr)
set_theme(theme_light())
update_theme(text = element_text(size   = 12,
                                 family = "DejaVu Serif"))
```

## Import and tidy data

``` r
results_bas <-
    read_delim(
      "data/bas_results.txt", 
      delim = "\t", escape_double = FALSE,
      show_col_types = FALSE,
      trim_ws = TRUE
      )

results_siafi <- 
    read_delim(
      "data/siafi_results.txt", 
      delim = "\t", escape_double = FALSE, 
      show_col_types = FALSE,
      trim_ws = TRUE
      )

results_he <- 
    read_delim(
      "data/he_results.txt", 
      delim = "\t", escape_double = FALSE, 
      show_col_types = FALSE,
      trim_ws = TRUE
      )

results <- 
  bind_rows(
    list(
      bas   = results_bas,
      he    = results_he,
      siafi = results_siafi),
    .id = "strategy") |> 
  janitor::clean_names() |> 
  select(-bestand) |> 
  mutate(
    strategy = str_to_upper(strategy),
    strategy = fct_relevel(fct(strategy), "BAS", "SIAFI", "HE"))
```

## Plot

``` r
parameters <- 
  c(
    g_wert_l    = "Economic yield (€)",
    wert_a      = "Net realizable value, harvested (€)",
    artprofil   = "mod. Shannon-Index",
    wert_v      = "Net realizable value, standing (€)"
  )

results |> 
  filter(baumart == "Alle Arten") |> 
  select(strategy, periode, baumart,
           wert_a, wert_v, g_wert_l, artprofil) |>
  mutate(
    g_wert_l = case_when(strategy == "bas" ~ wert_v,
                          TRUE ~ g_wert_l),
    periode  = (periode - 1) * 5) |> 
  pivot_longer(c(wert_a, artprofil),
               names_to  = "parameter",
               values_to = "value") |> 
  ggplot(aes(periode, value, linetype = strategy, color = strategy)) +
  geom_line(linewidth = .66) +
  labs(
    y        = "Value",
    x        = "Simulated years",
    color    = "Strategy",
    linetype = "Strategy"
  ) +
  scale_color_discrete(palette = "set2") +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_wrap(~parameter, scales = "free",
             labeller = labeller(parameter = parameters))
```

![](data_exploration_visualization_files/figure-commonmark/unnamed-chunk-2-1.png)

### Save plot for document

``` r
ggsave("./images/plot_results.png",
       units = "mm",
       width = 180 * 1,
       height = 180 * 1 /(16/9)^2
       )
```

## Create an overview

``` r
overview <- 
  results |> 
    filter(baumart == "Alle Arten") |> 
    group_by(strategy) |> 
    summarise(
      nrv_h          = sum(wert_a),
      economic_yield = max(g_wert_l)
      ) |>
  # Reducing the economic yield of BAS strategy by 
  # the lost winnings from leaving the thinnings in the stand.
  mutate(economic_yield_real = if_else(strategy == "BAS",
                                       economic_yield - nrv_h,
                                       economic_yield
                                       ))

overview <- 
  results |> 
    filter(baumart == "Alle Arten") |> 
    slice_tail(n = 1, by = "strategy") |>
    select(strategy,
           nrv_s = wert_v,
           shannon_index = artprofil) |> 
    left_join(overview,
              by = "strategy") |> 
    relocate(nrv_h, nrv_s, economic_yield_real, shannon_index,
             .before = economic_yield) |> 
  select(-economic_yield)

overview
```

    # A tibble: 3 × 5
      strategy nrv_h nrv_s economic_yield_real shannon_index
      <fct>    <dbl> <dbl>               <dbl>         <dbl>
    1 BAS      17096 27455               27456          1.08
    2 HE        7836 39187               47023          0.92
    3 SIAFI     9993 33655               43648          1.13

``` r
writexl::write_xlsx(overview, path = "./data/overview.xlsx")
```

### Plot the overview

``` r
overview |> 
  select(strategy, 
         "NRVh"           = nrv_h,
         "NRVs"           = nrv_s,
         "Economic yield" = economic_yield_real) |> 
  pivot_longer(-strategy, 
               names_to = "param",
               values_to = "value") |> 
  ggplot(aes(x = factor(param), y = value, fill = strategy)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(palette = "set2") +
  scale_y_continuous(labels = scales::label_currency(prefix = "€")) +
  labs(x = NULL,
       y = NULL,
       fill = "Strategy",
       # caption = "NRVh = Net realizable value, harvested \\ NRVs = Net realizable value, standing"
       )
```

![](data_exploration_visualization_files/figure-commonmark/unnamed-chunk-5-1.png)

#### And save it

``` r
ggsave("./images/plot_overview.png",
       units = "mm",
       width = 180 * .7,
       height = 180 * .7 /(16/9)
       )
```
