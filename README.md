# Spatial mobility and personal networks across settlement types
[![DOI](https://zenodo.org/badge/1443185758.svg)](https://doi.org/10.5281/zenodo.23889985)

R code for the article *Spatial Mobility and Personal Networks Across Settlement Types*, accepted for publication in the *Journal of Transport Geography*.

The study uses call detail records (CDRs) from Estonia, February 2018, to examine how people's spatial mobility and the structure of their communication networks differ between big cities, small cities and rural areas, at the level of individuals and of the ties between them.

## Data

The CDR data were provided by a mobile network operator under a data agreement and **cannot be shared**. The code is published so that every processing and analysis step can be read and checked; it cannot be run without the data.

The raw extract has one row per outbound communication event with:

- an anonymised sender ID and receiver ID (`op_pos_usr_id`, `op_pos_usr_id_rec`)
- a timestamp (`pos_time`)
- the serving cell tower site (`site_id`) and its coordinates (`lon`, `lat`)

Settlement, municipality and county boundaries and the population grid come from the Estonian Land Board and are publicly available.

## Terminology

The extract contains voice calls and text messages, which are not distinguished. In the code, **call** refers to any communication event, **caller** to the sender and **callee** to the receiver. Locations are recorded for senders only.

Settlement types are coded as **B** (big city: Tallinn, Tartu, Narva, Pärnu, Kohtla-Järve, Jõhvi), **S** (small city) and **R** (rural). A user's dominant settlement type (`cg_domset`) is the type from which they send most of their communication events.

Variable prefixes: `sn_` social network measures, `sm_` spatial mobility measures, `cg_` settlement context.

## Repository structure

```
scripts/               the analysis pipeline, numbered in the order it runs
development-archive/   scripts written while the analysis was developed;
                       not part of the final pipeline
```

## Pipeline

The scripts run in sequence within **one R session**: each script uses objects created by the earlier ones. Lines that save or load intermediate objects (`saveRDS()` / `readRDS()`) are included as comments, for starting from a saved copy instead.

| Script | Step | Main output | In the article |
|---|---|---|---|
| 01 | Spatial data preparation: settlement types from EHAK codes | `settlements` | |
| 02 | CDR data cleaning; communication events assigned to settlements | `mob_data` | |
| 03 | Network data preparation: technical checks, removal of non-personal accounts, internal users, tie reliability | `edges_pass` | |
| 04 | Network construction: weighted edge lists and graphs | `g_dir`, `g_und` | |
| 05 | Social network measures | `user_network_metrics` | Table 1 |
| 06 | Spatial mobility measures | `user_mobility_metrics` | Table 1 |
| 07 | Settlement context per user, including `cg_domset` | `user_contextual_metrics` | Table 1 |
| 08 | Final analysis dataset (one row per user) | `analysis_data` | |
| 09 | Graphs restricted to the analysis users | `g_*_induced` | |
| 10 | Descriptive network statistics | printed statistics | |
| 11 | Variable dictionary and labels | `var_labels` | |
| 12 | Exploratory analysis | diagnostics | |
| 13 | Settlement composition of contacts | figure | |
| 14 | Individual-level analysis | result tables | Tables 2–4, Figure 5, Tables S1–S11 |
| 15 | Connection-level analysis | result tables | Table 5, Tables S12–S18 |
| 16 | Dominant settlement per user (single settlement) | `users_settl_dom` | |
| 17 | Big-city robustness check: Tallinn and the other big cities | `city_medians` | Table S19 |
| 18 | Cell tower density by settlement type | density tables | |

Each script starts with a header describing its purpose, inputs and outputs.

## Requirements

R (4.x) with the packages:

`tidyverse` (including `dplyr`, `tidyr`, `purrr`, `lubridate`, `ggplot2`), `data.table`, `sf`, `igraph`, `broom`, `mgcv`, `scales`, `patchwork`, `knitr`

The full dataset (over 20 million communication events) needs a machine with ample memory; betweenness and the daily mobility measures are slow to compute.

## Development archive

`development-archive/` keeps the scripts written while the analysis took shape: earlier versions of the network filters and measures, exploratory analyses for earlier formulations of the research questions, a user-profiling (clustering) design that was later dropped, figure drafts, and checks explored during review. Each file starts with a short note on what it contains. They are kept as a record and are not needed to reproduce the results.
