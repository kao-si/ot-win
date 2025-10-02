
# OT Win - International

# Check and wrangle raw data

library(tidyverse)

# Load Initial Data ####

game <- read_rds("international/Initial-Data_Game.rds") %>%
  select(-c(tie, ledchg)) # remove the two variables as they are mostly missing

season <- read_rds("international/Initial-Data_Season.rds")

# Check Game-Level Data ####

# Redirect output to a text file for processing
sink("Check-Output_Game-Data.txt")

## Logical relation violations ====
cat("**Logical Relation Violations**\n")

cat("\nQuarter scores not equal to total score, Visitor\n")
game %>% filter(vsco_q1 + vsco_q2 + vsco_q3 + vsco_q4 + vsco_ot
                != vsco_tot) %>% select(1, 3:5, vsco_q1:vsco_tot)

cat("\nQuarter scores not equal to total score, Host\n")
game %>% filter(hsco_q1 + hsco_q2 + hsco_q3 + hsco_q4 + hsco_ot
                != hsco_tot) %>% select(1, 3:5, hsco_q1:hsco_tot)

cat("\nVisitor quarter scores not equal to host quarter scores\n")
game %>% filter(vsco_q1 + vsco_q2 + vsco_q3 + vsco_q4 !=
                hsco_q1 + hsco_q2 + hsco_q3 + hsco_q4) %>%
                select(1, 3:5, vsco_q1:vsco_q4, hsco_q1:hsco_q4)

cat("\nVisitor total score equal to host total score\n")
game %>% filter(vsco_tot == hsco_tot) %>% select(1, 3:5, vsco_tot, hsco_tot)

cat("\nNumber of ties in 4q smaller than that in 2m\n")
game %>% filter(tie4q < tie2m) %>% select(1, 3:5, tie4q, tie2m)

cat("\nTied duration in 4q smaller than that in 2m\n")
game %>% filter(tiesc4q < tiesc2m) %>% select(1, 3:5, tiesc4q, tiesc2m)

cat("\nNumber of lead changes in 4q smaller than that in 2m\n")
game %>% filter(ledchg4q < ledchg2m) %>% select(1, 3:5, ledchg4q, ledchg2m)

cat("\nHost lead duration in 4q smaller than that in 2m\n")
game %>% filter(hledsc4q < hledsc2m) %>% select(1, 3:5, hledsc4q, hledsc2m)

## Missing values ====
cat("\n**Missing Values**\n")

# Variables with systematic missing values
col_w_na <- c("att", "tiesc4q", "hledsc4q", "tiesc2m", "hledsc2m",
              "flssco", "lstie", "lsledtie")

# Variables without systematic missing values
col_wo_na <- setdiff(colnames(game), col_w_na)

# Create a function to pick rows with missing values in a specified variable
pickna <- function(var) {
  game %>%
    filter(is.na(!!rlang::sym(var))) %>%
    select(1, 3:5, !!rlang::sym(var))
}

# Parallel function for variables with systematic missing values
pickna2 <- function(var, excl_league) {
  game %>%
    filter(is.na(!!rlang::sym(var)) & !(league %in% excl_league)) %>%
    select(1, 3:5, !!rlang::sym(var))
}

list_excl_league <- setNames(
  c(list(c("acb", "cba")), rep(list("elb"), 7)),
  c("att", "tiesc4q", "hledsc4q", "tiesc2m", "hledsc2m", "flssco", "lstie", "lsledtie")
)

cat("\nRows with missing values in each variable (last column)\n")
map(col_wo_na, pickna)
map2(col_w_na, list_excl_league, pickna2)

## Outliers ====

# Pause output redirection
sink()

# Redirect output to another file and console
sink("Check-Output_Game-Data_Top-Bottom-Values.txt", split = TRUE)

# Create a function to show the top and bottom k values
# for all numeric variables within a league
show_top_bottom <- function(lg_name, k = 10) {
  # helper: pad a vector to length k with NAs (after removing NAs)
  pad_k <- function(x, k) {
    x <- x[!is.na(x)]
    if (length(x) < k) {
      c(x, rep(NA_real_, k - length(x)))
    } else {
      x[seq_len(k)]
    }
  }

  out <- game %>%
    filter(league == lg_name) %>%
    select(where(is.numeric)) %>%
    reframe(
      across(
        everything(),
        list(
          min = ~ pad_k(head(sort(., na.last = NA), k), k),
          max = ~ pad_k(rev(tail(sort(., na.last = NA), k)), k)
        ),
        .names = "{.col}_{.fn}"
      )
    )

  out <- tibble::as_tibble(out)
  cat(paste0("\nTop and bottom ",
  k, " values for numeric variables for league ", lg_name, "\n"))
  print(out, n = Inf, width = Inf)
  invisible(out)
}

lg_names <- unique(game$league)

walk(lg_names, show_top_bottom)

# Stop output redirection
sink()

# Resume redirecting output in the original file
sink("Check-Output_Game-Data.txt", append = TRUE)

cat("\n**Outliers**\n")

# Create a function to pick rows with outliers in a specified variable
pickol <- function(lg_name, var, n1, n2) {
  cat(paste0("\nRows with outliers in ", deparse(substitute(var)),
  " for league ", lg_name, "\n"))
  game %>%
    filter({{ var }} >= n1 & {{ var }} <= n2 & league == lg_name) %>%
    select(1, 3:5, {{ var }})
}

cat("\nRows with outliers in each variable (last column)\n")

# Example code:
# pickol("elb", vsco_q4, 29, 34)

# Stop output redirection
sink()

# Check Season-Level Data ####

# Redirect output to a text file for processing
sink("Check-Output_Season-Data.txt")

## Missing values ====
cat("\n**Missing Values**\n")

# Create a function to pick rows with missing values in a specified variable
pickna_st <- function(var) {
  season %>%
    filter(is.na(!!rlang::sym(var))) %>%
    select(1:3, !!rlang::sym(var))
}

cat("\nRows with missing values in each variable (last column)\n")
map(colnames(season), pickna_st)

## Outliers ====

# Pause output redirection
sink()

# Redirect output to another file and console
sink("Check-Output_Season-Data_Top-Bottom-Values.txt", split = TRUE)

# Create a function to show the top and bottom k values
# for all numeric variables within a league
show_top_bottom_st <- function(lg_name, k = 10) {
  # helper: pad a vector to length k with NAs (after removing NAs)
  pad_k <- function(x, k) {
    x <- x[!is.na(x)]
    if (length(x) < k) {
      c(x, rep(NA_real_, k - length(x)))
    } else {
      x[seq_len(k)]
    }
  }

  out <- season %>%
    filter(league == lg_name) %>%
    select(where(is.numeric)) %>%
    reframe(
      across(
        everything(),
        list(
          min = ~ pad_k(head(sort(., na.last = NA), k), k),
          max = ~ pad_k(rev(tail(sort(., na.last = NA), k)), k)
        ),
        .names = "{.col}_{.fn}"
      )
    )

  out <- tibble::as_tibble(out)
  cat(paste0("\nTop and bottom ",
  k, " values for numeric variables for league ", lg_name, "\n"))
  print(out, n = Inf, width = Inf)
  invisible(out)
}

lg_names_st <- unique(season$league)

walk(lg_names_st, show_top_bottom_st)

# Stop output redirection
sink()

# Resume redirecting output in the original file
sink("Check-Output_Season-Data.txt", append = TRUE)

cat("\n**Outliers**\n")

# Create a function to pick rows with outliers in a specified variable
pickol_st <- function(lg_name, var, n1, n2) {
  cat(paste0("\nRows with outliers in ", deparse(substitute(var)),
  " for league ", lg_name, "\n"))
  season %>%
    filter({{ var }} >= n1 & {{ var }} <= n2 & league == lg_name) %>%
    select(1:3, {{ var }})
}

cat("\nRows with outliers in each variable (last column)\n")

# Example code:
# pickol_st("jbl", wpcsn, 6.7, 6.7)

# Stop output redirection
sink()

# Modify Data ####

## Game-Level Data ====

# Create function for data modification
md <- function(lg_name, datev, vname, var, newval) {
  game[[var]][game$league == lg_name &
              game$date == datev &
              game$visitor == vname] <- newval

  return(game)
}

# Example code:
# game <- md("elb", 20080111, "Efes Pilsen", "att", 3601)

## Season-Level Data ====

# Create function for data modification
md_st <- function(lg_name, sn, tname, var, newval) {
  season[[var]][season$league == lg_name &
                season$season == sn &
                season$team == tname] <- newval

  return(season)
}

# Example code:
# season <- md_st("jbl", 201617, "宇都宮ブレックス", "wpcsn", 76.8)
