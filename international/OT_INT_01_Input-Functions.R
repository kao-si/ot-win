
# OT Win - International

# Create functions for data input

library(tidyverse)

# Function for Data Input at Game Level ####

# Create data frame
dat_game <- data.frame(
    league = character(),
    season = integer(),
    date = integer(),
    visitor = character(),
    host = character(),
    start_time = character(),
    att = integer(),
    otnum = integer(),
    type = character(),
    vsco_q1 = integer(),
    vsco_q2 = integer(),
    vsco_q3 = integer(),
    vsco_q4 = integer(),
    vsco_ot = integer(),
    vsco_tot = integer(),
    hsco_q1 = integer(),
    hsco_q2 = integer(),
    hsco_q3 = integer(),
    hsco_q4 = integer(),
    hsco_ot = integer(),
    hsco_tot = integer(),
    tie = integer(),
    ledchg = integer(),
    tie4q = integer(),
    ledchg4q = integer(),
    tiesc4q = numeric(),
    hledsc4q = numeric(),
    hled2m = integer(),
    tie2m = integer(),
    ledchg2m = integer(),
    tiesc2m = numeric(),
    hledsc2m = numeric(),
    flssco = numeric(),
    flsdif = integer(),
    lstie = numeric(),
    lsp3 = integer(),
    lsledtie = numeric(),
    lsv = integer()
)

gadd <- function(
    league,
    season,
    date,
    visitor,
    host,
    start_time,
    att,
    otnum,
    type,
    vsco_q1,
    vsco_q2,
    vsco_q3,
    vsco_q4,
    vsco_ot,
    vsco_tot,
    hsco_q1,
    hsco_q2,
    hsco_q3,
    hsco_q4,
    hsco_ot,
    hsco_tot,
    tie,
    ledchg,
    tie4q,
    ledchg4q,
    tiesc4q,
    hledsc4q,
    hled2m,
    tie2m,
    ledchg2m,
    tiesc2m,
    hledsc2m,
    flssco,
    flsdif,
    lstie,
    lsp3,
    lsledtie,
    lsv
) {
    new_row <- data.frame(
        league = league,
        season = season,
        date = date,
        visitor = visitor,
        host = host,
        start_time = stringr::str_pad(as.character(start_time), width = 4, side = "left", pad = "0"),
        att = att,
        otnum = otnum,
        type = type,
        vsco_q1 = vsco_q1,
        vsco_q2 = vsco_q2,
        vsco_q3 = vsco_q3,
        vsco_q4 = vsco_q4,
        vsco_ot = vsco_ot,
        vsco_tot = vsco_tot,
        hsco_q1 = hsco_q1,
        hsco_q2 = hsco_q2,
        hsco_q3 = hsco_q3,
        hsco_q4 = hsco_q4,
        hsco_ot = hsco_ot,
        hsco_tot = hsco_tot,
        tie = tie,
        ledchg = ledchg,
        tie4q = tie4q,
        ledchg4q = ledchg4q,
        tiesc4q = tiesc4q,
        hledsc4q = hledsc4q,
        hled2m = hled2m,
        tie2m = tie2m,
        ledchg2m = ledchg2m,
        tiesc2m = tiesc2m,
        hledsc2m = hledsc2m,
        flssco = flssco,
        flsdif = flsdif,
        lstie = lstie,
        lsp3 = lsp3,
        lsledtie = lsledtie,
        lsv = lsv
    )
    dat <- bind_rows(dat_game, new_row)
    return(dat)
}

# Function for Data Input at Season-Team Level ####

# Create data frame
dat_st <- data.frame(
    league = character(),
    season = integer(),
    team = character(),
    rksn = character(),
    wpcsn = numeric(),
    howpcsn = numeric(),
    rowpcsn = numeric()
)

stadd <- function(
    league,
    season,
    team,
    rksn,
    wpcsn,
    howpcsn,
    rowpcsn
) {
    new_row <- data.frame(
        league = league,
        season = season,
        team = team,
        rksn = as.character(rksn),
        wpcsn = wpcsn,
        howpcsn = howpcsn,
        rowpcsn = rowpcsn
    )
    dat <- bind_rows(dat_st, new_row)
    return(dat)
}
