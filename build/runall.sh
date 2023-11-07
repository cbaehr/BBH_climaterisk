#!/bin/sh
Rscript /Users/christianbaehr/Documents/Github/BBH_climaterisk/build/03_join_sautner_orbis.R &
Rscript /Users/christianbaehr/Documents/Github/BBH_climaterisk/build/03b_join_sautner_orbis_quarterly.R &
wait
Rscript /Users/christianbaehr/Documents/Github/BBH_climaterisk/build/04_merge_w_lobbyview.R &
Rscript /Users/christianbaehr/Documents/Github/BBH_climaterisk/build/04b_merge_w_lobbyview_quarterly.R &
wait
Rscript /Users/christianbaehr/Documents/Github/BBH_climaterisk/build/05_normalize_variables.R