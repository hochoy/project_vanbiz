
readin_tsv <- function(readpath){
  readr::read_tsv(readpath,
                  col_names = T,
                  na = c(""," ","NA",NA),
                  comment = "",
                  trim_ws = T,
                  progress = T)
}

writeout_tsv <- function(target_df,writepath) {
  write.table(target_df,
              file=writepath,
              sep = "\t",
              quote = FALSE,
              row.names = FALSE)
}

play_sound <- function() {
  system("afplay ~/aNALYSIS_PIPELINE/data/metagenomics/MC_other/conscmp1.wav")
}