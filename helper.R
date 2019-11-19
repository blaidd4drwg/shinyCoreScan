makeTidyData <- function(tmppath, counter, n) {
  tmp <- read_csv(tmppath, quote = "") %>%
    distinct()
  if (!str_detect((names(tmp)[1]), "Spectrum"))
    stop("No valid Avaatech XRF baxil batch file (csv)")
  names(tmp) <- names(tmp) %>%
    str_replace_all('\\"', "") %>%
    str_trim()
  
  tmp2 <- str_split_fixed(tmp$Spectrum, "\\!", 17) %>%
    as_tibble()
  names(tmp2) <-
    c(
      "CoreID",
      "unknown1",
      "unknown2",
      "Depth",
      "Date",
      "Time",
      "Duration",
      "Voltage",
      "Current",
      "unknown3",
      "unknown4",
      "Filter",
      "SlitDown",
      "SlitCross",
      "Run",
      "Rep",
      "unknown5"
    )
  
  tmp3 <- bind_cols(tmp2, tmp) %>%
    select(-starts_with("unknown"),
           -Spectrum,
           -`Live time`,
           -`Real time`,
           -Sample,
           -User) %>%
    mutate_at(
      vars(
        Depth,
        Voltage,
        Current,
        SlitDown,
        SlitCross,
        Duration,
        Throughput
      ),
      as.numeric
    ) %>%
    mutate(Depth = Depth - min(Depth) + 1) %>%
    gather(-(CoreID:Throughput), key = "Measure", value = "Value") %>%
    filter(!str_detect(Measure, "Rh")) %>%
    separate(Measure,
             sep = "[\\W]+",
             into = c("Element", "AbsLine", "Stat")) %>%
    spread(Stat, Value)
  setProgress(counter / n)
  tmp3
}
