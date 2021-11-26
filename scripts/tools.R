.tidy_names <- function(.x) {
  .x %>% rename("Bb" = "Bb (mcg/mL)",
                "C2" = "C2 (mcg/mL)",
                "C4b" = "C4b (mcg/mL)",
                "C5" = "C5 (mcg/mL)",
                "C5a" = "C5a (pg/mL)",
                "Factor D" = "Factor D (mcg/mL)",
                "MBL" = "MBL (mcg/mL)",
                "Factor I" = "Factor I (mcg/mL)",
                "C1q" = "C1q (mcg/mL)",
                "C3" = "C3 (mcg/mL)",
                "C4" = "C4 (mcg/mL)",
                "Factor B" = "Factor B (mcg/mL)",
                "Factor H" = "Factor H (mcg/mL)",
                "C4a" = "C4a (ng/mL)",
                "sC5b-9" = "sC5b-9 (ng/mL)",
                "pheno_type" = "pheno type") 
}

.z_norm <- function(x) {
  # input: gathered; normalize ml, return the data.frame
  cntr <- x %>% filter(pheno_type=="Control")
  m = mean(pull(x, ml), na.rm=TRUE)
  x %>% mutate(ml_norm = (ml - m) / sd(ml, na.rm=TRUE))
}