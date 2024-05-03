source("packages.R")
source("directories.R")

## Import data
# Custom function to read MS Excel-files
read_POD_Hospital_data <- function(range, col_types, sheet) {
  read_excel(
    path = file.path(data_dir, "POD_Auswertung.xlsx"),
    sheet = "tables factors PoD hospital",
    range = range,
    col_types = col_types
  )
}

# General settings for data import:
data_types <- c("numeric", "text", rep("numeric", 12))

# Import data only for exemplary data for the time being
POD_Hospital_sex_male <- read_POD_Hospital_data("A2:N18", data_types)
POD_Hospital_sex_male <- read_POD_Hospital_data("A2:N18", data_types) %>%
  # rename(pval = `p-Wert`) %>%
  mutate(se = sqrt(1/tpos + 1/tneg + 1/cpos + 1/cneg))
POD_Hospital_sex_male[, 5:8] <- round(POD_Hospital_sex_male[, 5:8]) 

# Sanity check for the p-values: new values estimated via Fisher's exact test
POD_Hospital_sex_male_complete_cases <- POD_Hospital_sex_male %>%
  filter(complete.cases(tpos, tneg, cpos, cneg)) %>%
  rowwise() %>%
  mutate(fisher_result = list(fisher.test(matrix(c(tpos, tneg, cpos, cneg), nrow = 2)))) %>%
  mutate(p_values = broom::tidy(fisher_result)$p.value) %>%
  select(-fisher_result)

# Analysis
POD_Hospital_sex_male <- conv.wald(out=OR, ci.lb=ci.lb, ci.ub=ci.ub, pval=pval, n=n_ges, data=POD_Hospital_sex_male, transf=log)
MA_POD_Hospital_sex_male <- rma.mv(yi, vi, random = ~ 1 | Nummer, mods = ~ hospitalbeds + ltcbeds,
                                     data=POD_Hospital_sex_male)

# Plots
mlabfun <- function(text, MA_POD_Hospital_sex_male) { 
  list(
    paste("\n\n\n\n\n\n\n\n",
      text, "\n",
      "Q = ", formatC(MA_POD_Hospital_sex_male$QE, digits = 2, format = "f"), "\n",
      "σ^2 = ", formatC(MA_POD_Hospital_sex_male$sigma2, digits = 2, format = "f"), "\n", 
      "p ", metafor:::.pval(MA_POD_Hospital_sex_male$pval, digits = 2, showeq = TRUE, sep = " "), "\n",
      "df = ", MA_POD_Hospital_sex_male$k - MA_POD_Hospital_sex_male$p
    )
  )
}


forest_sex <- forest(MA_POD_Hospital_sex_male, atransf=exp, addfit=TRUE, addpred=TRUE,
                 slab = NA , xlab = "OR", mlab = mlabfun("RE Model for All Studies", MA_POD_Hospital_sex_male), ilab = cbind(POD_Hospital_sex_male$Autor, POD_Hospital_sex_male$Jahr, POD_Hospital_sex_male$n_ges),
                 xlim=c(-10,7), alim=c(-4,4), ylim=c(-5,20), ilab.xpos=c(-6,-5,-3.5), ilab.pos=c(2), at = (log(c(0.025, 0.25, 1, 4, 40))),
                 cex=0.8, main = "Metaanalyse Geschlecht", header = "Author, Year, n"
)

funnel_sex <- funnel(MA_POD_Hospital_sex_male)

predict_sex <- predict(MA_POD_Hospital_sex_male, transf=exp, addx = TRUE, digits=3) 

predict(MA_POD_Hospital_sex_male, #newmods = c(4.45,37.75), 
        transf=exp, addx = TRUE, digits=3) 
