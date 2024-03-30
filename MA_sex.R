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
data_types <- c("numeric", "text", rep("numeric", 10))

# Import data only for exemplary data for the time being
POD_Hospital_sex_male <- read_POD_Hospital_data("A2:L18", data_types)
POD_Hospital_sex_male <- read_POD_Hospital_data("A2:L18", data_types) %>%
  rename(pval = `p-Wert`) %>%
  mutate(se = sqrt(1/tpos + 1/tneg + 1/cpos + 1/cneg))
POD_Hospital_sex_male[, 5:8] <- round(POD_Hospital_sex_male[, 5:8]) ## da in Excel per Formel berechnet, werden sonst Zahlen mit Nachkommastellen importiert, obwohl eigentlich schon in Excel gerundet

# Sanity check for the p-values: new values estimated via Fisher's exact test
POD_Hospital_sex_male_complete_cases <- POD_Hospital_sex_male %>%
  filter(complete.cases(tpos, tneg, cpos, cneg)) %>%
  rowwise() %>%
  mutate(fisher_result = list(fisher.test(matrix(c(tpos, tneg, cpos, cneg), nrow = 2)))) %>%
  mutate(p_values = broom::tidy(fisher_result)$p.value) %>%
  select(-fisher_result)

# Analysis
POD_Hospital_sex_male <- conv.wald(out=OR, ci.lb=ci.lb, ci.ub=ci.ub, pval=pval, n=n_ges, data=POD_Hospital_sex_male, transf=log)
MA_POD_Hospital_sex_male <- rma(yi, vi, weights = 1/n_ges, data=POD_Hospital_sex_male)

forest <- forest(MA_POD_Hospital_sex_male, #transf=exp, 
                 slab = NA , xlab = "log (OR)", mlab = mlabfun("RE Model for All Studies", MA_POD_Hospital_sex_male), ilab = cbind(POD_Hospital_sex_male$Autor, POD_Hospital_sex_male$Jahr, POD_Hospital_sex_male$n_ges),
                 xlim=c(-10,7), alim=c(-4,4), ylim=c(-5,20), ilab.xpos=c(-6,-5,-3.5), ilab.pos=c(2), at = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
                 cex=0.8, main = "Metaanalyse Geschlecht", header = "Author, Year, n"
)

predict <- predict(MA_POD_Hospital_sex_male, transf=exp, digits=3) ##Ausgabe der OR und CI

funnel <- funnel(MA_POD_Hospital_sex_male)
