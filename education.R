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
POD_Hospital_education <- read_POD_Hospital_data("BS2:CD14", data_types)
POD_Hospital_education <- read_POD_Hospital_data("BS2:CD14", data_types) %>%
  #rename(pval = `p-Wert`) %>%
  mutate(se = sqrt(1/tpos + 1/tneg + 1/cpos + 1/cneg))
POD_Hospital_education[, 5:8] <- round(POD_Hospital_education[, 5:8]) 

# Sanity check for the p-values: new values estimated via Fisher's exact test
POD_Hospital_education_complete_cases <- POD_Hospital_education %>%
  filter(complete.cases(tpos, tneg, cpos, cneg)) %>%
  rowwise() %>%
  mutate(fisher_result = list(fisher.test(matrix(c(tpos, tneg, cpos, cneg), nrow = 2)))) %>%
  mutate(p_values = broom::tidy(fisher_result)$p.value) %>%
  select(-fisher_result)

# Analysis
POD_Hospital_education <- conv.wald(out=OR, ci.lb=ci.lb, ci.ub=ci.ub, pval=pval, n=n_ges, data=POD_Hospital_education, transf=log)
MA_POD_Hospital_education <- rma.mv(yi, vi, random = ~ 1 | Nummer, data=POD_Hospital_education)

# Plots# 
mlabfun <- function(text, MA_POD_Hospital_education) { 
  paste(
    "\n\n\n\n\n\n\n\n",
    text, "\n",
    "Q = ", formatC(MA_POD_Hospital_education$QE, digits = 2, format = "f"), "\n",
    "σ^2 = ", formatC(MA_POD_Hospital_education$sigma2, digits = 2, format = "f"), "\n", 
    "p ", metafor:::.pval(MA_POD_Hospital_education$pval, digits = 2, showeq = TRUE, sep = " "), "\n",
    "df = ", MA_POD_Hospital_education$k - MA_POD_Hospital_education$p
  )
}

forest_education <- forest(MA_POD_Hospital_education, atransf=exp,
                 slab = NA , xlab = "OR", mlab = mlabfun("RE Model for All Studies", MA_POD_Hospital_education), ilab = cbind(POD_Hospital_education$Autor, POD_Hospital_education$Jahr, POD_Hospital_education$n_ges),
                 xlim=c(-10,7), alim=c(-4,4), ylim=c(-5,12), ilab.xpos=c(-6.5,-5,-3), ilab.pos=c(2), at = (log(c(0.05, 0.5, 1, 2, 20))),
                 cex=0.8, main = "Metaanalyse Education", header = "Author, Year, n"
)

funnel_education <- funnel(MA_POD_Hospital_education)

predict_education <- predict(MA_POD_Hospital_education, transf=exp, digits=3)
