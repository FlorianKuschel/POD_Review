## Effektgrößen und Varianzen
POD_Hospital_sex_male <- escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = POD_Hospital_sex_male, append = TRUE)

## Metaanalyse
MA_POD_Hospital_sex_male <- rma(yi, vi, data=POD_Hospital_sex_male)

## Daten nach NA in tpos, tneg, cpos und cneg bereinigen
cleaned_data <- POD_Hospital_sex_male[complete.cases(POD_Hospital_sex_male[, 5:8]), ]

## Metaanalyse mit bereinigten Daten
MA_cleaned_data <- rma(yi, vi, data=cleaned_data)

## Forest Plot erstellen
header_text <- "Author(s) and Year, N, tpos, tneg, cpos, cneg"
slabs <- apply(cleaned_data[, 2:8], 1, function(row) paste(row, collapse = " - "))
forest(MA_cleaned_data, slab = slabs, header = header_text, cex = 0.6, mgp = c(1, 0.5, 0))