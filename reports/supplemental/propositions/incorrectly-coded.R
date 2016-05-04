source("reports/propositions/ambiguous.R")

# ---- incorrectly-coded
# Of the propositions for which people think there is a normatively correct
# response, are there any that were incorrectly coded in the experiment?

table(proposition_classification[, c("correct_response", "norm_response", "agreement")],
      useNA = "ifany")