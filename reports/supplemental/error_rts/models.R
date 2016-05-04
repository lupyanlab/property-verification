# ---- error-mod
error_mod <- glmer(is_error ~ feat_c * mask_c + (feat_c * mask_c|subj_id),
                   family = binomial, data = question_first)
