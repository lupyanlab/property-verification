# Feature Meta





# Discrete question type by experiment


```r
mod_discrete_exp_err <- glmer(is_error ~ feat_c * mask_c * version_c + (1 | version_c/subj_id), data = df, 
    family = binomial)
```

```
##                         logodds      se   ci_lwr  ci_upr z_value   p_value
## feat_c                   0.2329 0.06115  0.11303 0.35273   3.808 0.0001399
## mask_c                   0.1086 0.06115 -0.01129 0.22839   1.775 0.0758520
## version_c               -0.2356 0.14397 -0.51775 0.04661  -1.636 0.1017928
## feat_c:mask_c            0.3256 0.12228  0.08592 0.56526   2.663 0.0077537
## feat_c:mask_c:version_c  0.3731 0.24446 -0.10608 0.85219   1.526 0.1270043
```

# Continuous question type by experiment


```r
mod_imag_err <- glmer(is_error ~ imagery_z * mask_c * version_c + (1 | version_c/subj_id), data = df, 
    family = binomial)
```

```
##                             logodds      se   ci_lwr  ci_upr z_value
## imagery_z                   0.10980 0.03118  0.04870 0.17090   3.522
## mask_c                      0.08294 0.06326 -0.04106 0.20693   1.311
## version_c                  -0.26499 0.14461 -0.54841 0.01844  -1.832
## imagery_z:mask_c            0.13963 0.06240  0.01733 0.26192   2.238
## imagery_z:mask_c:version_c  0.16692 0.12478 -0.07765 0.41149   1.338
##                              p_value
## imagery_z                  0.0004284
## mask_c                     0.1898664
## version_c                  0.0668804
## imagery_z:mask_c           0.0252375
## imagery_z:mask_c:version_c 0.1809993
```


```r
mod_facts_err <- glmer(is_error ~ facts_z * mask_c * version_c + (1 | version_c/subj_id), data = df, 
    family = binomial)
```

```
##                           logodds      se   ci_lwr    ci_upr z_value
## facts_z                   0.16483 0.02866  0.10865  0.221011   5.750
## mask_c                    0.14390 0.06389  0.01869  0.269123   2.252
## version_c                -0.28800 0.14514 -0.57246 -0.003543  -1.984
## facts_z:mask_c           -0.08244 0.05734 -0.19483  0.029942  -1.438
## facts_z:mask_c:version_c -0.11541 0.11468 -0.34017  0.109355  -1.006
##                            p_value
## facts_z                  8.908e-09
## mask_c                   2.429e-02
## version_c                4.721e-02
## facts_z:mask_c           1.505e-01
## facts_z:mask_c:version_c 3.142e-01
```
