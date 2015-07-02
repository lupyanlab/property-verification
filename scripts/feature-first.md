# Feature First




```r
df <- read.csv("./feature-first/feature-first-final.csv", stringsAsFactors = FALSE)
```

# Descriptive stats

<img src="figure/descriptive1.png" title="plot of chunk descriptive" alt="plot of chunk descriptive" style="display: block; margin: auto;" /><img src="figure/descriptive2.png" title="plot of chunk descriptive" alt="plot of chunk descriptive" style="display: block; margin: auto;" />

# Discrete question type


```
##   mask_type mask_c feat_type feat_c
## 1    nomask   -0.5 nonvisual   -0.5
## 2    nomask   -0.5    visual    0.5
## 3      mask    0.5 nonvisual   -0.5
## 4      mask    0.5    visual    0.5
```

```r
mod_discrete_err <- glmer(is_error ~ feat_c * mask_c + (1 | subj_id), data = df, family = binomial)
```

```
##               logodds      se   ci_lwr ci_upr z_value  p_value
## feat_c         0.1678 0.06498  0.04044 0.2952   2.582 0.009815
## mask_c         0.1073 0.06498 -0.02008 0.2346   1.651 0.098740
## feat_c:mask_c  0.1396 0.12995 -0.11508 0.3943   1.074 0.282647
```

## Compare question types without visual interference


```r
df_nomask <- filter(df, mask_type == "nomask")
```

```
##   mask_type feat_type feat_c
## 1    nomask nonvisual   -0.5
## 2    nomask    visual    0.5
```

```r
mod_nomask_err <- glmer(is_error ~ feat_c + (1 | subj_id), data = df_nomask, family = binomial)
```

```
##        logodds      se   ci_lwr ci_upr z_value p_value
## feat_c 0.09789 0.09321 -0.08479 0.2806    1.05  0.2936
```

## Simple effect of mask on nonvisual questions


```r
df_nonvisual <- filter(df, feat_type == "nonvisual")
```

```
##   mask_type mask_c feat_type
## 1    nomask   -0.5 nonvisual
## 2      mask    0.5 nonvisual
```

```r
mod_nonvis_err <- glmer(is_error ~ mask_c + (1 | subj_id), data = df_nonvisual, family = binomial)
```

```
##        logodds      se ci_lwr ci_upr z_value p_value
## mask_c 0.03709 0.09392 -0.147 0.2212  0.3949  0.6929
```

## Simple effect of mask on visual questions


```r
df_visual <- filter(df, feat_type == "visual")
```

```
##   mask_type mask_c feat_type
## 1    nomask   -0.5    visual
## 2      mask    0.5    visual
```

```r
mod_vis_err <- glmer(is_error ~ mask_c + (1 | subj_id), data = df_visual, family = binomial)
```

```
##        logodds     se   ci_lwr ci_upr z_value p_value
## mask_c  0.1761 0.0888 0.002027 0.3501   1.983 0.04739
```


<img src="figure/mod-discrete-plot.png" title="plot of chunk mod-discrete-plot" alt="plot of chunk mod-discrete-plot" style="display: block; margin: auto;" />

# Continous Question Ratings


```
##    question             count        imagery_mean    facts_mean  
##  Length:538         Min.   : 1.00   Min.   :0.00   Min.   :0.00  
##  Class :character   1st Qu.: 4.00   1st Qu.:1.00   1st Qu.:0.50  
##  Mode  :character   Median : 5.00   Median :1.40   Median :1.00  
##                     Mean   : 5.28   Mean   :1.43   Mean   :1.02  
##                     3rd Qu.: 6.00   3rd Qu.:1.82   3rd Qu.:1.50  
##                     Max.   :12.00   Max.   :3.43   Max.   :3.00  
##  difficulty_mean
##  Min.   :-2.00  
##  1st Qu.:-1.80  
##  Median :-1.50  
##  Mean   :-1.47  
##  3rd Qu.:-1.20  
##  Max.   : 0.50
```

```
##                 imagery_mean facts_mean difficulty_mean
## imagery_mean          1.0000     0.3790          0.3031
## facts_mean            0.3790     1.0000          0.3777
## difficulty_mean       0.3031     0.3777          1.0000
```

## Imagery


```r
mod_imag_err <- glmer(is_error ~ imagery_z * mask_c + (1 | subj_id), data = df, family = binomial)
```

```
##                  logodds      se   ci_lwr ci_upr z_value p_value
## imagery_z        0.04457 0.03329 -0.02068 0.1098  1.3389  0.1806
## mask_c           0.10048 0.06600 -0.02888 0.2298  1.5224  0.1279
## imagery_z:mask_c 0.05557 0.06665 -0.07507 0.1862  0.8337  0.4045
```

### Imagery without mask


```r
df_nomask <- filter(df, mask_type == "nomask")
mod_imag_nomask_err <- glmer(is_error ~ imagery_z + (1 | subj_id), data = df_nomask, family = binomial)
```

```
##           logodds      se   ci_lwr ci_upr z_value p_value
## imagery_z 0.01471 0.04866 -0.08066 0.1101  0.3024  0.7623
```

### Imagery with mask


```r
df_mask <- filter(df, mask_type == "mask")
mod_imag_mask_err <- glmer(is_error ~ imagery_z + (1 | subj_id), data = df_mask, family = binomial)
```

```
##           logodds      se   ci_lwr ci_upr z_value p_value
## imagery_z  0.0723 0.04514 -0.01616 0.1608   1.602  0.1092
```

<img src="figure/mod-imag-plot.png" title="plot of chunk mod-imag-plot" alt="plot of chunk mod-imag-plot" style="display: block; margin: auto;" />

## Facts


```r
mod_facts_err <- glmer(is_error ~ facts_z * mask_c + (1 | subj_id), data = df, family = binomial)
```

```
##                 logodds      se    ci_lwr  ci_upr z_value p_value
## facts_z         0.05476 0.03119 -0.006371 0.11590  1.7557 0.07914
## mask_c          0.11489 0.06586 -0.014199 0.24398  1.7444 0.08109
## facts_z:mask_c -0.02573 0.06243 -0.148091 0.09662 -0.4122 0.68017
```


