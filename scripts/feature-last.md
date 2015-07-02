# Feature Last




```r
df <- read.csv("./feature-last/feature-last-final.csv", stringsAsFactors = FALSE)
```

# Descriptive stats

<img src="figure/descriptive1.png" title="plot of chunk descriptive" alt="plot of chunk descriptive" style="display: block; margin: auto;" /><img src="figure/descriptive2.png" title="plot of chunk descriptive" alt="plot of chunk descriptive" style="display: block; margin: auto;" />


```r
df <- filter(df, subj_id != "MWPF214")
```

# Discrete question type

## Summary


```
## Source: local data frame [4 x 3]
## Groups: feat_type
## 
##   feat_type mask_type is_error
## 1 nonvisual      mask  0.06944
## 2 nonvisual    nomask  0.08004
## 3    visual      mask  0.11479
## 4    visual    nomask  0.08256
```

## Models


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
##               logodds     se   ci_lwr ci_upr z_value  p_value
## feat_c         0.2966 0.1039  0.09288 0.5003   2.854 0.004323
## mask_c         0.1092 0.1039 -0.09446 0.3130   1.051 0.293192
## feat_c:mask_c  0.5090 0.2078  0.10169 0.9164   2.449 0.014317
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
##        logodds     se  ci_lwr ci_upr z_value p_value
## feat_c 0.04175 0.1491 -0.2505  0.334    0.28  0.7794
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
##        logodds     se  ci_lwr ci_upr z_value p_value
## mask_c -0.1452 0.1553 -0.4496 0.1592 -0.9351  0.3498
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
##        logodds     se  ci_lwr ci_upr z_value  p_value
## mask_c  0.3645 0.1382 0.09349 0.6354   2.636 0.008384
```

## Plot

<img src="figure/plot-discrete.png" title="plot of chunk plot-discrete" alt="plot of chunk plot-discrete" style="display: block; margin: auto;" />

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
##                  logodds      se   ci_lwr ci_upr z_value   p_value
## imagery_z        0.17476 0.05279  0.07130 0.2782  3.3106 0.0009311
## mask_c           0.06508 0.10829 -0.14717 0.2773  0.6009 0.5478850
## imagery_z:mask_c 0.22182 0.10561  0.01483 0.4288  2.1004 0.0356952
```

### Imagery without mask


```r
df_nomask <- filter(df, mask_type == "nomask")
mod_imag_nomask_err <- glmer(is_error ~ imagery_z + (1 | subj_id), data = df_nomask, family = binomial)
```

```
##           logodds      se   ci_lwr ci_upr z_value p_value
## imagery_z 0.06425 0.07723 -0.08711 0.2156  0.8319  0.4054
```

### Imagery with mask


```r
df_mask <- filter(df, mask_type == "mask")
mod_imag_mask_err <- glmer(is_error ~ imagery_z + (1 | subj_id), data = df_mask, family = binomial)
```

```
##           logodds      se ci_lwr ci_upr z_value   p_value
## imagery_z  0.2855 0.07182 0.1448 0.4263   3.976 7.004e-05
```

<img src="figure/mod-imag-plot.png" title="plot of chunk mod-imag-plot" alt="plot of chunk mod-imag-plot" style="display: block; margin: auto;" />

## Facts


```r
mod_facts_err <- glmer(is_error ~ facts_z * mask_c + (1 | subj_id), data = df, family = binomial)
```

```
##                logodds      se   ci_lwr  ci_upr z_value   p_value
## facts_z         0.2746 0.04823  0.18003 0.36908   5.693 1.250e-08
## mask_c          0.1723 0.10988 -0.04309 0.38761   1.568 1.169e-01
## facts_z:mask_c -0.1383 0.09646 -0.32734 0.05078  -1.434 1.517e-01
```
