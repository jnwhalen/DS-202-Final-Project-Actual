README
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Accessing Data

``` r
cm <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/recent-grads.csv")
head(cm)
```

    ##   Rank Major_code                                     Major Total   Men Women
    ## 1    1       2419                     PETROLEUM ENGINEERING  2339  2057   282
    ## 2    2       2416            MINING AND MINERAL ENGINEERING   756   679    77
    ## 3    3       2415                 METALLURGICAL ENGINEERING   856   725   131
    ## 4    4       2417 NAVAL ARCHITECTURE AND MARINE ENGINEERING  1258  1123   135
    ## 5    5       2405                      CHEMICAL ENGINEERING 32260 21239 11021
    ## 6    6       2418                       NUCLEAR ENGINEERING  2573  2200   373
    ##   Major_category ShareWomen Sample_size Employed Full_time Part_time
    ## 1    Engineering  0.1205643          36     1976      1849       270
    ## 2    Engineering  0.1018519           7      640       556       170
    ## 3    Engineering  0.1530374           3      648       558       133
    ## 4    Engineering  0.1073132          16      758      1069       150
    ## 5    Engineering  0.3416305         289    25694     23170      5180
    ## 6    Engineering  0.1449670          17     1857      2038       264
    ##   Full_time_year_round Unemployed Unemployment_rate Median P25th  P75th
    ## 1                 1207         37        0.01838053 110000 95000 125000
    ## 2                  388         85        0.11724138  75000 55000  90000
    ## 3                  340         16        0.02409639  73000 50000 105000
    ## 4                  692         40        0.05012531  70000 43000  80000
    ## 5                16697       1672        0.06109771  65000 50000  75000
    ## 6                 1449        400        0.17722641  65000 50000 102000
    ##   College_jobs Non_college_jobs Low_wage_jobs
    ## 1         1534              364           193
    ## 2          350              257            50
    ## 3          456              176             0
    ## 4          529              102             0
    ## 5        18314             4440           972
    ## 6         1142              657           244

``` r
data("cm")
```

    ## Warning in data("cm"): data set 'cm' not found

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

``` r
#Cleaning
cm <- cm %>%
  filter(!is.na(Total), Total > 0)

cm <- cm %>%
  mutate(
    ShareMen = Men / Total,
    EmploymentRate = Employed / Total,
    UnemploymentRate = Unemployed / (Employed + Unemployed)
  )

head(cm)
```

    ##   Rank Major_code                                     Major Total   Men Women
    ## 1    1       2419                     PETROLEUM ENGINEERING  2339  2057   282
    ## 2    2       2416            MINING AND MINERAL ENGINEERING   756   679    77
    ## 3    3       2415                 METALLURGICAL ENGINEERING   856   725   131
    ## 4    4       2417 NAVAL ARCHITECTURE AND MARINE ENGINEERING  1258  1123   135
    ## 5    5       2405                      CHEMICAL ENGINEERING 32260 21239 11021
    ## 6    6       2418                       NUCLEAR ENGINEERING  2573  2200   373
    ##   Major_category ShareWomen Sample_size Employed Full_time Part_time
    ## 1    Engineering  0.1205643          36     1976      1849       270
    ## 2    Engineering  0.1018519           7      640       556       170
    ## 3    Engineering  0.1530374           3      648       558       133
    ## 4    Engineering  0.1073132          16      758      1069       150
    ## 5    Engineering  0.3416305         289    25694     23170      5180
    ## 6    Engineering  0.1449670          17     1857      2038       264
    ##   Full_time_year_round Unemployed Unemployment_rate Median P25th  P75th
    ## 1                 1207         37        0.01838053 110000 95000 125000
    ## 2                  388         85        0.11724138  75000 55000  90000
    ## 3                  340         16        0.02409639  73000 50000 105000
    ## 4                  692         40        0.05012531  70000 43000  80000
    ## 5                16697       1672        0.06109771  65000 50000  75000
    ## 6                 1449        400        0.17722641  65000 50000 102000
    ##   College_jobs Non_college_jobs Low_wage_jobs  ShareMen EmploymentRate
    ## 1         1534              364           193 0.8794357      0.8448055
    ## 2          350              257            50 0.8981481      0.8465608
    ## 3          456              176             0 0.8469626      0.7570093
    ## 4          529              102             0 0.8926868      0.6025437
    ## 5        18314             4440           972 0.6583695      0.7964662
    ## 6         1142              657           244 0.8550330      0.7217256
    ##   UnemploymentRate
    ## 1       0.01838053
    ## 2       0.11724138
    ## 3       0.02409639
    ## 4       0.05012531
    ## 5       0.06109771
    ## 6       0.17722641

\##Introduction

Often time there are assumptions surrounding college majors. What majors
pay better, what will get you a job, and what majors women tend to
gravitate towards. Being college students, we see stereotypes and
assumptions like this everywhere, but are any of them actually true?
What major should you choose if you want to be paid well and have the
most job security?

\##Data

This is data from the fivethirtyeight package on github. It explores
various college majors, their employment rates, average income, and
percentage of graduates that are women. The data was taken from the
American Community Survey in 2010-2012.

## Questions

What major category has the highest percentage of full time employment
after graduation?

Do major categories with higher percentages of women have higher
employment rates?

Which majors provide the best balance of employment rate and median
salary?

## Results

What major category has the highest percentage of full time employment
after graduation?

``` r
cm <- cm %>%
  mutate(FullTimeShare = Full_time / Employed)

cm <- cm %>%
  filter(!is.na(FullTimeShare), Employed > 0)

category_summary <- cm %>%
  group_by(Major_category) %>%
  summarize(AvgFullTimeShare = mean(FullTimeShare, na.rm = TRUE)) %>%
  arrange(desc(AvgFullTimeShare))

#Average Full time employment by major type
ggplot(category_summary, aes(x = reorder(Major_category, AvgFullTimeShare), y = AvgFullTimeShare, fill = Major_category)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Full-Time Employment Share by Major Category",
    x = "Major Category",
    y = "Average Full-Time Employment Share"
  ) +
  scale_y_continuous(labels = scales::percent_format())
```

![](FINAL_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Faceted by major
ggplot(cm, aes(x = reorder(Major, FullTimeShare), y = FullTimeShare)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Major_category, scales = "free_y") +
  labs(
    title = "Full-Time Employment Share by Major (Faceted by Major Category)",
    x = "Major",
    y = "Full-Time Employment Share"
  ) +
  scale_y_continuous(labels = scales::percent_format())
```

![](FINAL_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#Major colored to major type
ggplot(cm, aes(x = reorder(Major, FullTimeShare), y = FullTimeShare, color = Major_category)) +
  geom_col() +
  labs(
    title = "Full-Time Employment Share by Major",
    x = "Major",
    y = "Full-Time Employment Share"
  ) +
  scale_y_continuous(labels = scales::percent_format())
```

![](FINAL_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

Do major categories with higher percentages of women have higher
employment rates?

``` r
#Filter
cm <- cm %>%
  filter(Total > 0) %>%
  mutate(
    ShareWomen = Women / Total,
    EmploymentRate = Employed / Total
  )

# Aggregate by major category
category_summary <- cm %>%
  group_by(Major_category) %>%
  summarize(
    AvgShareWomen = mean(ShareWomen, na.rm = TRUE),
    AvgEmploymentRate = mean(EmploymentRate, na.rm = TRUE)
  )

# Plot: Share of Women vs Employment Rate
ggplot(category_summary, aes(x = AvgShareWomen, y = AvgEmploymentRate, label = Major_category)) +
  geom_point(, size = 4) +
  geom_text(nudge_y = 0.01, size = 3) +
  labs(
    title = "Women and Employment Rates", 
    x = "Average % Women (per Category)",
    y = "Average Employment Rate"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format())
```

![](FINAL_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(cm, aes(x = ShareWomen, y = EmploymentRate)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Employment Rate vs Share of Women", x = "Share of Women", y = "Employment Rate") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](FINAL_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#Women by Salary
library(scales)

ggplot(cm, aes(x = ShareWomen, y = Median, color = Major_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Median Salary vs Share of Women by Major", x = "Share of Women", y = "Median Salary") +
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels = label_dollar(scale = 0.001, suffix = "K")) + 
  theme(legend.position = "bottom")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](FINAL_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
#Men by Salary

ggplot(cm, aes(x = ShareMen, y = Median, color = Major_category)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Median Salary vs Share of Men by Major", x = "Share of Men", y = "Median Salary") +
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels = label_dollar(scale = 0.001, suffix = "K")) + 
  theme(legend.position = "bottom")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](FINAL_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
#Pie Chart
women_by_category <- cm %>%
  group_by(Major_category) %>%
  summarize(TotalWomen = sum(Women, na.rm = TRUE)) %>%
  mutate(Percent = TotalWomen / sum(TotalWomen)) %>%
  arrange(desc(Percent)) %>%
  mutate(label_pos = cumsum(Percent) - Percent / 2)

women_by_category
```

    ## # A tibble: 16 × 4
    ##    Major_category                      TotalWomen Percent label_pos
    ##    <chr>                                    <int>   <dbl>     <dbl>
    ##  1 Business                                634524 0.163      0.0814
    ##  2 Education                               455603 0.117      0.221 
    ##  3 Humanities & Liberal Arts               440622 0.113      0.336 
    ##  4 Health                                  387713 0.0995     0.443 
    ##  5 Psychology & Social Work                382892 0.0983     0.542 
    ##  6 Social Science                          273132 0.0701     0.626 
    ##  7 Biology & Life Science                  268943 0.0690     0.695 
    ##  8 Communications & Journalism             260680 0.0669     0.763 
    ##  9 Arts                                    222740 0.0572     0.825 
    ## 10 Engineering                             129276 0.0332     0.871 
    ## 11 Industrial Arts & Consumer Services     126011 0.0324     0.903 
    ## 12 Computers & Mathematics                  90283 0.0232     0.931 
    ## 13 Physical Sciences                        90089 0.0231     0.954 
    ## 14 Law & Public Policy                      87978 0.0226     0.977 
    ## 15 Agriculture & Natural Resources          35263 0.00905    0.993 
    ## 16 Interdisciplinary                         9479 0.00243    0.999

``` r
ggplot(women_by_category, aes(x = "", y = TotalWomen, fill = Major_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of Women by Major Category",
    x = NULL,
    y = NULL,
    fill = "Major Category"
  ) +
  theme_void() +
  theme(legend.position = "right")
```

![](FINAL_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
#Male Pie Chart
men_by_category <- cm %>%
  group_by(Major_category) %>%
  summarize(TotalMen = sum(Men, na.rm = TRUE)) %>%
  mutate(Percent = TotalMen / sum(TotalMen)) %>%
  arrange(desc(Percent)) %>%
  mutate(label_pos = cumsum(Percent) - Percent / 2)

men_by_category
```

    ## # A tibble: 16 × 4
    ##    Major_category                      TotalMen  Percent label_pos
    ##    <chr>                                  <int>    <dbl>     <dbl>
    ##  1 Business                              667852 0.232        0.116
    ##  2 Engineering                           408307 0.142        0.303
    ##  3 Humanities & Liberal Arts             272846 0.0949       0.422
    ##  4 Social Science                        256834 0.0893       0.514
    ##  5 Computers & Mathematics               208725 0.0726       0.595
    ##  6 Biology & Life Science                184919 0.0643       0.663
    ##  7 Arts                                  134390 0.0467       0.719
    ##  8 Communications & Journalism           131921 0.0459       0.765
    ##  9 Industrial Arts & Consumer Services   103657 0.0360       0.806
    ## 10 Education                             103526 0.0360       0.842
    ## 11 Psychology & Social Work               98115 0.0341       0.877
    ## 12 Physical Sciences                      95390 0.0332       0.910
    ## 13 Law & Public Policy                    91129 0.0317       0.943
    ## 14 Health                                 75517 0.0263       0.972
    ## 15 Agriculture & Natural Resources        40357 0.0140       0.992
    ## 16 Interdisciplinary                       2817 0.000979     1.00

``` r
ggplot(men_by_category, aes(x = "", y = TotalMen, fill = Major_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of Men by Major Category",
    x = NULL,
    y = NULL,
    fill = "Major Category"
  ) +
  theme_void() +
  theme(legend.position = "right")
```

![](FINAL_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

Which majors provide the best balance of employment rate and median
salary?

``` r
library(dplyr)
library(ggplot2)

# Load and prepare the data
cm <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/recent-grads.csv")

# Create EmploymentRate column
cm <- cm %>%
  mutate(EmploymentRate = Employed / Total) %>%
  filter(!is.na(EmploymentRate), !is.na(Median))

# Rank by combined employment rate and median salary (simple sum for ranking)
cm$Score <- scale(cm$EmploymentRate) + scale(cm$Median)

top_10 <- cm %>%
  arrange(desc(Score)) %>%
  slice(1:10)

# Bar plot
ggplot(top_10, aes(x = reorder(Major, Median), y = Median, fill = Major_category)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Majors with Best Balance of Employment & Salary",
    subtitle = "Ranked by combined employment rate and median salary",
    x = "Major",
    y = "Median Salary (USD)",
    fill = "Major Category"
  )
```

![](FINAL_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(cm, aes(x = EmploymentRate, y = Median, color = Major_category)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = mean(cm$Median, na.rm = TRUE), linetype = "dashed") +
  labs(
    title = "Employment Rate vs. Median Salary by Major",
    x = "Employment Rate",
    y = "Median Salary (USD)",
    color = "Major Category"
  )
```

![](FINAL_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

This bar chart shows the ten college majors that offer the best mix of
earning potential and job security. It ranks majors based on a combined
score of median salary and employment rate, helping highlight which
fields give you the best overall value. Unsurprisingly,
engineering-related majors like Petroleum and Architectural Engineering
top the list. The color coding breaks the majors into categories like
Business, Engineering, or Arts, making it easy to see which areas tend
to perform better. It’s a helpful chart for students thinking about what
major can lead to both a good paycheck and a solid chance of getting
hired.

This scatter plot maps out how all college majors compare in terms of
employment rate and salary. Each dot is a major, with salary on the
vertical axis and employment rate on the horizontal. The dotted line
shows the average salary overall, which makes it easy to spot which
majors are above or below that line. The colors group majors into
categories like Health, Business, Engineering, and so on. It’s a quick
way to see which fields give you the best shot at a job, which pay the
most, and which do both. Engineering majors, for example, usually land
in the top-right — high pay and high employment.

\##Conclusions Conclusions

1.  What major category has the highest percentage of full-time
    employment after graduation?

Our analysis found that engineering and health-related majors tend to
have the highest share of full-time employment after graduation. These
fields consistently lead to stable, full-time positions, likely due to
strong demand and the technical skills associated with these majors.
This suggests that students pursuing these degrees are more likely to
secure reliable employment shortly after completing their studies.

2.  Do major categories with higher percentages of women have higher
    employment rates?

We observed that majors with a higher proportion of women, such as
education and social work, often have lower average employment rates
compared to more male-dominated fields like engineering or computer
science. This doesn’t imply causation, but it may reflect broader labor
market dynamics, such as wage disparities or differences in industry
demand. These findings highlight ongoing gender-related challenges in
the workforce that are worth exploring further.

3.  Which majors provide the best balance of employment rate and median
    salary?

Choosing a college major is one of the most important decisions students
make, and our analysis shows just how much that choice can affect future
job prospects and earning potential. By analyzing employment rates and
median salaries across a wide range of majors, we found that some fields
particularly in engineering and computer science consistently offer both
strong job security and higher pay. On the other hand, many majors in
the arts and humanities tend to offer lower salaries and more limited
job prospects, though they may still provide value in less measurable
ways. Ultimately, while passion and interest should always play a role
in selecting a major, having clear data on economic outcomes can help
students make more informed and confident decisions about their
education and career paths.
