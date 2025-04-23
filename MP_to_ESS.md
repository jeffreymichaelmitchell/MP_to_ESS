MP_to_ESS
================
2025-04-22

## Manifesto Project to European Social Survey

This repository is to help students merge the longitudinal Manifesto
Project data to the European Social Survey. It selects the nationalist
parties, and weights the indicators for national way of life,
multiculturalism, and environmentalism by the party size. It also
interpollates the data to make a country-year dataset, then merges the
data to the European Social Survey.

This code is easily adaptable to other indicators in the MP dataset, but
it only matches the country year values to the ESS rounds. There are
much more rigerous ways to match the MP and ESS data, see for example:
<https://github.com/sophieehill/ess-cumulative> and
<https://github.com/denis-cohen/ess-election-dates>

``` r
library(manifestoR)
```

    ## Loading required package: NLP

    ## Loading required package: tm

    ## When publishing work using the Manifesto Corpus, please make sure to cite it correctly and to give the identification number of the corpus version used for your analysis.
    ## 
    ## You can print citation and version information with the function mp_cite().
    ## 
    ## Note that some of the scaling/analysis algorithms provided with this package were conceptually developed by authors referenced in the respective function documentation. Please also reference them when using these algorithms.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ ggplot2::annotate() masks NLP::annotate()
    ## ✖ dplyr::filter()     masks stats::filter()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

This code starts to download the data directly into R

``` r
mp_setapikey("manifesto_apikey.txt")
```

Download the data from the manifesto project into R what it returns is a
dataframe where each row is a country-year-party observation so there
are rows in every group for all parties in that election year, then the
indicator

``` r
mpds<-mp_maindataset(version = "current", south_america =FALSE, download_format = NULL, apikey=NULL, cache = TRUE)
```

    ## Connecting to Manifesto Project DB API... 
    ## Connecting to Manifesto Project DB API... corpus version: 2024-1

This code just fixes the date so that it is a ‘year’ measurement make
the date only 4 digits

``` r
mpds<-mpds %>% 
  mutate(date= (date*.01)) %>% 
  mutate(date=round(date))
```

This selects the MP variables and filters the dataset to all elections
in 1990 or after

Each row is a party in an election in a year in a country.

Here I filter the data so that it only includes parties in countries
with the Nationalist family (parfam 70), that are in the EU and after
1990.

``` r
mpds<-mpds %>% 
  select(country, countryname, date, party, per601, per501, per608, absseat, totseats, eumember, parfam) %>% 
  filter(date> 1990 & parfam== 70 & eumember>1)
```

There is a small issue, Portugal only has one observation of a
Nationalist party. All the other countries have at least 2

``` r
table(mpds$countryname)
```

    ## 
    ##        Austria        Belgium       Bulgaria        Croatia         Cyprus 
    ##             10              9              5              5              5 
    ## Czech Republic        Denmark        Estonia        Finland         France 
    ##              4             10              2              7              6 
    ##        Germany         Greece        Hungary          Italy         Latvia 
    ##              3             13              4             13              8 
    ##      Lithuania    Netherlands         Poland       Portugal        Romania 
    ##              3             11              2              1              3 
    ##         Serbia       Slovakia       Slovenia          Spain         Sweden 
    ##              5             11              5              2              4 
    ##         Turkey 
    ##              7

Lets filter Portugal out

``` r
mpds<- mpds %>% filter(countryname != 'Portugal')
```

This code makes a variable for the share of seats in parlement for each
party in each election

``` r
mpds$seat_share<-(mpds$absseat/mpds$totseats)*100
```

This code makes a variable for each parties’ score on per601 weighted by
their share of parliment

``` r
mpds$per601_size<-mpds$per601*mpds$seat_share
mpds$per608_size<-mpds$per608*mpds$seat_share
mpds$per501_size<-mpds$per501*mpds$seat_share
```

The problem with this data is that the election years don’t match with
the ESS rounds. This means we need to get values for the years when the
ESS was fielded. To do this I expand the dataframe so it includes a full
sequence of years from 1990-2023

Notice I make it into a new data frame called ‘mpdf1’.

``` r
mpds1<- mpds %>% complete(countryname, date= full_seq(date, period= 1), fill= list(per601_size= NA))
```

Then I interpolate the missing values for each year. Basically this is
where the year would fall on a line if you connected the two observed
values it falls between.

For this, you will need to install and load the imputeTS package

``` r
library(imputeTS)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
mpds1<-mpds1 %>% 
  group_by(countryname) %>% 
  mutate(per601_size= imputeTS::na_interpolation(per601_size)) %>% 
  mutate(per608_size= imputeTS::na_interpolation(per608_size)) %>% 
  mutate(per501_size= imputeTS::na_interpolation(per501_size))
```

Check how many country-year observations you have. This code will show
what countries have more than 1 country year observation, but notice it
does NOT save over the dataframe. It shows that there are several
country-year observations with more than one nationalist party.

``` r
mpds1 %>% group_by(countryname, date) %>% 
   filter(n()>1)
```

    ## # A tibble: 61 × 15
    ## # Groups:   countryname, date [27]
    ##    countryname  date country party per601 per501 per608 absseat totseats
    ##    <chr>       <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl>
    ##  1 Austria      2006      42 42420  0      15.1   10.2       21      183
    ##  2 Austria      2006      42 42710  3.58    3.23   4.48       7      183
    ##  3 Austria      2008      42 42420  1.30    4.33   5.84      34      183
    ##  4 Austria      2008      42 42710  0.291   4.07   5.23      21      183
    ##  5 Belgium      2014      21 21710  5.07    3.10   3.60       1      150
    ##  6 Belgium      2014      21 21917  7.47    1.31   5.64       3      150
    ##  7 Croatia      2020      81 81711  6.08    1.97   0         66      151
    ##  8 Croatia      2020      81 81714  6.62    4.30   0         16      151
    ##  9 Cyprus       2016      55 55711  2.96    0      0         18       56
    ## 10 Cyprus       2016      55 55720 16.3     0      0          2       56
    ## # ℹ 51 more rows
    ## # ℹ 6 more variables: eumember <dbl>, parfam <dbl>, seat_share <dbl>,
    ## #   per601_size <dbl>, per608_size <dbl>, per501_size <dbl>

It may be advisable to average the nationalist parties. You would do
this by using something like the following code. It makes a new
variables, like the one called ‘per601_size_mean’

This code also selects only the country, date, and indicator columns.
Then the distinct command removes the duplicate rows.

The result are country-year values of the weighted average of those
values for the nationalist parties in the manifesto project data.

``` r
mpds1<-mpds1%>% group_by(countryname, date) %>% mutate(per601_size_mean= mean(per601_size)) %>%
  mutate(per608_size_mean= mean(per608_size)) %>% 
  mutate(per501_size_mean= mean(per501_size)) %>% 
  select(countryname, date, per601_size_mean, per608_size_mean, per501_size_mean) %>% 
  distinct()
```

Now we can check again if there are any country years with more than 1
observation:

``` r
mpds1 %>% group_by(countryname, date) %>% 
   filter(n()>1)
```

    ## # A tibble: 0 × 5
    ## # Groups:   countryname, date [0]
    ## # ℹ 5 variables: countryname <chr>, date <dbl>, per601_size_mean <dbl>,
    ## #   per608_size_mean <dbl>, per501_size_mean <dbl>

Now that you have a tidy data set for the macro variable you will do the
demeaning procedure.

For this bit of code you will need to install and load the datawizard
package

This line of code makes a new dataframe with meaned (between) and
demeaned (within) values.

``` r
library(datawizard)
```

    ## 
    ## Attaching package: 'datawizard'

    ## The following object is masked from 'package:manifestoR':
    ## 
    ##     rescale

``` r
mpds_demeaned<- demean(mpds1, select = c("per601_size_mean", "per608_size_mean", "per501_size_mean"), by = "countryname")
```

Then load the ESS data and call it ‘ess’

``` r
library(readr)
ess <- read_csv("ESS 1_11.csv")
```

    ## Rows: 530711 Columns: 1637
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (25): name, proddate, cntry, cntbrth, cntbrtha, cntbrthb, cntbrthc, c...
    ## dbl  (1575): essround, edition, idno, dweight, pspwght, pweight, anweight, p...
    ## lgl    (17): prtvtait, prtvtro, prtclait, prtclro, prtmbait, prtmbro, rlgdna...
    ## dttm   (20): inwds, ainws, ainwe, binwe, cinwe, dinwe, einwe, finwe, ginwe, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Load the ess data and call it ‘ess’

This next chunk of code makes a variable in the ess data frame with the
full country names then makes a new year variable corresponding to the
ess round. Then it will give you a table with the number of country-year
observations.

``` r
ess$country_full<- recode(ess$cntry, 
                  "AT"="Austria",
                  "BE"="Belgium",
                  "BG"="Bulgaria",
                  "CH"="Switzerland",
                  "CY"="Cyprus",
                  "CZ"="Czech Republic",
                  "DE"="Germany",
                  "DK"="Denmark",
                  "EE"="Estonia",
                  "ES"="Spain",
                  "FI"="Finland",
                  "FR"="France",
                  "GB"="United Kingdom",
                  "GR"="Greece",
                  "HR"="Croatia",
                  "HU"="Hungary",
                  "IE"="Ireland",
                  "IL"="Israel",
                  "IS"="Iceland",
                  "IT"="Italy",
                  "PL"="Poland",
                  "PT"="Portugal",
                  "LT"="Lithuania",
                  "LU"="Luxembourg",
                  "NL"="Netherlands",
                  "NO"="Norway",
                  "RU"="Russia",
                  "SE"="Sweden",
                  "SI"="Slovenia",
                  "SK"="Slovakia",
                  "TR"="Turkey",
                  "UA"="Ukraine",
                  "LV"="Latvia",
                  "RS"= "Serbia",
                  "ME"= "Montenegro",
                  'XK'= 'Kosovo',
                  'RO'= 'Romania',
                  'MK'= 'North Macedonia',
                  'AL'= 'Albania'

)


#This makes a year variable that corresponds to the ess round
ess$year<-recode(ess$essround,
                "1"="2002",
                "2"="2004",
                "3"="2006",
                "4"="2008",
                "5"="2010",
                "6"="2012",
                "7"="2014",
                "8"="2016",
                "9"="2018",
                "10"="2020",
                "11"="2023")

table(ess$country_full, ess$year)
```

    ##                  
    ##                   2002 2004 2006 2008 2010 2012 2014 2016 2018 2020 2023
    ##   Albania            0    0    0    0    0 1201    0    0    0    0    0
    ##   Austria         2257 2256 2405    0    0    0 1795 2010 2499 2003 2354
    ##   Belgium         1899 1778 1798 1760 1704 1869 1769 1766 1767 1341 1594
    ##   Bulgaria           0    0 1400 2230 2434 2260    0    0 2198 2718    0
    ##   Croatia            0    0    0 1484 1649    0    0    0 1810 1592 1563
    ##   Cyprus             0    0  995 1215 1083 1116    0    0  781  875  685
    ##   Czech Republic  1360 3026    0 2018 2386 2009 2148 2269 2398 2476    0
    ##   Denmark         1506 1487 1505 1610 1576 1650 1502    0 1572    0    0
    ##   Estonia            0 1989 1517 1661 1793 2380 2051 2019 1904 1542    0
    ##   Finland         2000 2022 1896 2195 1878 2197 2087 1925 1755 1577 1563
    ##   France          1503 1806 1986 2073 1728 1968 1917 2070 2010 1977 1771
    ##   Germany         2919 2870 2916 2751 3031 2958 3045 2852 2358 8725 2420
    ##   Greece          2566 2406    0 2072 2715    0    0    0    0 2799 2757
    ##   Hungary         1685 1498 1518 1544 1561 2014 1698 1614 1661 1849 2118
    ##   Iceland            0  579    0    0    0  752    0  880  861  903  842
    ##   Ireland         2046 2286 1800 1764 2576 2628 2390 2757 2216 1770 2017
    ##   Israel          2499    0    0 2490 2294 2508 2562 2557    0 1308    0
    ##   Italy           1207    0    0    0    0  960    0 2626 2745 2640 2865
    ##   Kosovo             0    0    0    0    0 1295    0    0    0    0    0
    ##   Latvia             0    0    0 1980    0    0    0    0  918 1023    0
    ##   Lithuania          0    0    0    0 1677 2109 2250 2122 1835 1659 1365
    ##   Luxembourg      1552 1635    0    0    0    0    0    0    0    0    0
    ##   Montenegro         0    0    0    0    0    0    0    0 1200 1278    0
    ##   Netherlands     2364 1881 1889 1778 1829 1845 1919 1681 1673 1470 1695
    ##   North Macedonia    0    0    0    0    0    0    0    0    0 1429    0
    ##   Norway          2036 1760 1750 1549 1548 1624 1436 1545 1406 1411 1337
    ##   Poland          2110 1716 1721 1619 1751 1898 1615 1694 1500 2065 1442
    ##   Portugal        1511 2052 2222 2367 2150 2151 1265 1270 1055 1838 1373
    ##   Romania            0    0    0 2146    0    0    0    0    0    0    0
    ##   Russia             0    0 2437 2512 2595 2484    0 2430    0    0    0
    ##   Serbia             0    0    0    0    0    0    0    0 2043 1505 1563
    ##   Slovakia           0 1512 1766 1810 1856 1847    0    0 1083 1418 1442
    ##   Slovenia        1519 1442 1476 1286 1403 1257 1224 1307 1318 1252 1248
    ##   Spain           1729 1663 1876 2576 1885 1889 1925 1958 1668 2283 1844
    ##   Sweden          1999 1948 1927 1830 1497 1847 1791 1551 1539 2287 1230
    ##   Switzerland     2040 2141 1804 1819 1506 1493 1532 1525 1542 1523 1384
    ##   Turkey             0 1856    0 2416    0    0    0    0    0    0    0
    ##   Ukraine            0 2031 2002 1845 1931 2178    0    0    0    0    0
    ##   United Kingdom  2052 1897 2394 2352 2422 2286 2264 1959 2204 1149 1684

Then merge the macrodata to the ess file

``` r
ess$year<- as.numeric(ess$year)
ess_merged<- left_join(ess, mpds_demeaned, by= c('country_full'= 'countryname', 'year'='date'))
```

Then here you will need to match the nationalist party family list with
the individual respondents.

Future steps for modeling. We have now set up a macro level dataset that
is able to model the cross sectional country differences (between) in
political rhetoric on three indicators as well as how changes in those
indicators (within) relate to individual level measures in the ESS.

For modeling this you’ll need to install and/or load the lme4 and
lmerTest packages

Then the formula would look something like this m \<-
lmer(dependent_variable ~ x_between + x_within + (1 + x_within \|
country), data = ess_merged)

Then I like to report results using the sjPlot package. There is a ton
of useful stuff in there. For example if you want a nice table of the
results you can use:

tab_model(m)

There are also several plot functions in sjPlot. Here is a link to the
package website: <https://strengejacke.github.io/sjPlot/index.html>
