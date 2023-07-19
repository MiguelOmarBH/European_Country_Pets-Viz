European Pets
================
Miguel Omar
2023-07-18

# Introduction

Hello! Wellcome to this small tutorial on how to make a beautiful plot
to visualise data regarding number of pets in different european
countries. As this is my first Notebook in my learning journey, I was
inspired by different online courses, tutorials and blogs to make it.
Anything wouldn’t have been possible without the beautiful R community
users who shares their knowledge.

For this example, we’ll use a mix of a bar and a point chart to
represent the pet rate (dogs & cats) in each european country given
their population. The bars will be filled with colour depending on
whether the pet rate is above or below the mean. The points will be set
as the flag of each country.

Let’s go!

## Hands on

### Data and libraries preparation

First of all, we have to import all we need. Different libraries and the
dataset are imported below (feel free to install any of them if you
didn’t yet):

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggflags)
library(countrycode)
library(ggthemes) # Optional
pets <- readxl::read_xlsx("dogs_cats_europe.xlsx") # The dataset was previously cleaned. Data was obtained from cooperpetcare.com
```

### Data transforming

Once we have our libraries and the dataset imported, let’s have a quick
look:

``` r
print(pets)
```

    ## # A tibble: 29 × 4
    ##    Country           Dogs     Cats Population
    ##    <chr>            <dbl>    <dbl>      <dbl>
    ##  1 Austria         837000  1985000    8859000
    ##  2 Belgium        1340000  2085000   11460000
    ##  3 Bulgaria        750000   815000    7000000
    ##  4 Croatia         350000   437500    4076000
    ##  5 Cyprus           48648    60810     875899
    ##  6 Czech Republic 2205000  1150000   10650000
    ##  7 Denmark         610000   680000    5806000
    ##  8 Estonia         235000   290000    1325000
    ##  9 Finland         760000   975000    5518000
    ## 10 France         7500000 15100000   67060000
    ## # ℹ 19 more rows

As we can see, the dataset has 4 columns: the country, the number of
dogs, the number of cats and the population of each country. To start
working with it we need to perform some transformations. Let’s do it!

``` r
pets_2 <- pets %>% # We create a new dataset, but you can modify the original
  mutate(pet_rate = (Dogs + Cats)/Population*100) # Create a new column to calculate the pet proportion
pets_2$pet_rate_z <- round((pets_2$pet_rate - mean(pets_2$pet_rate))/sd(pets_2$pet_rate), 2) # We create a new variable with the Z-Score. Z-Score has mean = 0, so you'll understand the next step
pets_2$pet_mean <- ifelse(pets_2$pet_rate_z < 0, "below", "above") # With the Z-Score, we set a new column to tag each country regarding whether its pet rate is above or below the mean
pets_2 <- pets_2[order(pets_2$pet_rate_z), ] # Ordering the dataset by Z-Score
pets_2$Country <- factor(pets_2$Country, levels = pets_2$Country) # Converting the Country column to a factor to maintain the order while plotting
pets_2$iso <- tolower(countrycode(pets_2$Country, origin = "country.name", destination = 'iso2c')) # Creating a new variable with the ISO codes of each country. This step is important to create the flag points
```

Now, we’ve performed all the variables and transformations we need.
Let’s have a quick view to make sure everything is correct:

``` r
print(pets_2)
```

    ## # A tibble: 29 × 8
    ##    Country      Dogs    Cats Population pet_rate pet_rate_z pet_mean iso  
    ##    <fct>       <dbl>   <dbl>      <dbl>    <dbl>      <dbl> <chr>    <chr>
    ##  1 Greece     660000  605000   10720000     11.8      -1.67 below    gr   
    ##  2 Cyprus      48648   60810     875899     12.5      -1.6  below    cy   
    ##  3 Ireland    457000  326000    4904000     16.0      -1.3  below    ie   
    ##  4 Croatia    350000  437500    4076000     19.3      -1    below    hr   
    ##  5 Luxembourg  55500   69375     613894     20.3      -0.91 below    lu   
    ##  6 Denmark    610000  680000    5806000     22.2      -0.74 below    dk   
    ##  7 Bulgaria   750000  815000    7000000     22.4      -0.73 below    bg   
    ##  8 Sweden     890000 1480000   10230000     23.2      -0.66 below    se   
    ##  9 Norway     490000  783000    5328000     23.9      -0.6  below    no   
    ## 10 Slovakia   920000  550000    5450000     27.0      -0.32 below    sk   
    ## # ℹ 19 more rows

At this point, we’ll have all our data prepared for plotting!

### Plotting

The last step of this project is to make a plot. We can just make a
simple bar chart to represent our data as follows:

``` r
ggplot(data = pets_2) +
  geom_col(mapping = aes(x = pet_rate, y = Country), fill = "blue")
```

![](Europe_Pets_Markdown_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

This is a simple way to represent our data. But we want to make it look
even better!

We’ll create a bar style chart, but adding some modifications:

``` r
pets_plot <- ggplot(data = pets_2, aes(x = pet_rate, y = Country)) + # Calling the ggplot and assign it to a variable, choosing our x and y axes
  geom_bar(stat = "identity", aes(fill= pet_mean), width = .5) + # Choosing the bar plot, coloured by our pet_mean status (above or below)
  scale_fill_manual(name = "",
                    labels = c("Above average", "Below average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + # Here we set a color to each pet_mean status, so above the mean will be green and below red
  geom_point() +
  geom_flag(x=pets_2$pet_rate, y = pets_2$Country, aes(country = iso), size = 7) + # With the geom_point we call the flag geom. Same x and y axes, and in the aestethics we assign our ISO variable to country to generate our flags
  labs(title="Which european country has the highest pet rate?",
       subtitle = "Number of cats/dogs per 100 person",
       caption = "Source: cooperpetcare.com",
       x="Pet Proportion",
       y="") + # Some labeling for title, axes, etc
  theme_economist() + # This is optional, you can choose your favourite theme!
  scale_x_continuous(breaks=seq(0,70,5)) # Here we select our x axis intervals, from 0 to 70 in steps of 5
```

With that, we would have our plot finished. Now we can print it to see
how it looks like:

``` r
print(pets_plot)
```

![](Europe_Pets_Markdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Fantastic! Our code worked and our plot is ready to save and share! At a
glance we can see which Europe countries have the highest and lowest
amount of pets per 100 person, and which ones are above and below the EU
mean. And all that with a really beautiful viz!

## Conclusion

Here we’ve showed a way to visualise data with a beautiful aestethic. I
tried to apply what I’m learning to share it with the community in order
to help users and (of course) have recommendations of everyone who wants
to make any comments or suggestions. This is my really first Notebook,
as I recently started learning data analysis with R. I’m sure that my
code could have some improvable things, or that anything could be made
in a more efficient way. Feel free to share any comments, they’re all
wellcome!!
