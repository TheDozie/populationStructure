---
title: "ReadMe"
author: "Dozie Ezigbalike"
date: "Sunday, October 26, 2014"
output: html_document
---

## Visualization of population structure using line population pyramids

This is the readme file for the shiny project of the Developing Data Products Coursera Signature Track Course. The assignment is to write a shiny application and deploy on Rstudio's shiny server. However, the deployment to the server posed some problems, possibly because the application needed to read data files from a website; it would launch correctly, but hangs at the read.csv statement.   

I  saved the shiny application as a .zip file on github to be executed with the runUrl() command. But tha did not work either.

Then I discovered that the correct command to run shiny apps deployed to github is runGitHub:


```r
library(shiny)
runGitHub("populationStructure", "TheDozie")
```

```
## Downloading https://github.com/TheDozie/populationStructure/archive/master.tar.gz
```

```
## Warning: downloaded length 15835 != reported length 200
```

```
## Error: invalid octal digit
```

## What does the application do?

### What's a Population Pyramid

- Wikipedia: a graphical illustration that shows the distribution of various age groups in a population (typically that of a country or region of the world)
- Ploted with 5-year age classes on vertical axis and population count or percentages in horizontal bars
- Age-class axis in middle, males to the left, females to the right
- Population counts decrease with increasing age classes
- Therefore the shape is a *pyramid*, thus the name

### Example Population Pyramid

Population Pyramid of Africa in 2014 produced with Minato Nakazawa's pyramid package and data from UN's World Population Prospects.

<div style='text-align: center;'>
    <img height='560' src='http://ecastats.uneca.org/popVis/africa2014.png' />
</div>

### Comparing Population Pyramids

- We sometimes need to compare population pyramids:
+ Different countries or regions at the same time
+ Different epochs for the same country or region
- The only way to compare standard population pyramids is to plot them side by side for visual comparison
- Why not plot with lines that can be overlaid to easily make comparison
- This shiny application does that

### The Application

- The user interface presents a drop down list of all African countries
- A slider input allows two years to be selected
- Output:
+ Population pyramids (line version) for the base year, current year and comparison year (default: 1950, thisYear, 2100)
+ Evolution of broad population classes of children (ages 0-14), adult (15-64) and old (65+)
+ Comparative table of key population facts

<div style='text-align: center;'>
    <img height='200' src='http://ecastats.uneca.org/popVis/popUI.png' />
</div>

### Data and Output

The data are saved as country population files on a website. When a country is selected, the appropriate .csv file is loaded through a reactive statement. So it is only re-loaded when the country is changed.

You need to have installed shiny to run it.

The output looks like this:

<div style='text-align: center;'>
    <img height='300' src='http://ecastats.uneca.org/popVis/popPlots.png' />
</div>
