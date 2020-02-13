---
title: "Range Voting"
output: 
  html_document: 
    keep_md: yes
---

This package simulates the counting of votes in a costituency where the STAR System (Score Then Automatic Runoff) is used, in the context of Range Voting procedure, in which the electors are supposed to assign an integer score from 0 to 5 to every candidate. Both a function for first-past-the-post system and one for proportional system are included. Moreover, it is possible to create a random sample of votes and to plot the histogram of candidates' scores.

## Dependencies

Please notice that packages _doParallel_ and _scrime_ are imported when installing the package. Packages _plotly_ and _foreach_ are imported and loaded.

## Installation
In order to install and use the package:

```r
devtools::install_github('unimi-dse/1a62ddfc')

require(rangevoting)
```

## Dataset, _ballimp_ and _randomvote_ functions

The package already provides a CSV file containing votes for an imaginary costituency, but it may be substituted with any file created with function _randomvote_. 
```r
N = 100
candidates = c('A','B','C')
parties = c('left','center','right')
randomvote(N,candidates,parties)
```
Function _ballimp_, on the other hand, loads the provided CSV file and transforms it into a data frame with party names as row names and candidates in the first column.
```r
votes = ballimp()
```

## Function _majority_

This function reads a csv file containing votes and computes the winner of that costituency in a first-past-the-post system applying the STAR procedure. Then it updates the counter of total parliament seats won by each party.
In the case of range voting, the winner in a first-past-the-post costituency is the candidate with the highest average score. If two or more candidates have the same average score, then the Automatic Runoff part takes place and the winner among them is the candidate who has the highest number of the highest score.

```r
counter = majority(counter)
```

## Function _proportional_
This function reads a csv file containing votes for each party in a costituency and assigns the number of seats won by each party, updating the counter of total parliament seats per party. The number of seats specific to the costituency must be given as an input.

```r
counter = proportional(seats,counter)
```

## Function _votehist_
This function reads a csv file containing votes for each party in a costituency and plots the grouped histograms of scores obtained by each party.
```r
votehist()
```
