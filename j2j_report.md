# J2J Report
Lars Vilhuber  
March 8, 2017  



## A toy news release

This is a draft news release for a Census Bureau product called the Job-to-Job (J2J) Flows. It plays around with dynamic text and just basic replicability.

### Setting parameters
First, we need to set some parameters and get the data (if this were a Shiny webapp, that might be a pulldown menu, in particular for the state). 


```r
j2j.vintage <- "R2016Q3"
# j2jtype <- "j2j"
j2j.type <- "j2jr"
j2j.seas <- "s"
j2j.state <- "us"
# the data area
j2j.urlbase <- "https://lehd.ces.census.gov/data/j2j"
j2j.url <- paste(j2j.urlbase,j2j.vintage,j2j.type,j2j.state,sep = "/")
j2j.file <- paste(j2j.type,j2j.state,"all.csv.gz",sep="_")
# select all sorts of demo and firm vars
# no testing if they are valid!
j2j.industry <- "00" 
j2j.sex <- "0"
j2j.agegrp <- "A00"
j2j.race <- "A0"
j2j.ethnicity <- "A0"
j2j.education <- "E0"
j2j.firmage <- "0"
j2j.firmsize <- "0"
# or we could simply select the agg_level
# this is the same as all of the above
j2j.agg_level <- "1"
j2j.plotvar <- "EEHireR"
# and source statement
j2j.source <- paste("U.S. Census Bureau, Job-to-Job Flows, Release",j2j.vintage,sep = " ")
# our preferred format for pct numbers
j2j.fmtpct <- "%1.1f"
```

We are going to work with seasonally adjusted rates from the R2016Q3 release.

### Getting the data
Getting the data implies downloading the file "j2jr_us_all.csv.gz" from https://lehd.ces.census.gov/data/j2j/R2016Q3/j2jr/us.

```r
# we need the full data
conr <- gzcon(url(paste(j2j.url,j2j.file,sep="/")))
txt <- readLines(conr)
j2jdata <- read.csv(textConnection(txt))
j2jdata$yqstr <- paste(j2jdata$year,j2jdata$quarter,sep="Q")
j2jdata$linyq <- j2jdata$year + (j2jdata$quarter-1)/4
# we also need some metadata. for later.
```

## Doing stuff
We now have the data and can do stuff with it.

### The graph
Now we simply plot this:

#### Code for the graph

#### The actual graph
![](j2j_report_files/figure-html/figure1-1.png)<!-- -->

### The text
Now we can reference some of the data as well.

#### Preparing the text

```r
j2j.lyq <- max(analysis$yqstr)
j2j.ly <- subset(analysis,yqstr == j2j.lyq)[1,"year"]
j2j.lq <- subset(analysis,yqstr == j2j.lyq)[1,"quarter"]
j2j.linyq <- j2j.ly + (j2j.lq -1)/4

j2j.refword <- function(cardinal) {
  if ( cardinal == 1 ) tmpsuffix <- "first"
  if ( cardinal == 2 ) tmpsuffix <- "second"
  if ( cardinal == 3 ) tmpsuffix <- "third"
  if ( cardinal == 4 ) tmpsuffix <- "fourth"
  return(tmpsuffix)
}

j2j.compare <- function(this,last) {
  comp <- (this - last)/last * 100
  if ( comp > 0 ) word <- "grew"
  if ( comp < 0 ) word <- "shrunk"
  # 1 percent difference in numbers is unchanged
  if ( abs(comp) < 1 ) word <- "were largely unchanged"
  return(word)
}
# pick our poison
j2j.current <- subset(analysis,linyq==j2j.linyq)
j2j.prevq <- subset(analysis,linyq==j2j.linyq - 0.25)
j2j.prevy <- subset(analysis,linyq==j2j.linyq - 1)
```

## The actual text
The fraction of workers changing jobs 
grew
in the third quarter of 2015,
with 3.9% changing employers this quarter, 
compared to 
3.6% 
in the third quarter of 2014.
Flows into employment
were largely unchanged, 
with 7.0% 
employed on the last day of the quarter who did not hold a job on the first day of the quarter,
compared to 7.0% a year ago.
Separations to non-employment 
grew, 
with 6.7% 
employed on the first day of the quarter and no longer employed on the last day of the quarter,
compared to 6.4% a year ago.


