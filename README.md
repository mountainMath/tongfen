# tongfen

TongFen (通分) means to convert two fractions to the least common denominator, typically in preparation for further manipulation like addition or subtraction. In English, that's a mouthful and sounds complicated. But in Chinese there is a word for this, TongFen, which makes this process appear very simple.

When working with geospatial datasets we often want to compare data that is given on differnt regions. For example census data and election data. Or data from two different censuses. To properly compare this data we first need to convert it to a common geography. The process to do this is quite analogous to the process of TongFen for fractions, so we appropriate this term to give it a simple name. Using the **tongfen** package, preparing data on disparate geographies for comparison by converting them to a common geography is as easy as typing `tongfen`.

The package offers three basic functions

1) `tongfen_estimate` makes no assumption on the undrelying geographies and returns estimates of the data on the geography of the first dataset.
2) `tongfen_ct` works on Canadian census tracts from different censuses and returns the data on a common geography.
3) `tongfen_cancensus` works on any census geography obtained through the **cancensus** package. It utilizes the Statisitcs Canada correspondence files and gives the most accurate estimate of data across censues.

Not all data can be aggregated in this form, and some data requires different aggregation functions than others. For example, in census data we encounter variables representing simple counts, for example population, that must be added up when joining geographic regions. Averages or percentages require a weighted sum, medians cannot be aggregated but may be approximated. 

In the case of the data originating from the **cancensus** package the aggregation will be done automatically whenever possible, in other cases the user needs to specify how data should be aggregated.
