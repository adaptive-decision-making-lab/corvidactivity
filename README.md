
# Less activity means improved welfare? How pair housing influences pinyon jay (_Gymnorhinus cyanocephalus_) behavior

-   Created on 2024-02-05 by Jeffrey R. Stevens
    (<jeffrey.r.stevens@gmail.com>)
-   Finalized on 2024-06-04

This repository provides the reproducible research materials for our project that investigates how moving from individual to pair housing influences activity and behavior in pinyon jays (materials available at [Open Science Framework](https://osf.io/v9r6q/)). This includes the following:

-   Data
-   R script for data analysis
-   R Markdown file for the manuscript

## Citation

If you use any of these materials, please cite:

Wolff, L.M., & Stevens, J.R. (forthcoming). Less activity means improved welfare? How pair housing influences pinyon jay (_Gymnorhinus cyanocephalus_) behavior. _Animal Welfare_. https://doi.org/10.1101/2024.02.07.579343

## Summary

This study recorded 74 15-minute samples of 12 captive pinyon jays (Gymnorhinus cyanocephalus) in their home rooms in the Avian Cognition Lab at the University of Nebraska-Lincoln over a three week period in Feb-Mar 2021. For the first week, the birds were housed singly. At the beginning of the second week, the bird were moved to pair housing. From the video, we both estimated bird activity levels from pixel changes in the videos and directly measured 16 behaviors of the birds. The first data file contains activity level data with the mean pixel change for each of the 74 recordings (each row representing data from one recording). The second data file contains the frequencies of the 16 behaviors for each 10-second video sample for each pair in each recording. It includes both the full set of scored behavioral data for the pre- and post-move recordings and a subset of recordings used to calculate the inter-rater reliability (each row representing data from one sample for one pair in one recording).

## License

All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any     reasonable manner, but not in any way that suggests the licensor endorses you or your use.

No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

## Files

### Data files

`wolff_stevens_2024_data1.csv`

-   date - date of recording
-   timeofday - time of day of recording
-   phase - Study phase (Pre-move, During-move, or Post-move)
-   activity - Mean number of pixel changes between frames for one recording

`wolff_stevens_2024_data2.csv`

-   id - sample ID (each recording has 10 samples for each pair)
-   video_number - video recording number
-   date - date of recording
-   minute - minute of recording
-   bird_pair - names of birds in pair
-   initials - initials of video coder
-   feeding - number of feeding events in a sample
-   drinking - number of drinking events in a sample
-   foraging - number of foraging events in a sample
-   play - number of playing events in a sample
-   standing - number of standing events in a sample
-   walking - number of walking events in a sample
-   jumping - number of jumping events in a sample
-   perching - number of perching events in a sample
-   route_tracing - number of route-tracing events in a sample
-   head_thru_bar - number of head-thru-bar events in a sample
-   beak_scraping - number of beak-scraping events in a sample
-   pecking - number of pecking events in a sample
-   laying_down - number of laying-down events in a sample
-   preening - number of preening events in a sample
-   flapping - number of flapping events in a sample
-   hopping - number of hopping events in a sample

### R code
 
`wolff_stevens_2024_rcode.R` - code for running computations and generating figures

### R Markdown documents

`wolff_stevens_2024.Rmd` - R Markdown document with R code embedded for main manuscript and appendix

### Installation

To reproduce these results, first clone or unzip the Git repository into a folder. Then, ensure that a subfolder named “figures” is in the folder. Next, open `wolff_stevens_2024_rcode.R` in [RStudio](https://rstudio.com) or another R interface and ensure that all packages mentioned at the top of the script are installed. Once all packages are installed, run the script in R using `source("wolff_stevens_2024_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown document `wolff_stevens_2024.Rmd.` Open this file in RStudio and ensure that you have packages [{knitr}](https://yihui.org/knitr/) and [{rmarkdown}](https://rmarkdown.rstudio.com/) installed. Once installed, use {knitr} to render the document (control-shift-K). 

# Dataset Metadata
The following table is necessary for this dataset to be indexed by search
engines such as <a href="https://g.co/datasetsearch">Google Dataset Search</a>.
<div itemscope itemtype="http://schema.org/Dataset">
<table>
<tr>
<th>property</th>
<th>value</th>
</tr>
<tr>
<td>name</td>
<td><code itemprop="name">Pinyon jay activity and housing dataset</code></td>
</tr>
<tr>
<td>description</td>
<td><code itemprop="description">The dataset from the paper <a href="https://doi.org/10.1101/2024.02.07.579343">Less activity means improved welfare? How pair housing influences pinyon jay (<i>Gymnorhinus cyanocephalus</i>) behavior</a>. This study recorded 74 15-minute samples of 12 captive pinyon jays (<i>Gymnorhinus cyanocephalus</i>) in their home rooms in the Avian Cognition Lab at the University of Nebraska-Lincoln over a three week period in Feb-Mar 2021. For the first week, the birds were housed singly. At the beginning of the second week, the bird were moved to pair housing. From the video, we both estimated bird activity levels from pixel changes in the videos and directly measured 16 behaviors of the birds. The first data file contains activity level data with the mean pixel change for each of the 74 recordings (each row representing data from one recording). The second data file contains the frequencies of the 16 behaviors for each 10-second video sample for each pair in each recording. It includes both the full set of scored behavioral data for the pre- and post-move recordings and a subset of recordings used to calculate the inter-rater reliability (each row representing data from one sample for one pair in one recording).</code></td>
</tr>
</tr>
<tr>
<td>url</td>
<td><code itemprop="url">https://github.com/adaptive-decision-making-lab/corvidactivity</code></td>
</tr>
<tr>
<td>sameAs</td>
<td><code itemprop="sameAs">https://github.com/adaptive-decision-making-lab/corvidactivity</code></td>
</tr>
<tr>
<td>citation</td>
<td><code itemprop="citation">https://doi.org/10.1101/2024.02.07.579343</code></td>
</tr>
<tr>
<td>license</td>
<td>
<div itemscope itemtype="http://schema.org/CreativeWork" itemprop="license">
<table>
<tr>
<th>property</th>
<th>value</th>
</tr>
<tr>
<td>name</td>
<td><code itemprop="name">CC BY-SA 4.0</code></td>
</tr>
<tr>
<td>url</td>
<td><code itemprop="url">https://creativecommons.org/licenses/by-sa/4.0/</code></td>
</tr>
</table>
</div>
</td>
</tr>
</table>
</div>
