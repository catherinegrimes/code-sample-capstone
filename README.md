# code-sample-capstone
This is some of the code from my senior capstone project exploring the effect of a song's release date on its chart success.

cleaning.ipynb: In Python, I compiled, cleaned, and joined the datasets from Spotify and Billboard. I created some new variables for regression, then moved to R.

analysis.R: In R, I tweaked some variables and accounted for idiosyncrasies in the data. I explored a variety of models, eventually deciding on a linear regression of log of weeks on the chart on song release month, which aligned with my expectations from the literature. I created several visualizations to highlight my results and exported my regression tables with the  stargazer package.
