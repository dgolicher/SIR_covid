# Heuristic modelling of Covid data from Johns Hopkins

Run the app here:

https://dgolicher.shinyapps.io/SIR_Covid/

The shiny app runs by scraping data from 

https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series

Results are then displayed mainly through use of the dygraphs package in R

Adjust the parameters on the SIR tab until the curve closely matches the dauly deaths and the cumulative deaths. Finding a fit **does not** imply that the model is a good one. However it does imply that the mix of parameters are at least plausible if all the other assumptions of the SIR model are approximately met. The most problematic general assumptions in the Covid setting are panmixia and time invariance of model parameters. The lockdown timing will affact the model parameters. However the key period of any model run always  takes place prior to the outbreak being noticed and lockdown being implemented, providing the incidence case mortality is low, as the outbreak is not recognised until deaths and hospital admissions reach a worrying level.  
