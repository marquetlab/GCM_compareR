
#### GCM_compareR

`GCM_compareR` is a web application developed to assist ecologists,
conservationists and policy makers at understanding climate change
scenarios and differences between General Circulation Models (GCMs), and
at assisting the triage of subsets of models in an objective and
informed manner. GCM compareR is written in R and uses the web app
development package Shiny. This repository contains the development version of the software and web application.

General Circulation Models (GCMs) are commonly used for exploring scenarios of climate change.
Currently, scientists can chose from a large number of GCMs, as
meteorological research centers worldwide have contributed more than 35
different GCMs for four distinct climate change scenarios as part of the
Coupled Model Intercomparison Project Phase 5 (CMIP5; (Taylor, Stouffer,
and Meehl 2012)). Projections of future climate from all these models
tell a common story, but the spread among them is also significant
(Zappa and Shepherd 2017), which is an indicator of the irreducible
uncertainty concerning any unverifiable future projection. For this
reason, studies have shown that the choice of GCMs in modeling studies
is an important source of variability in model outputs (Thuiller et al.
2019). The situation demands for workflows to help researchers exploring
climate change scenarios to increase objectivity and repeatability in
research and assure a well judged treatment of uncertainty (Shepherd et
al. 2018).

`GCM_compareR` has been developed to play this role in helping researchers
approaching GCMs in climate change studies and assist the selection of
climate models. The app offers quick access to preloaded CMIP5
downscaled GCMs for the four RCPs (Vuuren et al. 2011) and allows users
to compare their projections for future years. Comparison results are
provided as scatterplots and maps where users may learn what makes
different any GCM, identify groups of GCMs with similar characteristics
(e.g. “colder” or “warmer” in their projection of temperature increase)
and define storylines about the future climate (Zappa and Shepherd
2017).

-----

#### Lastest news

>   - New publication usable for citation (Jan 2020)

>   - Release of `GCM compareR` (Sep 26, 2018)

-----

#### Use of the App

`GCM_compareR` compareR contains tabs that might be used from left to right to
define a comparison scenario, retrieve results and generate a report
with them.

  - The *Intro* tab includes all the information needed to use the app.
    Move to the *Workflow* section to find full details about how to use
    the app, and go to *About* to find information about developers.  
  - In the *Scenario* tab you will be able to set up a comparison
    scenario by making all choices: select the GCMs you would like to
    compare, pick a climate change scenario (year of projection, RCP…)
    and set the geographic extent of your analysis. Use the *Analyse*
    button on this tab to trigger the start of the analyses.  
  - The tabs *Explore selected GCMs*, *Variation from present* and
    *Variation among futures* will display the results after the
    calculation is completed. Finally, *Report* will download a report
    with all the figures produced and some explanatory text.

-----

#### Citation

Please, if you use `GCM_compareR` as part of your research, cite the app
as:

> Fajardo, J, Corcoran, D., Roehrdanz, P, Hannah, P, Marquet, P (in
> press) GCM compareR: A web application to assess differences and
> assist in the selection of General Circulation Models for climate
> change research. Methods in Ecology and Evolution.

-----

#### Use offline

You need to be connected online to use the app. When an analysis is run
in the app, many climatic layers in raster format are loaded and
analysed in the background, and these layers encompass several Gb of
hard drive space. However, if you prefer to use the app offline, you may
download it (including climatic layers) from the following location: (~4.3 Gb) <http://bit.ly/GCM_compareR_offline> (copy and paste the link in your
browser)

To run the app locally you need to have
<a href = "https://cran.r-project.org/" target = "_blank">R</a> and
<a href = "https://rstudio.com/products/rstudio/download/" target = "_blank">RStudio
Desktop</a> installed. After unzipping the files, locate and open the
*GCM\_compareR.Rproj* file. RStudio will open. While any of the *ui.R*
or the *server.R* files are opened, you should see a *run* button on the
top right corner of the script quadrant. Clic *run* to start the app on
your default browser. Alternatively, you can run the code
`shiny::runApp` instead of pressing *run*.

-----

#### Contact us

Please, email <derek.corcoran.barrios@gmail.com> with any question of
create an issue on
<a href = "https://github.com/marquetlab/GCM_compareR" target = "_blank">github</a>.

-----

#### Development

`GCM_compareR` has been developed by Javier Fajardo, Derek Corcoran,
Patrick Roehrdanz, Lee Hannah and Pablo Marquet in
<a href = "http://marquet.cl/" target = "_blank">Marquet Lab</a> in
Pontificia Universidad Católica de Chile, in Santiago de Chile. It was
built as part of the
<a href = "http://www.sparc-website.org/" target = "_blank">Spatial
Planning for Protected Areas in Response to Climate Change initiative
(SPARC)</a> project, a GEF initiative leaded by Conservation
International (CI), and with the support of
<a href = "http://ieb-chile.cl/" target = "_blank">Instituto de Ecología
y Biodiversidad (IEB)</a> in
Chile.

-----

#### Authors

<a href = "https://scholar.google.es/citations?user=L30b63UAAAAJ&hl=es&oi=ao" target = "_blank">Javier
Fajardo  
<a href = "https://derek-corcoran-barrios.github.io/" target = "_blank">Derek
Corcoran</a>  
Patrick Roehrdanz  
Lee Hannah  
<a href = "http://marquet.cl/" target = "_blank">Pablo Marquet</a>

-----

#### Climatic data

This application uses downscaled climate data published by CGIAR-CCAFS
(Research Program on Climate Change, Agriculture and Food Security)
under CC 4.0 license. All the raster data used by `GCM_compareR` is
available from their
<a href = "http://ccafs-climate.org/" target = "_blank">data portal</a>
and their R package (Chamberlain 2017).

-----

#### References

<div id="refs" class="references">

<div id="ref-Chamberlain2017">

Chamberlain, Scott. 2017. *Ccafs: Client for ’Ccafs’ ’Gcm’ Data*.
<https://CRAN.R-project.org/package=ccafs>.

</div>

<div id="ref-Shepherd2018">

Shepherd, Theodore G, Emily Boyd, Raphael A Calel, Sandra C Chapman,
Suraje Dessai, Ioana M Dima-west, Hayley J Fowler, and Rachel James.
2018. “Storylines: an alternative approach to representing uncertainty
in physical aspects of climate change.” *Climatic Change* 151. Climat ic
Change: 555–71. <https://doi.org/10.1007/s10584-018-2317-9>.

</div>

<div id="ref-Taylor2012a">

Taylor, Karl E., Ronald J. Stouffer, and Gerald A. Meehl. 2012. “An
overview of CMIP5 and the experiment design.” *Bulletin of the American
Meteorological Society* 93 (4): 485–98.
<https://doi.org/10.1175/BAMS-D-11-00094.1>.

</div>

<div id="ref-Thuiller2019">

Thuiller, Wilfried, Maya Guéguen, Julien Renaud, Dirk N Karger, and
Niklaus E Zimmermann. 2019. “Uncertainty in ensembles of global
biodiversity scenarios.” *Nature Communications* 10 (1446). Springer US:
1–9. <https://doi.org/10.1038/s41467-019-09519-w>.

</div>

<div id="ref-VanVuuren2011">

Vuuren, Detlef P. van, Jae Edmonds, Mikiko Kainuma, Keywan Riahi,
Allison Thomson, Kathy Hibbard, George C. Hurtt, et al. 2011. “The
representative concentration pathways: An overview.” *Climati c Change*
109 (1): 5–3 1. <https://doi.org/10.1007/s10584-011-0148-z>.

</div>

<div id="ref-Zappa2017">

Zappa, Giuseppe, and Theodore G. Shepherd. 2017. “Storylines of
Atmospheric Circulation Change for European Regional Climate Impact
Assessment.” *Journal of Climate* 30: 6561–77.
<https://doi.org/10.1175/JCLI-D-16-0807.1>.

</div>

</div>
