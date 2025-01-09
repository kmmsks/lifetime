# Lifetime incidence and age of onset of mental disorders, and 12-month service utilization in primary and secondary care: a Finnish nationwide registry study

Analysis code for a manuscript.

Authors: Kimmo Suokas, Ripsa Niemi, Mai Gutvilig, John J. McGrath, Kaisla Komulainen, Jaana Suvisaari, Marko Elovainio, Sonja Lumme, Sami Pirkola, Christian Hakulinen

The procedure to reproduce figures and tables of the study:

1.  In the Statistics Finalnd's restricted environment (Fiona), run the preprocessing scripts for the healthcare register as described in

    -   Suokas, K., Gutvilig, M., Lumme, S., Pirkola, S., & Hakulinen, C. (2024). Enhancing the accuracy of register-based metrics: Comparing methods for handling overlapping psychiatric register entries in Finnish healthcare registers. International Journal of Methods in Psychiatric Research, e2029. <https://doi.org/10.1002/mpr.2029>

    -  code: <https://github.com/kmmsks/hilmo_identify_episodes>

    - Select Model 2.
    
2.  In the Fiona environment, run the scripts from the folder `R_in_fiona`:

    -   `00_cumulative_incidence_incidence_rates.R`

    -   `00_service_utilization_in_fiona_environmet.R`

3.  Export the data from Fiona following the rules and restrictions by Statistics Finland

4.  Place the exported data to folder `data`in this project.

5.  Knitr the document `1_tables_figures.qmd` to produce tables and figures


Version: 0.0.2-submitted (2025-01-09) [![DOI](https://zenodo.org/badge/884702667.svg)](https://doi.org/10.5281/zenodo.14066560)


Regarding the interactive online material at <https://mentalnet.shinyapps.io/lifetime/>, see <https://github.com/kmmsks/ci_interactive>
