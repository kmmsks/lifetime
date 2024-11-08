# lifetime

Analysis code for manuscript:

**Lifetime incidence and age of onset of mental disorders, and 12-month service utilization in primary and secondary care: a Finnish nationwide registry study**

Authors: Kimmo Suokas, Ripsa Niemi, Mai Gutvilig, John J. McGrath, Kaisla Komulainen, Jaana Suvisaari, Marko Elovainio, Sonja Lumme, Sami Pirkola, Christian Hakulinen

The procedure to reproduce figures and tables of the study:

1.  In the Statistics Finalnd's restricted environment (Fiona), run the preprocessing scripts for the healthcare register as described in

    -   Suokas, K., Gutvilig, M., Lumme, S., Pirkola, S., & Hakulinen, C. (2024). Enhancing the accuracy of register-based metrics: Comparing methods for handling overlapping psychiatric register entries in Finnish healthcare registers. International Journal of Methods in Psychiatric Research, e2029. <https://doi.org/10.1002/mpr.2029>

    -  code: <https://github.com/kmmsks/hilmo_identify_episodes>

    - Select Model 2.
    
2.  In the Fiona environment, run the scripts from the folder `R_in_fiona`:

    -   

    -   `00_service_utilization_in_fiona_environmet.R`

3.  Export the data from Fiona following the rules and restrictions by Statistics Finland

4.  Place the exported data to folder `data`in this project.

5.  Knitr the document `1_manuscript_extended_data_supplement.qmd` to produce the manuscript in docx-format

6.  Use Word to finalize the document

    -   Stylise the front page.
    -   Check references.
    -   Replace "€€" with " " (two spacebars) to create indention to tables, as controlloing flextable indentions in docx output was difficult.
    -   Check figure sizes.
    -   Remove duplicate tables from the supplement filem as cross refence did not fuction correctly without them.
    -   Manually create table of contest for the supplementary file.


Regarding the interactive online material at <https://mentalnet.shinyapps.io/lifetime/>, see <https://github.com/kmmsks/ci_interactive>
