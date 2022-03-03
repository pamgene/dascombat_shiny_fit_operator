# DasCombat Shiny fit operator

##### Description

The `dascombat_shiny_fit_operator` is the Shiny version of the oeprator to obtain 
a fitted model based on the DasCOMBAT software developed at PamGene. 
This fitted model can then be used to make predictions in the using the `Apply Saved Model`
option in this operator.

##### Usage

Input projection|.
---|---
`y-axis`        | values
`row`           | type, description 
`column`        | type, description 
`colors`        | the reference batches
`labels` (optional) | contains the saved model

Output relations|.
---|---
`output_var`        | character, name of the DASCOMBAT model (to be used with other dascombat_prediction_operator)

##### Screenshots
![Example screenshot](/static/screenshot.PNG?raw=true "Example of application")

##### Details

Details on the computation can be found in the `pamgene::pgbatch` and `SVA::combat`
applications.

##### See Also

[dascombat_fit_operator](https://github.com/tercen/dascombat_fit_operator)
, [dascombat_prediction_operator](https://github.com/tercen/dascombat_prediction_operator)

