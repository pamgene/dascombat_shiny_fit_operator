# DasCombat Shiny fit operator

##### Description

The `dascombat_shiny_fit_operator` is the Shiny version of the oeprator to obtain a fitted model based on the DasCOMBAT software developed at PamGene. 
This fitted model can then be used to make predictions in the using the `Apply Saved Model` option in this operator.

##### Usage
The input projection depends on whether the fitting or prediction procedure is used in the `dascombat_shiny_fit_operator`. If the fitting procedure is desired, a label
is not required.

Input projection|.
---|---
`y-axis`        | the y values
`row`           | the peptide IDs
`column`        | the barcodes
`colors`        | the reference batches
`labels` (optional) | contains the saved model

The output projection depends on whether the fitting or prediction procedure is used in the `dascombat_shiny_fit_operator`. In case the fitting procedure is desired, the
Shiny application will return a `model` and a `CmbCor` variable. In case the prediction procedure is desired, it will only return a `CmbCor` variable.

Output relations|.
---|---
`model`        | character, name of the DASCOMBAT model (to be used with other dascombat_prediction_operator)
`CmbCor`       | list, the combat corrected values (`y-axis` in the input)

##### Screenshots
![Example screenshot](/static/screenshot.PNG?raw=true "Example of application")

##### Details

This Shiny application can be used for fitting a DASCombat model to data, but also
to apply this model on existing data.
Details on the computation can be found in the `pamgene::pgbatch` and `SVA::combat`
applications.

##### See Also

[dascombat_fit_operator](https://github.com/tercen/dascombat_fit_operator)
, [dascombat_prediction_operator](https://github.com/tercen/dascombat_prediction_operator)

