# DasCombat Shiny fit operator

##### Description

The `dascombat_shiny_fit_operator` is a software developed at PamGene.
There are two ways of usage:
1. Model fitting and applying the model (prediction) is done in 1 step
2. First model fitting is done on part of the data (usually control samples). Second, this fitted model is used to make predictions on the full dataset.
In the first step, save the model with the `Return link to Combat model` option. In the second step, use the `Apply saved model` option to apply the fitted model (add the `model` factor that was output from the fitting step as a label on the apply step).

##### Input projection and output relations
The input projection depends on whether the fitting and prediction procedure is used at once or in two steps. The label is only required in the two-step procedure in the second step.

Input projection|.
---|---
`y-axis`        | normalized (log- or VSN transformed) values (not S100!)
`row`           | peptide IDs
`column`        | Single data value per cell, e.g. Barcode, Row
`colors`        | expected technical batches (e.g. Barcode, Run)
`labels` (optional) | the saved model

The output relations depend on whether the fitting or prediction procedure is used. In case the fitting procedure is used, the
Shiny application returns a `model` and a `CmbCor` variable. In case the prediction procedure is used, it will only return a `CmbCor` variable.

Output relations|.
---|---
`model`        | character, name of the DASCOMBAT model (to be used in the prediction step)
`CmbCor`       | Combat-corrected values

##### Usage
In the operator view, the batch effect removal can be assessed by inspecting the PCA before and after Combat.
If the batch effect is removed, click on `Done`.

An example of batch effect removal:
![Example screenshot](/static/combat.png?raw=true "Example of application")

