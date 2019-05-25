# MacroDataR

A data set with commonly used macroeconomic indicators. Allows for automatically updating the whole data through the API if the original data sources.
When citing the data sources, please also refer to the original data sources.

## For non-R users
You can download the current version of the dataset from the `data` folder in csv format. This data gets updated from time to time. 

## For R users
Once you installed the package via

```
devtools::install_github("graebnerc/MacroDataR", ref = "development")
```

you can access the data from anywhere via `MacroDataR::macro_data`.

Please note that currently only a development version is available. 

You can update the data via the function `update_macro_data`. 
By setting `updata_full=TRUE` the function downloads all data from the API of the original data sources.

## Open issues

In case you want a new variable included or have any comments, please feel free to submit an issue.
All input is highly appreciated.
