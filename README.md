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

## Citation

Please cite the original data sources. 

In case of the **Barro-Lee education data** (`school_agv_yrs`, `school_share_sec`, `school_share_sec`), please cite the following article: 
Barro, Robert and Jong-Wha Lee, 2013, "A New Data Set of Educational Attainment in the World, 1950-2010." *Journal of Development Economics*, vol 104, pp.184-198, doi: [10.1016/j.jdeveco.2012.10.001](https://doi.org/10.1016/j.jdeveco.2012.10.001)

In case of the **KOF Globalization data** (`kof_G`, `kof_G_df`, `kof_G_dj`, `kof_EcG`, `kof_EcG_df`, `kof_EcG_dj`, `kof_trade`, `kof_trade_df`, `kof_trade_dj`, `kof_fin`, `kof_fin_df`, and `kof_fin_dj`), please cite:
Gygli, Savina, Florian Haelg, Niklas Potrafke and Jan-Egbert Sturm (2019): The KOF Globalisation Index – Revisited, *Review of International Organizations*, doi: [10.1007/s11558-019-09344-2](https://doi.org/10.1007/s11558-019-09344-2)

In case of the **Chinn-Ito index** (`chinn_ito` or `chinn_ito_normed`), please cite:
Chinn, Menzie D. and Hiro Ito (2006). "What Matters for Financial Development? Capital Controls, Institutions, and Interactions", *Journal of Development Economics*, Vol. 81(1), p. 163-192, doi: [10.1016/j.jdeveco.2005.05.010](https://doi.org/10.1016/j.jdeveco.2005.05.010).
