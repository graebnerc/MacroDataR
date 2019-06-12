---
title: "Macroeconomic data"
author: "Claudius Gräbner"
date: "2019-06-11"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Macroeconomic data}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Information about the data set

## Meta data
Here goes the documentation of the data set. Note that you can also look at `data-raw/macro_data.R` to see how the data gets constructed.


| Variable         | Description                                  | Unit                          | Source           |
| -----------------|:--------------------------------------------:| :----------------------------:|:----------------:|
| iso3c                   | The iso3 country code                                                                                       | iso3c    | [Info][1] |
| year                    | The year of observation                                                                                     |   NA     |  |
| res_rents               | Natural ressource rents                                                                                     | % of GDP | [World Bank][2]
| gdp_real_lcu            | Real GDP in constant LCU                                                                                    | constant LCU | [World Bank][3]
| gdp_real_lcu_growth     | Real GDP growth (constant LCU)                                                                              | % change | [World Bank][4]
| gdp_real_pc_lcu         | Real GDP per capita (constant LCU)                                                                          | constant LCU per capita | [World Bank][5]
| gdp_real_pc_lcu_growth  | Real GDP per capita growth (constant LCU)                                                                   | % change | [World Bank][6]
| gdp_real_usd            | Real GDP (constant 2010 US$)                                                                                | constant 2010 US$ | [World Bank][7]
| gdp_real_pc_usd         | Real GDP per capita (constant 2010 US$)                                                                     | constant 2010 US$ per capita | [World Bank][8]
| gdp_nom_lcu             | Nominal GDP (current LCU)                                                                                   | current LCU | [World Bank][9]
| gdp_nom_pc_lcu          | Nominal GDP per capita (current LCU)                                                                        | current LCU per capita | [World Bank][10]
| gdp_nom_usd             | Nominal GDP (current US$)                                                                                   | current US$ | [World Bank][11]
| gdp_nom_pc_usd          | Nominal GDP per capita (current US$)                                                                        | current US$ per capita | [World Bank][12]
| gdp_real_ppp            | GDP, PPP (constant 2011 int $)                                                                              | constant 2011 int $ | [World Bank][13]
| gdp_real_pc_ppp         | GDP, PPP per capita (constant 2011 int $)                                                                   | constant 2011 int $ per capita | [World Bank][14]
| gdp_nom_ppp             | GDP, PPP (current int $)                                                                                    | current int $ | [World Bank][15]
| gdp_nom_pc_ppp          | GDP, PPP per capita (current int $)                                                                         | current int $ per capita | [World Bank][16]     
| cpi                     | Harmonised consumer price index (all items)                                                                 | 2015 = 100           | [AMECO][16]       |
| unemp_rate              | Unemployment rate                                                                                           | % of active population | [AMECO][16]     |
| unemp_rate_wb | Unemployment rate from World Bank (modeled ILO estimate) | % of total labor force | [World Bank][48] | 
| cap_form                | Gross fixed capital formation, total economy                                                                | current prices       | [AMECO][16]       |
| wage_share              | Adjusted wage share                                                                                         | % of GDP             | [AMECO][16]       |
| rulc                    | Real unit labour costs: total economy (Ratio compensation per employee to nominal GDP per pers employed.)   | 2010=100             | [AMECO][16]       |
| nulc_nac                | Nominal unit labour costs: total economy (Ratio compensation per employee to real GDP per pers employed.)   | Loc curr: 2010 = 100 | [AMECO][16]       |
| nulc_eur                | Nominal unit labour costs: total economy (Ratio compensation per employee to real GDP per pers employed.)   | EUR: 2010 = 100      | [AMECO][16]       |
| eci_harv                | Economic Complexity Index (ECI)           | Index | [Harvard][17] |
| eci_rank_harv           | Rank of the country according to the ECI  | Index | [Harvard][17] |
| eci_mit                | Economic Complexity Index (ECI)           | Index | [MIT][18] |
| eci_rank_mit           | Rank of the country according to the ECI  | Index | [MIT][18] |
| current_account_GDP_WB           | Sum of net exports of goods and services, net primary income, and net secondary income.  | % of GDP | [World Bank][19] |
| current_account_GDP_ameco           | Sum of net exports of goods and services, net primary income, and net secondary income.  | % of GDP | [AMECO][16] |
| population           | Total population (midyear estimates).  | People | [World Bank][20] |
| population_ameco           | Total population (more years but less countries than WB).  | 1000 people | [AMECO][16] |
| debt_corp_nf_percGDP  | Debt of non-financial corporations (`DBTS11GDP`).  | % of GDP | [OECD (FIN_IND_FBS)][21]|
| debt_corp_f_percGDP   | Debt of financial corporations (`DBTS12GDP`).  | % of GDP | [OECD (FIN_IND_FBS)][21]|
| debt_gen_gov_percGDP  | Debt of general government (`DBTS13GDP`).  | % of GDP | [OECD (FIN_IND_FBS)][21] |
| debt_hh_npish_percGDI | Debts of households and NPISHs (`DBTS14_S15GDI`).  | % of GDI | [OECD (FIN_IND_FBS)][21]|
| total_debt_percGDP    | Debt of the total economy (`DBTS1GDP`).  | % of GDP | [OECD (FIN_IND_FBS)][21] |
| empl_ind | Employment in industry | % of total employment | [World Bank][22] |
| empl_agr | Employment in agriculture | % of total employment | [World Bank][23] |
| empl_serv | Employment in services | % of total employment | [World Bank][24] |
| empl_self | Self-employed | % of total employment | [World Bank][25] |
| unemp_youth_neet | Share of youth not in education, employment of training |  % of youth population | [World Bank][26] |
| VA_industry_gdp | Value added of industry (including construction) |  % of GDP |  [World Bank][27] |
| VA_manufct_gdp | Value added of manufacturing | % of GDP |  [World Bank][28] |
| wgi_accountability | Voice and Accountability captures perceptions of the extent to which a country's citizens are able to participate in selecting their government, as well as freedom of expression, freedom of association, and a free media (this is the estimate, errors and percentiles are also [available][29]). | Index (-2.5 to 2.5) | WGI [home][29]  [API][30] |
| wgi_control_corrupt | Control of Corruption captures perceptions of the extent to which public power is exercised for private gain, including both petty and grand forms of corruption, as well as "capture" of the state by elites and private interests (this is the estimate, errors and percentiles are also [available][29]). | Index (-2.5 to 2.5) | WGI [home][29]  [API][30] |
| wgi_gov_effectvn | Government Effectiveness captures perceptions of the quality of public services, the quality of the civil service and the degree of its independence from political pressures, the quality of policy formulation and implementation, and the credibility of the government's commitment to such policies (this is the estimate, errors and percentiles are also [available][29]). | Index (-2.5 to 2.5) | WGI [home][29]  [API][30] |
| wgi_pol_stability | Political Stability and Absence of Violence/Terrorism captures perceptions of the likelihood that the government will be destabilized or overthrown by unconstitutional or violent means, including politically-motivated violence and terrorism (this is the estimate, errors and percentiles are also [available][29]). | Index (-2.5 to 2.5) | WGI [home][29]  [API][30] |
| wgi_rule_of_law | Rule of Law captures perceptions of the extent to which agents have confidence in and abide by the rules of society, and in particular the quality of contract enforcement, property rights, the police, and the courts, as well as the likelihood of crime and violence. (this is the estimate, errors and percentiles are also [available][29]). | Index (-2.5 to 2.5) | WGI [home][29]  [API][30] |
| wgi_regul_quality | Regulatory Quality captures perceptions of the ability of the government to formulate and implement sound policies and regulations that permit and promote private sector development (this is the estimate, errors and percentiles are also [available][29]). | Index (-2.5 to 2.5) | WGI [home][29]  [API][30] |
| fdi_net_inflow_GDP | Foreign direct investment, net inflows | % of GDP |  [World Bank][31] | 
| fdi_net_outflow_GDP | Foreign direct investment, net outflows | % of GDP |  [World Bank][32] |  
| tax_rev_total_GDP | Tax revenue | % of GDP |  [World Bank][33] | 
| tax_rev_trade_TAX | Taxes on international trade | % of revenue |  [World Bank][34] | 
| tax_rev_inc_profits_TAX | Taxes on income, profits and capital gains | % of revenue |  [World Bank][35] | 
| trade_total_GDP | Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product | % of GDP | [World Bank][36] |  
| trade_exp_GDP | Exports of goods and services | % of GDP |  [World Bank][37] | 
| trade_imp_GDP | Imports of goods and services | % of GDP |  [World Bank][38] | 
| RD_expend_GDP | Research and development expenditure | % of GDP |  [World Bank][39] | 
| RD_scientists | The number of researchers engaged in Research &Development (R&D) | Persons per per million people |  [World Bank][40] |
| interest_long_term | Interest rates of long term (in most cases 10 year) government bonds (`IRLT`). | Per cent | [OECD (MEI_FIN)][41] |
| average_wages | Average annual wages per full-time and full-year equivalent employee in the total economy. | 2017 USD PPPs| [OECD (AV_AN_WAGE)][42] |
| debt_gen_gvt_gross | Government gross debt-to-GDP ratio (`DBTS13GDP`). | % of GDP | [OECD (NAAG)][43] |
| school_agv_yrs | Average Years of Schooling Attained | Share of population over 15 | [Barro-Lee][44] |
| school_share_sec | Percentage of Complete Secondary Schooling Attained | Share of population over 15 | [Barro-Lee][44] |
| school_share_ter | Percentage of Complete Tertiary Schooling Attained | Share of population over 15 | [Barro-Lee][44] |
| kof_G | KOF Globalisation Index | Index | [KOF Institute][45] |
| kof_G_df | KOF Globalisation Index, de facto | Index | [KOF Institute][45] |
| kof_G_dj | KOF Globalisation Index, de jure | Index | [KOF Institute][45] |
| kof_EcG | KOF Economic Globalisation Index | Index | [KOF Institute][45] |
| kof_EcG_df | KOF Economic Globalisation Index, de facto | Index | [KOF Institute][45] |
| kof_EcG_dj | KOF Economic Globalisation Index, de jure | Index | [KOF Institute][45] |
| kof_trade | KOF Trade Globalisation Index | Index | [KOF Institute][45] |
| kof_trade_df | KOF Trade Globalisation Index, de facto | Index | [KOF Institute][45] |
| kof_trade_dj | KOF Trade Globalisation Index, de jure | Index | [KOF Institute][45] |
| kof_fin | KOF Financial Globalisation Index | Index | [KOF Institute][45] |
| kof_fin_df | KOF Financial Globalisation Index, de facto | Index | [KOF Institute][45] |
| kof_fin_dj | KOF Financial Globalisation Index, de jure | Index | [KOF Institute][45] |
| chinn_ito | Measure for a country's degree of capital account openness.  | Index | [Chinn and Ito][46] |
| chinn_ito_normed | `chinn_ito` normalized bwetween 0 and 1 | Index | [Chinn and Ito][46] |
| lmf_port_eq_asst_stock| Portfolio equity assets (stock) | Current US dollars, millions | [LMF database][47] |
| lmf_port_eq_liab_stock | Portfolio equity liabilities (stock) | Current US dollars, millions | [LMF database][47] |
| lmf_port_dbt_asst | Portfolio debt assets | Current US dollars, millions | [LMF database][47] |
| lmf_port_dbt_liab | Portfolio debt liabilities | Current US dollars, millions | [LMF database][47] |
| lmf_fdi_asst_stock | FDI assets (stock) | Current US dollars, millions | [LMF database][47] |
| lmf_fdi_asst_other | FDI assets (other) | Current US dollars, millions | [LMF database][47] |
| lmf_fdi_liab_stock | FDI liabilities (stock) | Current US dollars, millions | [LMF database][47] |
| lmf_fdi_liab_other | FDI liabilities (other) | Current US dollars, millions | [LMF database][47] |
| lmf_debt_asst_stock | Debt assets (stock): sum of "portfolio debt securities" and "other investment" (but not FDI intercompany debt, which is in the FDI statistics). | Current US dollars, millions | [LMF database][47] | 
| lmf_debt_liab_stock | Debt liabilities (stock): sum of "portfolio debt securities" and "other investment" (but not FDI intercompany debt, which is in the FDI statistics). | Current US dollars, millions | [LMF database][47] |
| lmf_nfa | Alternative FDI or equity valuation: includes net foreign assets using FDI or equity assets and liabilities estimated using different methodologies. | Current US dollars, millions | [LMF database][47] |
| lmf_nfa_gdp | NFA/GDP | % of GDP (current USD) | [LMF database][47] |
| labor_force_total | Labor force comprises people ages 15 and older who supply labor for the production of goods and services during a specified period. It includes people who are currently employed and people who are unemployed but seeking work as well as first-time job-seekers.| Nb of people | [World Bank][48]  
| bond_yield | Central government bond yields on the secondary market, gross of tax, with a residual maturity of around 10 years, as outlined in Art 121 of the Maastricht Treaty, where it is used as a measure for convergence.| Interest rate | [Eurostat][50] |
| sect_balance_foreign | Financial balance of foreign sector: Balances with RoW in % of GDP (sheet 10) multiplied with -1 | % of GDP |  [AMECO][16] |
| sect_balance_gvnt | Financial balance of government sector: net lending of general government (sheet 16) | % of GDP |  [AMECO][16] |
| sect_balance_priv_corp | Financial balance of private corporate sector: Net lending of corporations (sheet 14) divided by GDP current prices (sheet 6) | % of GDP |  [AMECO][16] |
| sect_balance_priv_HH | Financial balance of household sector: Net lending of households and NPISH (sheet 15) divided by GDP current prices (sheet 6) | % of GDP |  [AMECO][16] |
| sect_balance_priv | Financial balance of private sector: sect_balance_priv_corp + sect_balance_priv_HH | % of GDP |  [AMECO][16] |

[1]: https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
[2]: https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
[3]: https://data.worldbank.org/indicator/NY.GDP.MKTP.KN
[4]: https://data.worldbank.org/indicator/NY.GDP.PCAP.KN
[5]: https://data.worldbank.org/indicator/NY.GDP.PCAP.KD.ZG
[6]: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
[7]: https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
[8]: https://data.worldbank.org/indicator/NY.GDP.MKTP.CN
[9]: https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
[10]:https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
[11]: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
[12]: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD
[13]: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
[14]: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
[15]: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
[16]: https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/macro-economic-database-ameco/download-annual-data-set-macro-economic-database-ameco_en
[17]: http://atlas.cid.harvard.edu/downloads
[18]: https://atlas.media.mit.edu/en/resources/data/
[19]: https://data.worldbank.org/indicator/BN.CAB.XOKA.GD.ZS
[20]: https://data.worldbank.org/indicator/SP.POP.TOTL
[21]: https://stats.oecd.org/Index.aspx?DataSetCode=FIN_IND_FBS
[22]: https://data.worldbank.org/indicator/sl.ind.empl.zs
[23]: https://data.worldbank.org/indicator/SL.AGR.EMPL.ZS
[24]: https://data.worldbank.org/indicator/SL.SRV.EMPL.ZS
[25]: https://data.worldbank.org/indicator/SL.EMP.SELF.ZS
[26]: https://data.worldbank.org/indicator/SL.UEM.NEET.ZS
[27]: https://data.worldbank.org/indicator/NV.IND.TOTL.ZS
[28]: https://data.worldbank.org/indicator/NV.IND.MANF.ZS
[29]: https://info.worldbank.org/governance/wgi/#home
[30]: https://api.worldbank.org/v2/sources/3/indicators
[31]: https://data.worldbank.org/indicator/BX.KLT.DINV.WD.GD.ZS
[32]: https://data.worldbank.org/indicator/BM.KLT.DINV.WD.GD.ZS
[33]: https://data.worldbank.org/indicator/GC.TAX.TOTL.GD.ZS
[34]: https://data.worldbank.org/indicator/GC.TAX.INTT.RV.ZS
[35]: https://data.worldbank.org/indicator/GC.TAX.YPKG.RV.ZS
[36]: https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
[37]: https://data.worldbank.org/indicator/NE.EXP.GNFS.ZS
[38]: https://data.worldbank.org/indicator/NE.IMP.GNFS.ZS
[39]: https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
[40]: https://data.worldbank.org/indicator/SP.POP.SCIE.RD.P6
[41]: https://stats.oecd.org/Index.aspx?DataSetCode=MEI_FIN
[42]: https://stats.oecd.org/Index.aspx?DataSetCode=AV_AN_WAGE
[43]: https://stats.oecd.org/Index.aspx?DataSetCode=NAAG
[44]: http://www.barrolee.com/
[45]: https://www.kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-globalisation-index.html
[46]: http://web.pdx.edu/~ito/Chinn-Ito_website.htm
[47]: http://www.philiplane.org/EWN.html
[48]: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
[49]: https://data.worldbank.org/indicator/SL.TLF.TOTL.IN
[50]: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=irt_lt_mcby_a&lang=en

## Open issues

* Add more tests for duplicated rows, just as with AMECO


## Descriptive statistics
Here is more information about the data.

Table: The descriptive statistics

variable                              mean             sd    obs
--------------------------  --------------  -------------  -----
average_wages                 3.482449e+04   1.187282e+04    570
bond_yield                    6.109006e+00   3.890869e+00    674
cap_form                      1.160987e+02   3.306685e+02   2139
chinn_ito                     1.257337e+00   1.349416e+00    819
chinn_ito_normed              7.417914e-01   3.159909e-01    819
cpi                           8.335915e+01   2.044781e+01    985
current_account_GDP_ameco    -1.244760e+00   5.231229e+00   1970
current_account_GDP_WB       -5.801584e-01   4.606344e+00    733
debt_corp_f_percGDP           5.345272e+02   1.222250e+03    503
debt_corp_nf_percGDP          1.317964e+02   5.724615e+01    503
debt_gen_gov_percGDP          6.880335e+01   3.575374e+01    507
debt_gen_gvt_gross            6.880335e+01   3.575374e+01    507
debt_hh_npish_percGDI         9.560289e+01   6.303779e+01    494
eci_harc_coi_hs               7.238397e-01   1.021782e+00    480
eci_harc_coi_sitc             8.914932e-01   1.188616e+00   1021
eci_harv_hs                   1.281993e+00   5.192609e-01    480
eci_harv_sitc                 1.338742e+00   5.426136e-01   1021
eci_mit                       1.311625e+00   5.340857e-01    989
eci_mit_plus                  1.134707e+00   3.495672e-01    949
empl_agr                      6.374046e+00   4.858520e+00    616
empl_ind                      2.772955e+01   6.615437e+00    616
empl_self                     1.610849e+01   7.530074e+00    616
empl_serv                     6.589642e+01   9.148009e+00    616
fdi_net_inflow_GDP            4.282146e+00   1.265412e+01    824
fdi_net_outflow_GDP           4.231619e+00   1.165608e+01    824
gdp_nom_lcu                   1.070035e+12   3.632065e+12   1012
gdp_nom_pc_lcu                9.247323e+04   3.658569e+05   1012
gdp_nom_pc_ppp                2.870353e+04   1.504384e+04    598
gdp_nom_pc_usd                1.928975e+04   1.845562e+04   1012
gdp_nom_ppp                   6.050902e+11   8.018594e+11    598
gdp_nom_usd                   4.149332e+11   6.995337e+11   1012
gdp_real_lcu                  1.420163e+12   4.050062e+12   1002
gdp_real_lcu_growth           2.921126e+00   3.127344e+00    993
gdp_real_pc_lcu               1.230659e+05   4.066080e+05   1002
gdp_real_pc_lcu_growth        2.511200e+00   3.133174e+00    993
gdp_real_pc_ppp               3.325649e+04   1.452085e+04    598
gdp_real_pc_usd               2.945176e+04   1.705887e+04   1002
gdp_real_ppp                  6.977720e+11   8.737992e+11    598
gdp_real_usd                  6.195019e+11   8.009846e+11   1002
gini_post_tax                 3.788363e+01   8.570254e+00   5279
gini_pre_tax                  4.358905e+01   6.759470e+00   5278
interest_long_term            6.037222e+00   3.570034e+00    698
kof_EcG                       6.826566e+01   1.512263e+01    926
kof_EcG_df                    5.963684e+01   1.936568e+01    926
kof_EcG_dj                    7.691221e+01   1.479206e+01    926
kof_fin                       6.825611e+01   1.883040e+01    926
kof_fin_df                    6.518397e+01   2.310748e+01    926
kof_fin_dj                    7.121789e+01   1.857130e+01    926
kof_G                         7.546839e+01   1.100691e+01    926
kof_G_df                      7.245519e+01   1.088731e+01    926
kof_G_dj                      7.849435e+01   1.259912e+01    926
kof_trade                     6.829445e+01   1.282840e+01    926
kof_trade_df                  5.408971e+01   1.902936e+01    926
kof_trade_dj                  8.257585e+01   1.187440e+01    926
labor_force_total             9.843727e+06   1.145189e+07    638
lmf_debt_asst_stock           1.558368e+02   2.328264e+02    673
lmf_debt_liab_stock           1.699135e+02   2.205404e+02    670
lmf_fdi_asst_other            2.019153e+02   2.425833e+02     81
lmf_fdi_asst_stock            1.883401e+02   2.413064e+02    741
lmf_fdi_liab_other            2.010452e+02   2.829810e+02     88
lmf_fdi_liab_stock            1.395978e+02   2.210145e+02    756
lmf_nfa                      -3.925397e+01   1.881579e+02    758
lmf_nfa_gdp                            NaN             NA      0
lmf_port_dbt_asst             1.630123e+02   2.244507e+02    508
lmf_port_dbt_liab             1.598543e+02   2.217842e+02    501
lmf_port_eq_asst_stock        1.670565e+02   2.258434e+02    756
lmf_port_eq_liab_stock        1.607547e+02   2.341049e+02    734
nulc_eur                      7.219230e+01   3.394590e+01   1799
nulc_lcu                      6.865927e+01   3.701823e+01   1799
population                    1.976719e+07   2.299377e+07   1232
population_ameco              2.617413e+04   4.581516e+04   2544
RD_expend_GDP                 1.608288e+00   8.337851e-01    445
RD_scientists                 3.140586e+03   1.535799e+03    424
res_rents                     4.628165e-01   5.182169e-01    900
rulc                          1.050939e+02   1.074576e+01   1800
school_agv_yrs                8.096014e+00   2.312813e+00    286
school_share_sec              2.091318e+01   1.514680e+01    286
school_share_ter              5.860839e+00   4.779374e+00    286
tax_rev_inc_profits_TAX       2.490449e+01   9.332486e+00    810
tax_rev_total_GDP             2.108720e+01   4.959340e+00    805
tax_rev_trade_TAX             1.515835e+00   2.691640e+00    475
total_debt_percGDP            7.967861e+02   1.261850e+03    503
trade_exp_GDP                 4.321252e+01   2.948889e+01   1007
trade_imp_GDP                 4.226116e+01   2.455558e+01   1007
trade_total_GDP               8.547369e+01   5.379680e+01   1007
unemp_rate                    6.808210e+00   5.280900e+00   1928
unemp_rate_wb                 8.858721e+00   4.456263e+00    616
unemp_youth_neet              1.077576e+01   4.020563e+00    323
VA_industry_gdp               2.551180e+01   5.100148e+00    660
VA_manufct_gdp                1.645300e+01   4.436667e+00    660
wage_share                    5.672020e+01   7.347621e+00   1800
wgi_accountability            1.223702e+00   2.685152e-01    418
wgi_control_corrupt           1.236671e+00   7.473791e-01    418
wgi_gov_effectvn              1.297896e+00   5.396178e-01    418
wgi_pol_stability             8.588920e-01   4.180943e-01    418
wgi_regul_quality             1.294658e+00   4.024164e-01    418
wgi_rule_of_law               1.281833e+00   5.266454e-01    418

# Information about the package

The package can be used to automatically update the whole dataset.
This way, it is straightforward to use newly published data.

To this end, you can use the function `update_macro_data`, which automatically recompiles the whole dataset.
See `help("update_macro_data", package = "MacroDataR")` for more details.
