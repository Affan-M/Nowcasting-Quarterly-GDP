# Nowcasting Headline and Sectoral Quarterly GDP

The paper was written by Mohamed Affan ([@Affan-M](https://github.com/Affan-M)) and Shaneez Latheef ([@shaneez-al](https://github.com/shaneez-al)), with a focus on the case of Maldives. It has been published by [Maldives Monetary Authority](https://mma.gov.mv/documents/Research%20and%20Policy%20Notes/2024/Nowcasting%20Headline%20and%20Sectoral%20Quarterly%20GDP.pdf).

The Quarto markdown file contains the paper (`./Paper/`), while the codes for the analysis is contained within the Scripts folder (`./Paper/Scripts/`).

## Abstract
Economic data becomes available with significant lags which in turn leads to uncertainty regarding the present state of the economy. In this paper, we set up a framework that can generate nowcasts for headline and sectoral level GDP, and conduct experiments to assess the accuracy of nowcasts. Using a large dataset of 449 economic indicators, we generate nowcasts from a host of univariate and multivariate models, which include traditional econometric nowcasting models and machine learning algorithms. We produce combinations of the best-performing models for each production sector. Furthermore, we explore hierarchical reconciliation methods to ensure that all individual nowcasts would adhere to aggregation constraints and empirically test the nowcast performance of the models and model combinations.

## API requirements
In order to run the `Data.R` script and refresh the data, you would need API keys/tokens from the [MMA Statistics Database](https://database.mma.gov.mv/api) and [U.S. Energy Information Administration (EIA)](https://www.eia.gov/opendata/), which you can request for free. Once you acquire the API tokens, you would need to create two text files with the respective API tokens in it:
- `config_mma.txt` with the API token for the MMA Statistics Database
- `config_eia.txt` with the API token for the EIA database

These files must be placed in the Scripts directory (`./Paper/Scripts/`).
