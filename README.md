# jab-replication

Replication files for Jackknife-after-Bootstrap in dissertation

Most simulations were run on [ROAR](https://www.icds.psu.edu/access-roar-and-roar-collab-online/), Penn State's super computer.

## File Structure

-   R/ - code to run simulations and generate plots

    -   chapter1-figures.R - code to generate all figures in Chapter 1 of dissertation

    -   clash-of-kings-simulation.R - JaB for Clash of Kings network

    -   game-of-thrones-simulation.R - JaB for Game of Thrones network

    -   ice-distance.R - ICE distance, file copied directly from [here](https://github.com/Siva-47/ICE/blob/main/methods/ICE.R)

    -   jab-for-regression.R - JaB for linear regression simulation

    -   karate-simulations.R - JaB simulation study with karate network

    -   local-boot-functions.R - functions needed to run local bootstrap method, see roxygen documentation in file for more details

    -   paul-revere-simulations.R - JaB simulation study with Paul Revere network

    -   trump-simulation.R - JaB for TrumpWorld network. Also includes generating simulated null distributions for Jason Greenblatt.

-   data/ - Rdata files generated from simulation R scripts. Some files are too large to push to GitHub, but can be generated from R scripts.

    -   cok-eigen.Rdata - results from Clash of Kings network with eigenvector centrality

    -   cok-strength.Rdata - results of Clash of Kings network with strength centrality

    -   got-eigen.Rdata - results of Game of Thrones network with eigenvector centrality

    -   got-strength.Rdata - results of Game of Thrones network with strength centrality and bootstrap samples (file is too large to push to GitHub]

    -   karate-distances.Rdata - results of simulation study with karate network

    -   pr-distances.Rdata -results of simulation study with Paul Revere network

    -   trump-betweenness.Rdata - results of TrumpWorld network with betweenness centrality

    -   trump-boot-samps.Rdata - bootstrap samples used in TrumpWorld simulations (too large to store on GitHub]

    -   trump-degree.Rdata - results of TrumpWorld network with degree centrality

    -   trump-eigen.Rdata - results of TrumpWorld network with eigenvector centrality

## Data set sources

-   Karate network: In the igraphdata package [here](https://rdrr.io/cran/igraphdata/man/karate.html)

-   Paul Revere network: Data set published [here](https://github.com/kjhealy/revere) Original data set is in David Hackett Fischer's *Paul Revere's Ride* (Oxford University Press, 1995].

-   *A Song of Ice and Fire* (George R.R. Martin) data found [here](https://github.com/harsh2201/GOTNetworkAnalysis)

-   Trump World: The orignal TrumpWorld data is from Buzzfeed News and can be found [here](https://www.buzzfeednews.com/article/johntemplon/help-us-map-trumpworld) and is included in the [JaB package on GitHub](https://github.com/olbeck/jab)
