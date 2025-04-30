# jab-replication

Replication files for Jackknife-after-Bootstrap in dissertation

Most simulations were run on (ROAR)[<https://www.icds.psu.edu/access-roar-and-roar-collab-online/>], Penn State's super computer.

## File Structure

-   R/ - code to run simulations and generate plots

    -   clash-of-kings-simulation.R - JaB for Clash of Kings network

    -   game-of-thrones-simulation.R - JaB for Game of Thrones network

    -   ice-distance.R - ICE distance, file copied directly from (here)[<https://github.com/Siva-47/ICE/blob/main/methods/ICE.R>]

    -   jab-for-regression.R - JaB for linear regression simulation

    -   karate-simulations.R - JaB simulation study with karate network

    -   local-boot-functions.R - functions needed to run local bootstrap method, see roxygen documentation in file for more details

    -   paul-revere-simulations.R - JaB simulation study with Paul Revere network

-   data/ - Rdata files generated from simulation R scripts. Some files are too large to push to GitHub, but can be generated from R scripts.

    -   cok-eigen.Rdata - results from Clash of Kings network with eigenvector centrality

    -   cok-strength.Rdata - results of Clash of Kings network with strength centrality

    -   got-eigen.Rdata - results of Game of Thrones network with eigenvector centrality

    -   got-strength.Rdata - results of Game of Thrones network with strength centrality

    -   karate-distances.Rdata - results of simulation study with karate network

    -   pr-distances.Rdata -results of simulation study with Paul Revere network
