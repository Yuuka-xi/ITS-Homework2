# Cottbus DRT Service Study

This repository contains the files for an Intelligent Transport Systems homework project on demand-responsive transport (DRT) service in Cottbus.

The study compares several DRT service configurations with different fleet sizes, vehicle capacities, and maximum waiting-time settings. The simulation outputs are evaluated with service quality indicators and an equal-weight AHP ranking.

## Repository Structure

```text
ITS-Homework2/
+-- Assignment_2/
|   +-- drt_ahp_ranking_S1_S9_equal_criteria.R
|   +-- Task3-Cottbus/
|       +-- config_S1.xml ... config_S9.xml
|       +-- new_scenarios_S1_S9_parameters.csv
|       +-- drt-vehicles/
|       +-- output/
+-- Draft/
+-- GroupC_523622_523617_476466_H2.docx
+-- ITS_GroupC_viedo.mp3
+-- TU_Berlin_Praesentation_Master_einfarbig_Rot.pptx
```

## Scenarios

The final scenarios are listed in:

```text
Assignment_2/Task3-Cottbus/new_scenarios_S1_S9_parameters.csv
```

They vary three main parameters:

| Parameter | Meaning |
|---|---|
| Fleet | Number of DRT vehicles |
| Seats | Vehicle seating capacity |
| MaxWait | Maximum allowed passenger waiting time in seconds |

## Evaluation Criteria

The scenarios are compared using six criteria:

- Average waiting time
- 95th percentile waiting time
- Total travel time
- Rejections
- Idle vehicle share
- Passenger-km / vehicle-km ratio

All criteria are treated as equally important in the AHP ranking.

## Running the AHP Analysis

Run the R script:

```r
source("Assignment_2/drt_ahp_ranking_S1_S9_equal_criteria.R")
```

The script reads the scenario summary from:

```text
Assignment_2/Task3-Cottbus/output/new_scenarios_S1_S9/
```

It generates bar charts for each criterion, pairwise comparison matrices, and the final AHP ranking.

## Outputs

Simulation and analysis results are stored under:

```text
Assignment_2/Task3-Cottbus/output/new_scenarios_S1_S9/
```

The AHP figures and ranking outputs are stored under:

```text
Assignment_2/Task3-Cottbus/output/new_scenarios_S1_S9/ahp_equal_criteria_outputs/
```

## Final Report

The final submitted report is:

```text
GroupC_523622_523617_476466_H2.docx
```

Draft documents are kept in `Draft/` for reference.
