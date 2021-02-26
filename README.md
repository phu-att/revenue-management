# Revenue Management

## Introduction
This project aims to incorporate dynamic programming to assist in identifying 
optimal revenue in three particular industries. Additonally, some heuristic approaches
were explored, and executed and compared the results with one another.

## Scope of the Project
- To analyse a café and explore various strategies to find optimal allocation
- To advice a sporting event organizer regarding an optimal allocation of five products 
- To analyse an optimal room allocation of a hotel in Morocco adjacent to the F1 event in 2021

## Dataset
* Self-generated data for the first and second instances  
* Self-collected data from various sources for the third instance (see ```companion_doc.pdf``` for more details)

## Directory Structure
``` bash
revenue-management
├── LICENSE
├── README.md
├── companion_doc.pdf
├── requirement.txt
└── scripts
    ├── analysis_00.R
    ├── analysis_01.R
    └── outputs
        ├── HF12_100t.pdf
        ├── HF12_200t.pdf
        ├── HF1_100t.pdf
        ├── HF1_200t.pdf
        ├── HF2_100t.pdf
        ├── HF2_200t.pdf
        ├── LF12_100t.pdf
        ├── LF12_200t.pdf
        ├── LF1_100t.pdf
        ├── LF1_200t.pdf
        ├── LF2_100t.pdf
        ├── LF2_200t.pdf
        └── marginal_plot.pdf
```
## Requirement
- To install the library: ``` bash install.packages("lpSolve",repos = "http://cran.us.r-project.org") ```

## File Description
* ```companion_document.pdf``` -> Document that explains rational and analysis of the project
* ```requirements.txt``` -> Required R library 

### scripts
* ```analysis_00.R``` -> The code contains data processing process & conducted analysis, for the first and second instances 
* ```analysis_01.R``` -> The code contains data processing process & conducted analysis, for the thrid instance

### outputs
* ```HF12_100t.pdf``` -> The graph contains an optimal allocation for High-Fare Day1&Day2 product, with 100 periods remaininig 
* ```HF12_200t.pdf``` -> The graph contains an optimal allocation for High-Fare Day1&Day2 product, with 200 periods remaininig
* ```HF1_100t.pdf``` -> The graph contains an optimal allocation for High-Fare Day1 product, with 100 periods remaininig 
* ```HF1_200t.pdf``` -> The graph contains an optimal allocation for High-Fare Day1 product, with 200 periods remaininig  
* ```HF2_100t.pdf``` -> The graph contains an optimal allocation for High-Fare Day2 product, with 100 periods remaininig 
* ```HF2_200t.pdf``` -> The graph contains an optimal allocation for High-Fare Day2 product, with 200 periods remaininig 
* ```LF12_100t.pdf``` -> The graph contains an optimal allocation for Low-Fare Day1&Day2 product, with 100 periods remaininig 
* ```LF12_200t.pdf``` -> The graph contains an optimal allocation for Low-Fare Day1&Day2 product, with 200 periods remaininig
* ```LF1_100t.pdf``` -> The graph contains an optimal allocation for Low-Fare Day1 product, with 100 periods remaininig 
* ```LF1_200t.pdf``` -> The graph contains an optimal allocation for Low-Fare Day1 product, with 200 periods remaininig  
* ```LF2_100t.pdf``` -> The graph contains an optimal allocation for Low-Fare Day2 product, with 100 periods remaininig 
* ```LF2_200t.pdf``` -> The graph contains an optimal allocation for Low-Fare Day2 product, with 200 periods remaininig 
* ``` marginal_plot.pdf ``` -> The graph contains a diminishing marginal return effect of protecting High-Fare hotel package of a Morocco hotel
