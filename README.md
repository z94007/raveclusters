# README

## Cluster Power Activities across Multiple Subjects

This module does xxx. Objective? Use case? 

**Work flow**
Add a use case (for example, if I want h-cluster, how to set params)

1. Select data….
2. …how to set input
3. What to expect from output and how to diagnose it


## Detailed Document on Inputs and Outputs

**Inputs**

- Data files: xxx

**Outputs**

- Cluster visualization: Cluster center shape xxx


----------

**Inputs** :

| Id                  | label                                               | Class            | Discription                                                                                                                                 |
| ------------------- | --------------------------------------------------- | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| analysis_data       | Data files located in this project's RAVE directory |                  |                                                                                                                                             |
| check_scale         | Z-score data                                        | checkbox (logic) | if selected, z-scoring would be applied on the signals within the selected time window across electrodes                                    |
| input_groups        | Condition Group                                     | list             | The condition groups that we would like to study, with elements as selected conditions and given list element names as condition group name |
| input_method        | Clustering Method                                   | character        | name of cluster agorithms(ex. 'H-Clust', 'PAM')                                                                                             |
| time_window         | Time Window                                         | number           | a 2-element vector to indicate the time window range, the analysis would only apply on the power within the time window                     |
| distance_method     | Clustering Distance Measurement                     | character        | methods to calculate the distance (dissimilarity) for the clustering (ex. 'euclidean', 'maximum',"manhattan", "canberra", "minkowski")      |
| mds_distance_method | MDS Distance Measurement                            |                  | methods to calculate the distance between points to generate the mds diagnosis (ex. 'euclidean', 'maximum',"manhattan","canberra")          |
| op_run              | Optimal Number of Clusters Analysis                 | checkbox         | if selected, generate the plots of silhouette and SSE for  different clustering methods to estimate the optimal number of clusters          |
| do_run              | Run Analysis                                        |                  | run the clustering analysis to generate all the plots and tables                                                                            |

**Outputs** : 

| Id                 | Label                      | Class   | Description                                                                                                              |
| ------------------ | -------------------------- | ------- | ------------------------------------------------------------------------------------------------------------------------ |
| cluster_plot       | Cluster Visualization      | graph   | for each clusters, visualize the mean and point-wise standard deviation of the power against different conditions        |
| tsne_plot          | MDS Diagnosis              | graph   | visualize the original high dimension data in two dimension space with MDS                                               |
| cluster_membership | Clustering Membership      | chart   | tables shown which electrode from which subject is in which cluster                                                      |
| dendrogram_plot  | Dendrogram                 | diagram | hierarchical clustering only; shows how the hierarchical arrangement happens among objects                               |
| optimal_cluster_number_plot     | Optimal number of clusters | graph   | visualize the silhouette and SSE for the different numbers of clustering, help determine the optimal numbers of clusters |


