# 2024-FAB

Code for the 2024 [Syracuse Football Analytics Blitz](https://footballanalyticsblitz.weebly.com/past-fabs.html).

Case Requirements:
- Projected quarterback success over the next 3 years in the context of each scheme.
- Risk profile analysis: which quarterback is the most projectable? Which quarterback is the least projectable? Consider the variance of projected outcomes.
- Which traits are most transferable from college to the NFL?

Our Approach and Analysis:
- Create a k-means cluster for the scheme for each CFB/NFL team in each season.
- Using the results from the k-means cluster to create an XGB Classification model to predict schemes on a play-by-play level
- Aggregated scheme results for NFL QBs, and built a model that predicts the expected SIS points for a QB based on their college data
- Used that model to make predictions for the 6 QB's (Williams, Maye, Daniels, McCarthy, Penix Jr., Nix) in each scheme
