# AVD-MSc-thesis
The code for my MSc Thesis ``Enhancing Aortic Valve Diameter Prediction: Accounting for Demographic Variability and Measurement Techniques"

1. Run Preprocessing.R: includes all necessary packages and data preprocessing (including cleaning, outlier removal and transformations)

2. Explorations: donor_exploration.R, UMCG_exploration.R, Lopez_exploration.R, differences_exploration.R

3. Analysis to find the best model structure: Model_selection.R 

4. Trend_correction.R identifies the changepoints via the SN algorithm, applies trend corrections to the AV diameters, and retrains a full GAM. Before running this:
	- Run Full GAM from best_model_structure.R (needed for the initial residuals)
	- Run segment_neighbourhood_window.R 
	- To see it applied in an example see example_simple_SN_algorithm.R

5. bias_quantified.R generates pseudo-echo AV diameters for the donor data

6. Best_models.R can be run to compare the performance of models to the established models from previous literature

7. For the AVR model (to predict the physical size of healthy AV diameter), use the trend corrected model. For diagnosis, use the trend corrected & echo corrected model (the model using psuedo-echo AV diameter measurements).
