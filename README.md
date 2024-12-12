This R package includes two functions designed to compute the Percentile Annual Cycle for climate variables.

The first function implements the Moving Empirical Percentile (MEP) method, which calculates the empirical percentile for a given day using a centered moving window (a 29-day window is recommended).

The second function applies Generalized Additive Model (GAM) smoothing to the MEP output, effectively reducing undesired peaks caused by sampling issues.
