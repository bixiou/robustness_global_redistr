You need to improve the MRP computations.

For the U.S.:
- are you using the latest date available?
- inflate money amounts to 2025 values using inflation. For income thresholds, use 36205, 72097, 129899
- check that urbanity is defined in the same way using the Census and in the survey; if it's not the case, create and use a new survey's urbanity variable consistently with the Census

For non-US: 
- don't assume independence between margins
- download census (or equivalent) data to get the joint distribution of quota variables
- re-compute the MRP using this

For each country:
- export a table that compares the one-dimensional margins according to pop_freq and the official data fetched for this exercise; flag any deviation by more than 5%
- based on the joint distributions, find the sociodemographics that are over- or under-represented in the sample (e.g. by looking at the intersections of each pair of quota variables)