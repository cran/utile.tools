utile.tools: Tools for Preparing and Summarizing Data for Publication
==============================

### Tabulate Functions
- **tabulate_model()**: Returns tabulated data from a model object. Useful for graphing a fit or creating a useable coefficient table. Currently only supports survival models with plans to expand in future.
- **tabulate_at_risk()**: Returns a risk table from a model object and specified time points.
- **tabulate_logit()**: Legacy function. Returns list of summary statistics from data for a logistic regression model.

### Paste Functions
- **paste_freq()**: Returns a human-readable frequency from count(able) data. Handily has methods for several types of data.
- **paste_median()**: Returns a human-readable median with inter-quartile range from numeric data.
- **paste_mean()**: Returns a human-readable mean with standard deviation from numeric data.
- **paste_efs()**: Returns a human-readable event-free-survival from a survfit object and a specified time point.

### Calculation Functions
- **calc_duration()**: Returns the duration of time between two provided date objects. Essentially a lubridate macro.

### Test Functions
- **test_numeric()**: Returns a p-value from parametric or non-parametric testing of stratified continuous (numeric) data. Supports parametric and non-parametric testing (see docs).
- **test_factor()**: Returns a p-value from parametric or non-parametric testing of stratified categorical (factor) data. Supports parametric and non-parametric testing (see docs).
