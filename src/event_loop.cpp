#include <Rcpp.h>
using namespace Rcpp;

//' Generate Arrival Events for the ER Simulation
//'
//' Uses an exponential inter-arrival process (Poisson arrivals) to produce a
//' data.frame of arrival events with assigned urgency levels. This is the
//' performance-critical sampling loop that benefits from C++ execution.
//'
//' @param arrival_rate  Double. Patients per minute (lambda for the exponential).
//' @param sim_duration  Double. Total simulation duration in minutes.
//' @param urgency_probs NumericVector of length 3. Probabilities for urgency
//'   levels 1 (Critical), 2 (Urgent), 3 (Standard). Must sum to 1.
//'
//' @return A data.frame with columns:
//' \describe{
//'   \item{type}{Character "ARRIVAL" for every row.}
//'   \item{time}{Numeric arrival time (minutes from simulation start).}
//'   \item{patient_id}{NA_character_ — filled in by R when processed.}
//'   \item{urgency_level}{Integer 1, 2, or 3.}
//' }
//' @export
// [[Rcpp::export]]
DataFrame generate_arrivals_cpp(double arrival_rate,
                                double sim_duration,
                                NumericVector urgency_probs) {

  if (arrival_rate <= 0.0)
    stop("arrival_rate must be positive.");
  if (sim_duration <= 0.0)
    stop("sim_duration must be positive.");
  if (urgency_probs.size() != 3)
    stop("urgency_probs must have exactly 3 elements.");

  // Cumulative probabilities for urgency sampling
  double cp1 = urgency_probs[0];
  double cp2 = urgency_probs[0] + urgency_probs[1];

  std::vector<double> times;
  std::vector<int>    urgencies;

  double current_time = 0.0;

  // Pre-reserve a reasonable capacity to avoid repeated reallocations
  int expected_n = (int)(arrival_rate * sim_duration * 1.5) + 10;
  times.reserve(expected_n);
  urgencies.reserve(expected_n);

  // Exponential inter-arrival sampling loop
  while (true) {
    // Draw inter-arrival time: Exp(arrival_rate)
    double u_time = R::rexp(1.0 / arrival_rate);
    current_time += u_time;

    if (current_time >= sim_duration) break;

    // Draw urgency level from categorical distribution
    double u_urgency = R::runif(0.0, 1.0);
    int urgency;
    if      (u_urgency < cp1) urgency = 1;
    else if (u_urgency < cp2) urgency = 2;
    else                      urgency = 3;

    times.push_back(current_time);
    urgencies.push_back(urgency);
  }

  int n = (int)times.size();

  // Build the return data.frame
  CharacterVector type_col(n, "ARRIVAL");
  NumericVector   time_col(n);
  CharacterVector pid_col(n, NA_STRING);
  IntegerVector   urgency_col(n);

  for (int i = 0; i < n; ++i) {
    time_col[i]    = times[i];
    urgency_col[i] = urgencies[i];
  }

  DataFrame result = DataFrame::create(
    Named("type")          = type_col,
    Named("time")          = time_col,
    Named("patient_id")    = pid_col,
    Named("urgency_level") = urgency_col
  );

  return result;
}


//' Compute Summary Statistics Over a Numeric Vector (Vectorized C++)
//'
//' Fast computation of mean, median, and specified quantile for a numeric
//' vector. Used internally to accelerate KPI calculations on large patient logs.
//'
//' @param x       NumericVector. Input values (e.g., wait times).
//' @param quantile_p Double. Quantile to compute, between 0 and 1. Default 0.95.
//'
//' @return A named NumericVector with elements: \code{mean}, \code{median},
//'   \code{quantile}.
//' @export
// [[Rcpp::export]]
NumericVector summary_stats_cpp(NumericVector x, double quantile_p = 0.95) {
  if (x.size() == 0)
    stop("Input vector must not be empty.");
  if (quantile_p < 0.0 || quantile_p > 1.0)
    stop("quantile_p must be between 0 and 1.");

  int n = x.size();

  // Mean
  double sum = 0.0;
  for (int i = 0; i < n; ++i) sum += x[i];
  double mean_val = sum / n;

  // Sort copy for median and quantile
  NumericVector sorted = clone(x).sort();

  double median_val;
  if (n % 2 == 0) {
    median_val = (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0;
  } else {
    median_val = sorted[n / 2];
  }

  // Quantile (type 7 — R default)
  double h     = (n - 1) * quantile_p;
  int    h_lo  = (int)std::floor(h);
  double frac  = h - h_lo;
  double quant_val;
  if (h_lo + 1 < n) {
    quant_val = sorted[h_lo] + frac * (sorted[h_lo + 1] - sorted[h_lo]);
  } else {
    quant_val = sorted[h_lo];
  }

  NumericVector result = NumericVector::create(
    Named("mean")     = mean_val,
    Named("median")   = median_val,
    Named("quantile") = quant_val
  );

  return result;
}
