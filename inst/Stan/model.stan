data {
  int N_stations;
  vector[N_stations] psi;
  vector[N_stations] tau;
  vector[N_stations] phi;
  vector[N_stations] gamma;

  matrix[N_stations, N_stations] Q_u;

}


parameters {
  real<lower = 0> tau_psi;
  real<lower = 0> tau_tau;
  real<lower = 0> tau_phi;
  real<lower = 0> tau_gamma;
}

transformed parameters {


}


model {
matrix[N_stations, N_stations] Q_u_psi = tau_psi * Q_u;
matrix[N_stations, N_stations] Q_u_tau = tau_tau * Q_u;
matrix[N_stations, N_stations] Q_u_phi = tau_phi * Q_u;
matrix[N_stations, N_stations] Q_u_gamma = tau_gamma * Q_u;

target += exponential_lpdf(tau_psi | 1);
target += exponential_lpdf(tau_tau | 1);
target += exponential_lpdf(tau_phi | 1);
target += exponential_lpdf(tau_gamma | 1);

target += multi_normal_prec_lpdf(psi | rep_vector(0, N_stations), Q_u_psi);
target += multi_normal_prec_lpdf(tau | rep_vector(0, N_stations), Q_u_tau);
target += multi_normal_prec_lpdf(phi | rep_vector(0, N_stations), Q_u_phi);
target += multi_normal_prec_lpdf(gamma | rep_vector(0, N_stations), Q_u_gamma);

}




