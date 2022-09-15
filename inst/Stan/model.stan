functions {
  real gevt_lpdf(vector y, vector t, real t0, vector mu, real sigma, real xi) {
    int N = rows(y);
    // vector[N] mu_i = mu * (1 + delta * (t - t0));
    vector[N] lp;
    vector[N] z;


    for(i in 1:N){
      if (xi * (y[i] - mu[i])/sigma <= -1)
        reject("Parameters don't fit GEV constraints");
      else
        if (abs(xi) < 1e-15)
          z[i] = exp((mu[i] - y[i]) / sigma);
        else
          z[i] = pow(1 + 1/xi * ((y[i] - mu[i]) / sigma), -1/xi);

        lp[i] = -log(sigma) - (1 + 1/xi) * log(z[i]) - z[i];
    }
    return sum(lp);

  }
}

data {
  int N_obs;
  int N_stations;
  array[N_stations] int start;
  array[N_stations] int stop;
  real t0;

  vector[N_obs] y;
  vector[N_obs] year;
  array[N_obs] int station;
}


parameters {
  vector[N_stations] phi;
  vector[N_stations] tau;
  vector[N_stations] psi;
  vector[N_stations] gamma;

  real mu_phi;
  real<lower = 0> sigma_phi;

  real mu_tau;
  real<lower = 0> sigma_tau;

  real mu_psi;
  real<lower = 0> sigma_psi;

  real mu_gamma;
  real<lower = 0> sigma_gamma;

}

transformed parameters {
  vector<lower = 0>[N_stations] mu = exp(phi);
  vector<lower = 0>[N_stations] sigma = exp(tau - phi);
  vector[N_stations] xi;
  vector[N_stations] delta;
  vector<lower = y - sigma[station]./xi[station]>[N_obs] mu_t;

  for (i in 1:N_stations) {
    xi[i] = 1 / (1 + exp(-psi[i])) - 0.5;
    delta[i] = 0.008 * 1 / (1 + exp(-gamma[i])) - 0.004;
    mu_t[start[i]:stop[i]] = mu[i] * (1 + delta[i] * (year[start[i]:stop[i]] - t0));
  }

}


model {

  target += normal_lpdf(mu_phi | 12, 2);
  target += normal_lpdf(mu_tau | 0.6, 2);
  target += normal_lpdf(mu_psi | 4.6, 1);
  target += normal_lpdf(mu_gamma | 0.01, 1);

  target += exponential_lpdf(sigma_phi | 1);
  target += exponential_lpdf(sigma_tau | 1);
  target += exponential_lpdf(sigma_psi | 1);
  target += exponential_lpdf(sigma_gamma | 1);

  target += normal_lpdf(phi | mu_phi, sigma_phi);
  target += normal_lpdf(tau | mu_tau, sigma_tau);
  target += normal_lpdf(psi | mu_psi, sigma_psi);
  target += normal_lpdf(gamma | mu_gamma, sigma_gamma);

  for (i in 1:N_stations) {
    target += gevt_lpdf(y[start[i]:stop[i]] | year[start[i]:stop[i]], t0, mu_t[start[i]:stop[i]], sigma[i], xi[i]);
  }



}

