// 
data {
  int<lower=1> n_subjects;
  int<lower=1> n_trials;
  int<lower=1,upper=2> choice[n_subjects, n_trials];     
  real<lower=-1, upper=1> reward[n_subjects, n_trials]; 
}

transformed data {
  vector[2] initV;  // initial values for V
  initV = rep_vector(0.0, 2);
}

parameters {
  real<lower=0,upper=1> alpha_neg[n_subjects];
  real<lower=0,upper=1> alpha_pos[n_subjects];
  real<lower=0,upper=3> tau[n_subjects];  
}

model {
  for (s in 1:n_subjects) {
    vector[2] v; 
    real pe;    
    v = initV;

    for (t in 1:n_trials) {        
      choice[s, t] ~ categorical_logit(tau[s] * v);
      pe = reward[s, t] - v[choice[s, t]];   
      
      if (reward[s, t] > 0) {
        v[choice[s, t]] = v[choice[s, t]] + alpha_pos[s] * pe; 
      } else {
        v[choice[s, t]] = v[choice[s, t]] + alpha_neg[s] * pe; 
      }
    }
  }    
}



