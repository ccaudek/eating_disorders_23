// Estimation of Rescorla-Wagner parameters (distinguishing between alpha_pos)
// and alpha_neg). The estimation is subject-by-subject, NOT hierarchical.
// The data list, n_trials provides a vector with a different number of trials 
// for each subject. The maximum number of trials is 320.  If a subject has a 
// number of trials smaller than 320, say 200, then the elements from 201 to 
// 320 will be zeros.
//
// The code is written by Lei Zhang (BayesCog Wien). I changed the fact that
// the number of trials can differ among subjects. The number 320 is hard-coded
// because, for the ED-Montecatini data, it represents the maximum possible
// number of trials for each subject.
//
// Last Modified Date: Sat Oct  9 07:40:27 2021

data {
  int<lower=1> n_subjects;
  int<lower=1> n_trials[n_subjects];
  int<lower=0,upper=2> choice[n_subjects, 320];     
  real<lower=-1, upper=1> reward[n_subjects, 320]; 
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

    for (t in 1:n_trials[s]) {        
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



