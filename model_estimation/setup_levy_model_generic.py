import time
import pickle
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
from scipy.stats import levy_stable
import tensorflow as tf

import bayesflow as bf

# Suppress scientific notation for floats
np.set_printoptions(suppress=True)

print(tf.__version__)

# Get rid of annoying tf warning
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'

def simulate_levy_trial(a=1, z=0.5, v=0, t=0.2, alpha=2.0, dt=0.001):
    """
    Simulates the response time for one trial from a Lévy-Flight Process
     * param a:     threshold separation (a>0)
     * param z:     relative Starting Point (0<z<1)
     * param v:     drift
     * param t:     non-decisio-time (t>0)
     * param alpha: stability parameter (0<a<2)
     * param dt:    step size for simulation (dt>0)
    
    The function returns a simulated response time. A negative sign indicates a response at the lower threshold
    """
    
    scale = np.power(dt,(1/alpha)) / np.sqrt(2)
    
    bin_size = np.int_(1/dt)  # The decision path is simulated in bins of 1 second
    start = [a*z]
    cnt = 0

    while(True):
        path = np.array(start + np.cumsum(v*dt + levy_stable.rvs(alpha, 0, scale=scale, size=bin_size)))
        if np.any(path < 0): 
            cnt = cnt + np.min(np.where(path < 0))
            return (-(t + cnt*dt)).astype(np.float32)
        
        if np.any(path > a): 
            cnt = cnt + np.min(np.where(path > a))
            return (t + cnt*dt).astype(np.float32)
        
        start = path[-1]
        cnt = cnt + bin_size

RNG = np.random.default_rng(2024)

def LF_prior():
    """
    Draws one set of marginally informative priors for the 7-parameter Levy-Flight-Model
    v1 nad v2 represent drift rates for two different stimulus types, 
    with positive and negative average slopes of evidence accumulation, respectively.
    
    The function returns a np array with all parameter values
    """
    a      = RNG.gamma(5, 1/2) + 0.1
    v      = RNG.normal(3, 3)
    t0     = RNG.gamma(1, 1/3) + 0.1
    st0    = RNG.beta(1,3) * 2*t0
    alpha  = RNG.beta (4, 2) * 2

    return np.array([a, v, t0, st0, alpha]).astype(np.float32)

PARAM_NAMES = ['a', 'v', 't', 'st', 'alpha']

prior = bf.simulation.Prior(prior_fun=LF_prior, param_names=PARAM_NAMES)

def LF_experiment(theta, n_obs=1000):
    """
    Simulates response times for one participant of a lévy-flight experiment.
     * param theta: numpy array with the parameters of the lfm (a,z,v1,v2,t,st,alpha) 
     * param n_obs: (maxiimum) number of observations
    
    The function returns a 2 by n_obs array, where the first column give the stimulus type (0,1)
    and the second column gives teh response times (negative values indicate responese at the lower threshold)
    """
    sim_data = np.zeros([n_obs,2])
    # cnd = np.random.randint(0,2,n_obs)
    sim_data[:,0] = np.zeros(n_obs)
    for i in range(n_obs):
        sim_data[i,1] = simulate_levy_trial(
                            a = theta[0], 
                            z = 0.5,
                            v = theta[1], # for a 3 condition experiment, this needs 3
                            t = np.random.uniform(theta[2] - theta[3]/2, theta[2] + theta[3]/2), 
                            alpha = theta[4]
                        )   
    return sim_data.astype(np.float32)

simulator = bf.simulation.Simulator(simulator_fun = LF_experiment)
model = bf.simulation.GenerativeModel(prior=prior, simulator=simulator, name="lfm")

summary_net = bf.networks.SetTransformer(input_dim=2, 
                                         summary_dim=21,
                                             dense_settings = {'units': 256, 'activation': 'relu'},
                                         num_dense_fc = 3,
                                         name="lfm_summary")

inference_net = bf.networks.InvertibleNetwork(
    num_params=len(prior.param_names),
    coupling_settings={"dense_args": dict(kernel_regularizer=None), "dropout": False},
    num_coupling_layers = 12,
    name="lfm_inference",
)

amortizer = bf.amortizers.AmortizedPosterior(inference_net, summary_net, name="lfm_amortizer")

prior_means, prior_stds = prior.estimate_means_and_stds(n_draws=100000)
prior_means = np.round(prior_means, decimals=1)
prior_stds = np.round(prior_stds, decimals=1)
print(prior_means, prior_stds)

def configurator(forward_dict, min_trials=90, max_trials=300):
    """Configure the output of the GenerativeModel for a BayesFlow setup."""

    # Prepare placeholder dict
    out_dict = {}

    # Extract simulated response times
    data = forward_dict["sim_data"]
    num_trials = np.random.randint(min_trials, max_trials + 1)
    idx = np.random.choice(range(data.shape[1]), size=num_trials, replace=False)
    data = data[:, idx, :]

    out_dict["summary_conditions"] = data.astype(np.float32)


    # Make inference network aware of varying numbers of trials
    # We create a vector of shape (batch_size, 1) by repeating the sqrt(num_obs)
    vec_num_obs = np.sqrt(num_trials) * np.ones((data.shape[0], 1))
    out_dict["direct_conditions"] = np.sqrt(vec_num_obs).astype(np.float32)

    # Get data generating parameters
    params = forward_dict["prior_draws"].astype(np.float32)

    # Standardize parameters
    out_dict["parameters"] = ((params - prior_means) / prior_stds).astype(np.float32)

    return out_dict

trainer = bf.trainers.Trainer(
    generative_model=model, 
    amortizer=amortizer, 
    configurator=configurator,
    default_lr=0.00005,
    checkpoint_path="checkpoints//generic"
)