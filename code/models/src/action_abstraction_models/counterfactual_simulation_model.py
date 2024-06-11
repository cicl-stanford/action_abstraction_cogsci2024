import csv
import numpy as np
import os
import pandas as pd
import scipy.stats as stats
np.random.seed(100)

from gridworld_globals import TARGET_REWARD_TOTAL, STEPS
from gridworld_io import initialize_grid_world


# Simulation model globals
JSON_INPUT_DIR = "gridworld_json"
CSV_OUTPUT_DIR = "model_output"

SAMPLES = 1000
LOG_BERRY_NOISE = 0.112
LOG_STEP_NOISE = 0.030
SOFTMAX_TEMP = 0.59
EXPERIMENT_TRIALS = [
    "trial_13_v2",
    "trial_318",
    "trial_553",
    "trial_374",
    "trial_920",
    "trial_453",
    "trial_894",
    "trial_863",
    "trial_989",
    "trial_269",
    "trial_528",
    "trial_406",
    "trial_955",
    "trial_962",
    "trial_740",
    "trial_556",
    "trial_825",
    "trial_859",
    "trial_629",
    "trial_82"
]



def run_model(model, samples, log_berry_noise, log_step_noise, softmax_temp):
    print(f'Running model with parameters: \
            \n\tmodel={model} \
            \n\tsamples={samples} \
            \n\tlog_berry_noise={log_berry_noise} \
            \n\tlog_step_noise={log_step_noise} \
            \n\tsoftmax_temp={softmax_temp}')

    counterfactual_summary = [] # list of dicts, each dict is a row in the CSV representing simulations for a particular trial
    for trial_name in EXPERIMENT_TRIALS:
        print(f'\nBEGINNING TRIAL: {trial_name}')
        # Make GridWorld object from JSON file
        fp = os.path.join(JSON_INPUT_DIR, f'{trial_name}.json')
        gw = initialize_grid_world(fp)
        print(f'-> {str(gw)}')

        # Determine agent's outcome
        # TODO move to separate function
        print("Evaluating agent path...")
        decision_tree = gw.initialize_decision_tree()
        decision_tree = gw.populate_decision_tree(decision_tree)
        unique_terminal_nodes = gw.get_unique_paths(decision_tree.get_terminal_nodes())
        best_path = gw.get_best_path(unique_terminal_nodes)
        path_expected_reward = gw.get_path_expected_reward(best_path)
        path_likelihood = gw.get_path_likelihood(path_end_state=best_path[-1],
                                                 all_end_states=[elem.grid_world_state for elem in unique_terminal_nodes],
                                                 softmax_temp=softmax_temp)
        path_reward = gw.get_path_reward(best_path)
        path_reached_reward_goal = int(path_reward >= TARGET_REWARD_TOTAL)
        reward_goal_probability = gw.get_reward_goal_probability(unique_terminal_nodes, TARGET_REWARD_TOTAL, softmax_temp)

        # Use agent's best path to revise visibility of any mystery trees harvested on the path
        # Counterfactual reward estimates below will then factor in true reward for these trees rather than sampling from uniform distribution
        best_path_final_state = best_path[-1]
        best_path_harvested_mystery_trees = np.multiply(np.array(best_path_final_state.harvested_trees), ~np.array(best_path_final_state.tree_visibility)+2)
        updated_tree_visibility = np.array(best_path_final_state.tree_visibility) | best_path_harvested_mystery_trees

        # Summarize agent path
        print(f'-> Trial summary:\
                \n\tbest path: {[elem.agent_position for elem in best_path]}\
                \n\texpected reward: {path_expected_reward}\
                \n\tpath likelihood: {path_likelihood}\
                \n\ttrue reward: {path_reward}\
                \n\treached goal: {path_reached_reward_goal}\
                \n\treward goal probability: {reward_goal_probability}')

        print(f'Evaluating counterfactuals with {samples} samples...')
        counterfactual_start_outcome_change = 0
        counterfactual_agent_outcome_change = 0
        position_counterfactual_terminal_states = {}
        position_counterfactual_likelihoods = {}
        agent_counterfactual_terminal_states = {}
        agent_counterfactual_likelihoods = {}

        for _ in range(samples):
            # START POSITION COUNTERFACTUAL
            # Sample steps from *truncated* log normal around expected number
            # NB: not currently seeding these
            step_sample = int(round(np.power(10, stats.truncnorm(
                a=(np.log10(8) - np.log10(STEPS)) / log_step_noise,
                b=(np.log10(12) - np.log10(STEPS)) / log_step_noise,
                loc=np.log10(STEPS),
                scale=log_step_noise).rvs(1)[0]), 0))

            if step_sample not in position_counterfactual_terminal_states:
                cf_gw_cf_start = initialize_grid_world(fp, steps = step_sample)
                cf_gw_cf_start.agent_start_position = gw.get_counterfactual_start()
                cf_decision_tree_cf_start = cf_gw_cf_start.initialize_decision_tree()
                cf_decision_tree_cf_start = cf_gw_cf_start.populate_decision_tree(cf_decision_tree_cf_start)
                terminal_states = [elem.grid_world_state for elem in cf_gw_cf_start.get_unique_paths(cf_decision_tree_cf_start.get_terminal_nodes())]
                # Calculate softmax probability of each terminal state above based on agent's expected reward
                likelihoods = [cf_gw_cf_start.get_path_likelihood(path_end_state=elem,
                                                                  all_end_states=terminal_states,
                                                                  softmax_temp=softmax_temp) for elem in terminal_states]
                position_counterfactual_terminal_states[step_sample] = terminal_states
                position_counterfactual_likelihoods[step_sample] = likelihoods
            else:
                terminal_states = position_counterfactual_terminal_states[step_sample]
                likelihoods = position_counterfactual_likelihoods[step_sample]

            # Sample a terminal state from `terminal_states` in proportion to softmax probability in `likelihoods`
            sample_end_state = terminal_states[np.random.choice(a=len(terminal_states), p=likelihoods)]
            # Update mystery tree visibility in sampled end state based on agent's original path
            sample_end_state.tree_visibility = updated_tree_visibility

            # Calculate total reward estimate for sampled end state (using noisy representation of visible tree rewards)
            visible_rewards = sample_end_state.get_noisy_visible_tree_rewards(log_berry_noise)
            # Remaining mystery tree values sampled from uniform distribution
            sample_mystery_rewards = sample_end_state.get_mystery_tree_sample_rewards()
            sample_reward = visible_rewards + sample_mystery_rewards
            # Update count of sampled paths in which outcome changes
            counterfactual_start_outcome_change += int(sample_reward < TARGET_REWARD_TOTAL) if path_reached_reward_goal else int(sample_reward >= TARGET_REWARD_TOTAL)

            # AGENT COUNTERFACTUAL
            # Sample steps from *truncated* log normal around expected number
            step_sample = int(round(np.power(10, stats.truncnorm(
                a=(np.log10(8) - np.log10(STEPS)) / log_step_noise,
                b=(np.log10(12) - np.log10(STEPS)) / log_step_noise,
                loc=np.log10(STEPS),
                scale=log_step_noise).rvs(1)[0]), 0))
            if step_sample not in agent_counterfactual_terminal_states:
                cf_gw_cf_agent = initialize_grid_world(fp, steps = step_sample)
                cf_gw_cf_agent.agent = gw.get_counterfactual_agent()
                cf_decision_tree_cf_agent = cf_gw_cf_agent.initialize_decision_tree()
                cf_decision_tree_cf_agent = cf_gw_cf_agent.populate_decision_tree(cf_decision_tree_cf_agent)
                terminal_states = [elem.grid_world_state for elem in cf_gw_cf_agent.get_unique_paths(cf_decision_tree_cf_agent.get_terminal_nodes())]
                # Calculate softmax probability of each terminal state above based on agent's expected reward (no noise in visible tree reward)
                likelihoods = [cf_gw_cf_start.get_path_likelihood(path_end_state=elem,
                                                                  all_end_states=terminal_states,
                                                                  softmax_temp=softmax_temp) for elem in terminal_states]
                agent_counterfactual_terminal_states[step_sample] = terminal_states
                agent_counterfactual_likelihoods[step_sample] = likelihoods
            else:
                terminal_states = agent_counterfactual_terminal_states[step_sample]
                likelihoods = agent_counterfactual_likelihoods[step_sample]

            # Sample a terminal state from `terminal_states` in proportion to softmax probability in `likelihoods`
            sample_end_state = terminal_states[np.random.choice(a=len(terminal_states), p=likelihoods)]
            # Update mystery tree visibility in sampled end state based on agent's original path
            sample_end_state.tree_visibility = updated_tree_visibility

            # Calculate total reward estimate for sampled end state (using noisy representation of visible tree rewards)
            visible_rewards = sample_end_state.get_noisy_visible_tree_rewards(log_berry_noise)
            # Remaining mystery tree values sampled from uniform distribution
            sample_mystery_rewards = sample_end_state.get_mystery_tree_sample_rewards()
            sample_reward = visible_rewards + sample_mystery_rewards
            # Update count of sampled paths in which outcome changes
            counterfactual_agent_outcome_change += int(sample_reward < TARGET_REWARD_TOTAL) if path_reached_reward_goal else int(sample_reward >= TARGET_REWARD_TOTAL)

        # Summarize simulations above
        print(f'-> Counterfactual START outcome changes: {counterfactual_start_outcome_change} ({round(counterfactual_start_outcome_change/samples, 2)})')
        print(f'-> Counterfactual AGENT outcome changes: {counterfactual_agent_outcome_change} ({round(counterfactual_agent_outcome_change/samples, 2)})')
        counterfactual_summary.append({
            "model": model,
            "trial_name": trial_name,
            "agent_type": gw.agent.type,
            "start_position": gw.agent_start_position,
            "path_reached_reward_goal": path_reached_reward_goal,
            "path_reward": path_reward,
            "path_likelihood": path_likelihood,
            "reward_goal_probability": reward_goal_probability,
            "counterfactual_start_outcome_change_probability": counterfactual_start_outcome_change / samples,
            "counterfactual_agent_outcome_change_probability": counterfactual_agent_outcome_change / samples,
            "model_samples": samples,
            "log_berry_noise": log_berry_noise,
            "log_step_noise": log_step_noise,
            "softmax_temp": softmax_temp,
        })

    # Write to CSV
    # TODO move to separate function
    header = list(counterfactual_summary[0].keys())
    vals = [[elem[key] for key in header] for elem in counterfactual_summary]
    output_filename = f'cf_model_{model}_samples_{samples}_berry_noise_{log_berry_noise}_step_noise_{log_step_noise}_softmax_{softmax_temp}.csv'
    csvfile = os.path.join(CSV_OUTPUT_DIR, output_filename)
    print(f'Writing to csv: {csvfile}')
    with open(csvfile, "a") as file:
        csv_writer = csv.writer(file, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL)
        # Write vals to CSV
        for idx, row in enumerate(vals):
            if idx == 0:
                csv_writer.writerow(header)
            print(f'Writing data row: {row}')
            csv_writer.writerow(row)



if __name__ == "__main__":
    run_model(model='default', samples=SAMPLES, log_berry_noise=LOG_BERRY_NOISE, log_step_noise=LOG_STEP_NOISE, softmax_temp=SOFTMAX_TEMP)