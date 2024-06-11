from ast import literal_eval
import csv
import os
import pandas as pd


# Heuristic model globals
JSON_INPUT_DIR = "gridworld_json"
CSV_OUTPUT_DIR = "model_output"
DISCOUNT_FACTOR = 0.9
MYSTERY_TREE_MEAN_REWARD = 5

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



def get_agent_path_info(trial_name, summary):
    """
    Fetch row in `summary` dataframe corresponding to the trial in `trial_name`
    """
    return summary.loc[summary["name"] == trial_name]


def get_agent(trial_name, summary):
    """
    Agent type on this trial ('optimist' or 'pessimist')
    """
    path_info = get_agent_path_info(trial_name, summary)
    return path_info.iloc[0]["agent_type"]


def get_start_location(trial_name, summary):
    """
    Starting location of the agent on this trial
    """
    path_info = get_agent_path_info(trial_name, summary)
    return literal_eval(path_info.iloc[0]["agent_start_position"])


def get_outcome(trial_name, summary):
    """
    Binary outcome on this trial: did the agent reach the reward goal?
    """
    path_info = get_agent_path_info(trial_name, summary)
    return path_info.iloc[0]["path_reached_reward_goal"]


def get_path_reward(trial_name, summary):
    """
    Reward the agent obtained on this trial
    """
    path_info = get_agent_path_info(trial_name, summary)
    return path_info.iloc[0]["path_true_reward"]


def get_discounted_expected_reward(trial_name, summary,
                                   gamma = DISCOUNT_FACTOR,
                                   exp_reward_myst_trees = MYSTERY_TREE_MEAN_REWARD):
    """
    Average expected reward of trees in the grid, weighted by their manhattan distance from the agent
    """
    path_info = get_agent_path_info(trial_name, summary)
    start_pos = get_start_location(trial_name, summary)
    tree_visibility = literal_eval(path_info.iloc[0]["tree_visibility"])
    tree_rewards = literal_eval(path_info.iloc[0]["tree_rewards"])
    tree_positions = literal_eval(path_info.iloc[0]["tree_positions"])
    path_taken = literal_eval(path_info.iloc[0]["best_path"])

    # Update tree visibility to reflect path taken
    for loc in path_taken:
        if loc in tree_positions:
            idx = tree_positions.index(loc)
            if tree_visibility[idx] == 0: tree_visibility[idx] = 1

    numerator = 0
    count = 0
    for i in range(len(tree_rewards)):
        count += 1
        distance = abs(start_pos[0] - tree_positions[i][0]) + abs(start_pos[1] - tree_positions[i][1])
        if tree_visibility[i] == 1:
            numerator += pow(gamma, distance)*tree_rewards[i]
        elif tree_visibility[i] == 0:
            numerator += pow(gamma, distance)*exp_reward_myst_trees
    return numerator/count


def get_trial_overview(trial_name, summary):
    """
    Get a summary of the trial in `trial_name` from the `summary` dataframe
    """
    return {
        "trial_name": trial_name,
        "agent": get_agent(trial_name, summary),
        "start_location": get_start_location(trial_name, summary),
        "start_location_row": 1 if get_start_location(trial_name, summary)[0] == 1 else 10,
        "outcome": get_outcome(trial_name, summary),
        "path_true_reward": get_path_reward(trial_name, summary),
        "discounted_expected_reward": get_discounted_expected_reward(trial_name, summary)
    }

def write_to_csv(trial_summary, filename):
    """
    Write summary of trials encoded in `trial_summary` to a csv file at the path in `filename`
    """
    header = list(trial_summary[0].keys())
    vals = [[elem[key] for key in header] for elem in trial_summary]
    print(f'Writing to csv: {filename}')
    with open(filename, "w") as file:
        csv_writer = csv.writer(file, delimiter=",", quotechar= '"', quoting=csv.QUOTE_MINIMAL)
        for idx, row in enumerate(vals):
            if idx == 0:
                csv_writer.writerow(header)
            print(f'Writing data row: {row}')
            csv_writer.writerow(row)


def main():
    trial_summary = pd.read_csv(os.path.join(JSON_INPUT_DIR, "gridworld_summary.csv"))
    counterfactual_summary = [get_trial_overview(trial_name, trial_summary) for trial_name in EXPERIMENT_TRIALS]
    write_to_csv(counterfactual_summary, os.path.join(CSV_OUTPUT_DIR, "heuristic_model_trial_summary.csv"))




if __name__ == "__main__":
    main()