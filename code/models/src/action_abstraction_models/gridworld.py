import numpy as np
import scipy.stats as stats
from gridworld_globals import AGENTS, AGENT_MYSTERY_TREE_EXPECTED_REWARD, MAX_REWARD, MIN_REWARD
from utils import get_agent_start_positions


"""
GridWorld object defined to hold all static information about a grid world
"""
class GridWorld:
    def __init__(self,
                 name,
                 rows,
                 cols,
                 agent,
                 agent_start_position,
                 tree_positions,
                 tree_rewards,
                 tree_visibility,
                 steps
                 ):
        self.name = name
        self.rows = rows
        self.cols = cols
        self.agent = agent
        self.agent_start_position = agent_start_position
        self.tree_positions = tree_positions
        self.tree_rewards = tree_rewards
        self.tree_visibility = tree_visibility
        self.steps = steps



    def __str__(self):
        """Yields string representation of GridWorld object"""
        return (
            f'GridWorld object: {self.name} \
            \n\trows: {self.rows} \
            \n\tcols: {self.cols} \
            \n\tagent: {self.agent} \
            \n\tagent_start_position: {self.agent_start_position} \
            \n\ttree_positions: {self.tree_positions} \
            \n\ttree_rewards: {self.tree_rewards} \
            \n\ttree_visibility: {self.tree_visibility} \
            \n\tsteps: {self.steps}'
        )

    def initialize_decision_tree(self):
        state = GridWorldState(rows=self.rows,
                              cols=self.cols,
                              agent=self.agent,
                              tree_positions=self.tree_positions,
                              tree_rewards=self.tree_rewards,
                              tree_visibility=self.tree_visibility,
                              agent_position=self.agent_start_position,
                              steps_remaining=self.steps,
                              harvested_trees=np.zeros(len(self.tree_rewards)).astype(int))
        return GridWorldNode(grid_world_state=state,
                             parent_node=None,
                             child_nodes=[])

    def populate_decision_tree(self, root_node):
        root_node.visit_node()
        return root_node

    # NB: takes in list of GridWorldNode objects, returns list of GridWorldNode objects
    def get_unique_paths(self, terminal_nodes):
        unique_nodes = {}
        for node in terminal_nodes:
            state_str = node.grid_world_state.id()
            if state_str not in unique_nodes.keys():
                unique_nodes[state_str] = node
        return unique_nodes.values()

    # NB: takes in list of GridWorldNode objects, finds the one with the highest expected reward,
    # returns list of GridWorldState objects in the path to the highest reward terminal node
    def get_best_path(self, terminal_nodes):
        return max(terminal_nodes, key=lambda node: node.grid_world_state.get_expected_reward()).get_path()

    # NB: takes in list of GridWorldState objects as a path, returns expected reward of path
    def get_path_expected_reward(self, path):
        if len(path) == 0: return 0
        else: return path[-1].get_expected_reward()

    # NB: takes in list of GridWorldState objects as a path, returns true reward of path
    def get_path_reward(self, path):
        if len(path) == 0: return 0
        else: return path[-1].get_total_reward()

    def get_path_likelihood(self, path_end_state, all_end_states, softmax_temp):
        return np.exp(path_end_state.get_expected_reward()/softmax_temp) / (np.sum([np.exp(state.get_expected_reward()/softmax_temp) for state in all_end_states]))

    def get_noisy_path_likelihoods(self, end_states, log_noise, softmax_temp):
        noisy_expected_rewards = [state.get_noisy_expected_reward(log_noise) for state in end_states]
        return [np.exp(val/softmax_temp) / (np.sum([np.exp(reward/softmax_temp) for reward in noisy_expected_rewards])) for val in noisy_expected_rewards]

    def get_reward_goal_probability(self, terminal_nodes, reward_goal, softmax_temp):
        # Sum of likelihood of paths that *would* reach reward goal
        # NB: this is different from the proportion of available paths that do in fact reach the goal
        return np.sum([self.get_path_likelihood(node.grid_world_state, [elem.grid_world_state for elem in terminal_nodes], softmax_temp) for node in terminal_nodes if node.grid_world_state.get_total_reward() >= reward_goal])

    def get_counterfactual_agent(self):
        # NB: this only works with 2 agents
        return GridWorldAgent(type=list(set(AGENTS) - set([self.agent.type]))[0])

    def get_counterfactual_start(self):
        # NB: this only works with 2 start positions
        return list(filter(lambda elem: elem != self.agent_start_position, get_agent_start_positions(self.rows, self.cols)))[0]


class GridWorldAgent:
    def __init__(self, type):
        if type not in AGENTS:
            raise Exception("Agent type must be one of: {}".format(AGENTS))
        self.type = type

    def __str__(self):
        return f'GridWorldAgent object: {self.type}'

    def get_mystery_tree_reward(self):
        return AGENT_MYSTERY_TREE_EXPECTED_REWARD[self.type]




class GridWorldNode:
    def __init__(self,
                 grid_world_state,
                 parent_node,
                 child_nodes):
        self.grid_world_state = grid_world_state
        self.parent_node = parent_node
        self.child_nodes = child_nodes

    def __str__(self):
        return f'GridWorldNode object has grid_world_state: {[self.grid_world_state,]} \
            \nGridWorldNode object has parent_node: {self.parent_node} \
            \nGridWorldNode object has child_nodes: {self.child_nodes}'

    def visit_node(self):
        child_states = self.grid_world_state.get_next_states()
        for state in child_states:
            self.child_nodes.append(GridWorldNode(grid_world_state=state,
                                                  parent_node=self,
                                                  child_nodes=[]))
        for node in self.child_nodes:
            node.visit_node()

    def count_child_nodes(self):
        n_nodes = 1
        for child_node in self.child_nodes:
            n_nodes += child_node.count_child_nodes()
        return n_nodes

    def count_terminal_nodes(self):
        n_nodes = 0
        for child_node in self.child_nodes:
            n_nodes += child_node.count_terminal_nodes()
        if self.is_terminal_node():
            n_nodes += 1
        return n_nodes

    def get_terminal_nodes(self):
        terminal_nodes = []
        for child_node in self.child_nodes:
            terminal_nodes += child_node.get_terminal_nodes()
        if self.is_terminal_node():
            terminal_nodes.append(self)
        return terminal_nodes

    def is_terminal_node(self):
        return len(self.child_nodes) == 0

    def get_path(self):
        path = []
        state = self
        while state.parent_node:
            path.append(state.grid_world_state)
            state = state.parent_node
        path.append(state.grid_world_state)
        path.reverse()
        return path



class GridWorldState:
    def __init__(self,
                 # static
                 rows,
                 cols,
                 agent,
                 tree_positions,
                 tree_rewards,
                 tree_visibility,
                 # dynamic (these are updated at each step)
                 agent_position,
                 steps_remaining,
                 harvested_trees):
        self.rows = rows
        self.cols = cols
        self.agent = agent
        self.tree_positions = tree_positions
        self.tree_rewards = tree_rewards
        self.tree_visibility = tree_visibility
        self.agent_position = agent_position
        self.steps_remaining = steps_remaining
        self.harvested_trees = harvested_trees


    def __str__(self):
        return f'GridWorldState object {[self,]}: \
            \n\trows: {self.rows} \
            \n\tcols: {self.cols} \
            \n\tagent: {self.agent} \
            \n\ttree_positions: {self.tree_positions} \
            \n\ttree_rewards: {self.tree_rewards} \
            \n\ttree_visibility: {self.tree_visibility} \
            \n\tagent_position: {self.agent_position} \
            \n\tsteps_remaining: {self.steps_remaining} \
            \n\tharvested_trees: {self.harvested_trees}'


    # TODO is there a more elegant way to do this? Seems like `hash` function can be used here?
    def id(self):
        # Exclude agent's position since we don't really care about differences in where the agent is
        # if they've harvested the same trees and the trial is the same otherwise
        return f'rows: {self.rows}, \
                cols: {self.cols}, \
                agent: {self.agent.type}, \
                tree_positions: {self.tree_positions}, \
                tree_rewards: {self.tree_rewards}, \
                tree_visibility: {self.tree_visibility}, \
                steps_remaining: {self.steps_remaining}, \
                harvested_trees: {self.harvested_trees}'


    def get_move_options(self):
        if self.steps_remaining > 0:
            return [[self.agent_position[0], self.agent_position[1]+1], # up
                    [self.agent_position[0]+1, self.agent_position[1]], # right
                    [self.agent_position[0], self.agent_position[1]-1], # down
                    [self.agent_position[0]-1, self.agent_position[1]]] # left
        else:
            return []


    def get_next_states(self):
        child_states = [] # TODO consider returning list comprehension with just valid positions
        for position in self.get_move_options():
            # Check if move is within grid bounds
            if (position[0] >= 1 and position[0] <= self.rows) and (position[1] >= 1 and position[1] <= self.cols):
                # Create GridWorldState object for each valid move option and append to child_states
                tree_harvest = self.harvested_trees.copy()
                if position in self.tree_positions:
                    tree_harvest[self.tree_positions.index(position)] = 1
                child_states.append(GridWorldState(rows=self.rows,
                                                    cols=self.cols,
                                                    agent=self.agent,
                                                    tree_positions=self.tree_positions,
                                                    tree_rewards=self.tree_rewards,
                                                    tree_visibility=self.tree_visibility,
                                                    agent_position=position,
                                                    steps_remaining=self.steps_remaining-1,
                                                    harvested_trees=tree_harvest))
        return child_states


    def get_total_reward(self):
        return np.sum(np.multiply(self.harvested_trees, self.tree_rewards))

    def get_visible_tree_rewards(self):
        return np.sum(np.multiply(np.multiply(np.array(self.harvested_trees), np.array(self.tree_visibility)), np.array(self.tree_rewards)))

    def get_noisy_visible_tree_rewards(self, log_noise):
        # Sample reward values of all trees from a log normal distribution around the true (log transformed) value with `log_noise` SD
        noisy_rewards = np.array([
            int(round(np.power(10, stats.truncnorm(
                a=(np.log10(MIN_REWARD) - np.log10(elem)) / log_noise,
                b=(np.log10(MAX_REWARD) - np.log10(elem)) / log_noise,
                loc=np.log10(elem),
                scale=log_noise).rvs(1)[0]
            ), 0)) for elem in self.tree_rewards
        ])
        # Return the sum of noisy sampled values for visible trees only
        self.noisy_visible_rewards = np.sum(np.multiply(np.multiply(np.array(self.harvested_trees), np.array(self.tree_visibility)), noisy_rewards))
        return self.noisy_visible_rewards

    def get_mystery_tree_expected_rewards(self):
        # Source for inverting visible tree array: https://stackoverflow.com/questions/56594598/change-1s-to-0-and-0s-to-1-in-numpy-array-without-looping
        return np.sum(np.multiply(np.array(self.harvested_trees), ~np.array(self.tree_visibility)+2)*self.agent.get_mystery_tree_reward())

    def get_expected_reward(self):
        # Add sum of harvested visible tree rewards and harvested mystery tree *expected* rewards
        return self.get_visible_tree_rewards() + self.get_mystery_tree_expected_rewards()

    def get_noisy_expected_reward(self, log_noise):
        # Sample visible tree rewards from log normal distribution around true value
        return self.get_noisy_visible_tree_rewards(log_noise) + self.get_mystery_tree_expected_rewards()


    def get_mystery_tree_sample_rewards(self):
        # TODO make this a function of the agent class to match the `get_mystery_tree_reward` function for the agent?
        num_trees = np.sum(np.multiply(np.array(self.harvested_trees), ~np.array(self.tree_visibility)+2))
        return np.sum(np.random.choice(a=np.arange(start=MIN_REWARD, stop=MAX_REWARD+1), size=num_trees, replace=True))



