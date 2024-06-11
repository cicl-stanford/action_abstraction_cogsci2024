/**
 * Starts the observation of the agent's trajectory in the Gridworld.
 * @param {Gridworld} gridworld - The Gridworld instance.
 * @param {Object} data - Data containing the observed trajectory and agent start position.
 * @param {number} end_delay - Delay before submitting after the observation ends.
 */
function startObservation(gridworld, data, end_delay) {

  /**
   * Moves the agent to the next step in the observed trajectory.
   * @param {number} stepIndex - Index of the current step in the observed trajectory.
   */
  function nextStep(stepIndex) {
    if (stepIndex < data.observed_trajectory.length) {
      const currentPosition = stepIndex === 0 ? 
        [data.agent_start_position[0] - 1, data.agent_start_position[1] - 1] : 
        [data.observed_trajectory[stepIndex - 1][0] - 1, data.observed_trajectory[stepIndex - 1][1] - 1];

      const nextPosition = [
        data.observed_trajectory[stepIndex][0] - 1, 
        data.observed_trajectory[stepIndex][1] - 1
      ];

      gridworld.animateAgentMovement(currentPosition, nextPosition, gs.agent.animations.speed, false, false, () => {
        const treeIndex = gridworld.trees.findIndex(tree => tree.x === nextPosition[0] && tree.y === nextPosition[1]);

        if (treeIndex !== -1) {
          const berriesCollected = gridworld.trees[treeIndex].shakeTree(() => {
            nextStep(stepIndex + 1);
          });
          gridworld.basket.addBerries(berriesCollected);
        } else {
          nextStep(stepIndex + 1);
        }
      });
    } else {
      returnHome(stepIndex - 1);
    }
  }

  /**
   * Returns the agent to the starting position.
   * @param {number} stepIndex - Index of the current step in the observed trajectory.
   */
  function returnHome(stepIndex) {
    if (stepIndex >= 0) {
      const currentPosition = [
        data.observed_trajectory[stepIndex][0] - 1, 
        data.observed_trajectory[stepIndex][1] - 1
      ];

      const nextPosition = stepIndex === 0 ? 
        [data.agent_start_position[0] - 1, data.agent_start_position[1] - 1] : 
        [data.observed_trajectory[stepIndex - 1][0] - 1, data.observed_trajectory[stepIndex - 1][1] - 1];

      gridworld.animateAgentMovement(currentPosition, nextPosition, gs.agent.animations.return_speed, true, true, () => {
        returnHome(stepIndex - 1);
      });
    } else {
      gridworld.inObservation = false;
      setTimeout(() => {
        document.getElementById('submitBtn').click();
      }, end_delay);
    }
  }

  nextStep(0); // Start the observation
}