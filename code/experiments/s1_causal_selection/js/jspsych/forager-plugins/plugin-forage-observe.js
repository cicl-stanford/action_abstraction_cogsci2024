var jsPsychForageObserve = (function(jspsych) {
  "use strict";

  const info = {
    name: "forage-observe",
    parameters: {
      data: {
        type: jspsych.ParameterType.OBJECT,
        default: undefined,
      },
      sequence_uuid: {
        type: jspsych.ParameterType.STRING,
        default: undefined,
      },
      tree_trajectory: {
        type: jspsych.ParameterType.OBJECT,
        default: null,
      },
      coordinate_trajectory: {
        type: jspsych.ParameterType.OBJECT,
        default: null,
      },
      trees_data: {
        type: jspsych.ParameterType.OBJECT,
        default: null,
      },
      berries_needed: {
        type: jspsych.ParameterType.INT,
        default: 0,
      },
      cue_duration: {
        type: jspsych.ParameterType.INT,
        default: 1500,
      },
      start_delay: {
        type: jspsych.ParameterType.INT,
        default: 1000,
      },
      end_delay: {
        type: jspsych.ParameterType.INT,
        default: 1000,
      },
      progress_prompt: {
        type: jspsych.ParameterType.STRING,
        default: null,
      },
    },
  };

  /**
   * **FORAGE-OBSERVE**
   *
   * A jsPsych plugin for an observation trial in the Gridworld environment
   *
   * @author [Justin Yang]
   */
  class ForageObservePlugin {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }

    trial(display_element, trial) {
      const trial_start = Date.now();
      const agent_name = gs.agent.names[trial.data.agent_type];
      const agent_hsl = convertToHSL(gs.agent.colors[trial.data.agent_type]);
      const follow_up_trial = trial.trees_data !== null;

      let html = `
        <div id="gridworldContainer">
          <div id="gridworldAndSteps">
            <canvas id="stepsCanvas" width="20" height="800"></canvas>
            <canvas id="gridworldCanvas" width="800" height="800"></canvas>
          </div>
          <div id="sidebar">
            <div id="instructionText">
              ${follow_up_trial ? 'Now watch' : 'Watch'} the path <br><span style="color: ${agent_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agent_name}</span> took!
            </div>
            <div id="basketContainer">
              <canvas id="basketCanvas" width="300" height="200"></canvas>
              <div id="berriesCounter"></div>
            </div>
            <button id="submitBtn">Continue</button>
          </div>
        </div>
      `;

      html += `
        <div id="occluder">
          <p>${follow_up_trial ? 'Now watch' : 'Watch'} the path <span style="color: ${agent_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agent_name}</span> took!</p>
        </div>
      `;

      display_element.innerHTML = html;
      if (trial.progress_prompt) {
        $('#jspsych-progressbar-container').find('span:first').html(trial.progress_prompt);
      }

      // Initialize the Gridworld with the trial data
      const gridworld = new Gridworld(trial.data, false, trial.trees_data);
      if (trial.tree_trajectory) gridworld.treeTrajectory = trial.tree_trajectory;
      if (trial.coordinate_trajectory) gridworld.coordinateTrajectory = trial.coordinate_trajectory;
      gridworld.basket.berriesNeeded = trial.berries_needed;
      gridworld.basket.updateCounter();

      this.jsPsych.pluginAPI.setTimeout(() => {
        $('#occluder').fadeOut('fast');
        this.jsPsych.pluginAPI.setTimeout(() => {
          startObservation(gridworld, trial.data, trial.end_delay);
        }, trial.start_delay);
      }, trial.cue_duration);

      // Submit button logic
      const submitBtn = document.getElementById('submitBtn');
      submitBtn.addEventListener('click', () => {
        const trial_end = Date.now();

        // Collect data from the Gridworld
        const trial_data = {
          ..._.omit(trial, 'on_finish', 'type', 'data'),
          actual_tree_trajectory: gridworld.actualTreeTrajectory,
          actual_trajectory: gridworld.actualTrajectory,
          berries_collected: gridworld.basket.berriesCollected,
          berries_needed: gridworld.basket.berriesNeeded,
          outcome: gridworld.basket.berriesCollected >= gridworld.basket.berriesNeeded,
          trial_start: trial_start,
          trial_end: trial_end,
          gridworld: gridworld
        };

        gridworld.inObservation = true;
        if (trial.post_trial_gap) {
          gridworld.destroy();
          display_element.innerHTML = "";
        }

        // End the trial
        this.jsPsych.finishTrial(trial_data);
      });
    }
  }

  ForageObservePlugin.info = info;

  return ForageObservePlugin;

})(jsPsychModule);