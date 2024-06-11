var jsPsychForagePredict = (function (jspsych) {
  "use strict";

  const info = {
    name: "forage-predict",
    parameters: {
      data: {
        type: jspsych.ParameterType.OBJECT,
        default: undefined,
      },
      progress_prompt: {
        type: jspsych.ParameterType.STRING,
        default: null,
      },
    },
  };

  /**
   * **FORAGE-PREDICT**
   *
   * A jsPsych plugin for a prediction trial in the Gridworld environment
   *
   * @author [Justin Yang]
   */
  class ForagePredictPlugin {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }

    trial(display_element, trial) {
      const trial_start = Date.now();
      const sequence_uuid = UUID();
      const agent_name = gs.agent.names[trial.data.agent_type];
      const agent_hsl = convertToHSL(gs.agent.colors[trial.data.agent_type]);

      let html = `
        <div id="gridworldContainer">
          <div id="gridworldAndSteps">
            <canvas id="stepsCanvas" width="20" height="800"></canvas>
            <canvas id="gridworldCanvas" width="800" height="800"></canvas>
          </div>
          <div id="sidebar">
            <div id="instructionText">
              Click the trees in the order you think <span style="color: ${agent_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agent_name}</span> will harvest them.
            </div>
            <div id="remainingSteps"></div>
            <div id="reminderHeader"><em>Reminders</em></div>
            <div id="instructionSubtext">
              Click the <em>previous</em> tree to undo.<br>Click <span style="color: ${agent_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agent_name}</span> to reset.
            </div>
            <div id="agentReminder">
              <img id="agentImg" src="assets/img/agent_traits-overview.png" width="75%">
            </div>
            <div id="basketContainer">
              <canvas id="basketCanvas" width="300" height="200"></canvas>
              <div id="berriesCounter"></div>
            </div>
            <button id="submitBtn">Submit</button>
          </div>
        </div>
      `;

      html += `
        <div id="occluder">
          <p>Click the trees in the order you think <span style="color: ${agent_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agent_name}</span> will harvest them!</p>
        </div>
      `;

      display_element.innerHTML = html;
      if (trial.progress_prompt) {
        $('#jspsych-progressbar-container').find('span:first').html(trial.progress_prompt);
      }
      
      this.jsPsych.pluginAPI.setTimeout(() => { $('#occluder').fadeOut('fast'); }, trial.cue_duration);

      const gridworld = new Gridworld(trial.data, true);

      const basket = document.getElementById('basketContainer');
      basket.style.display = 'none';

      const submitBtn = document.getElementById('submitBtn');
      submitBtn.addEventListener('click', () => {
        const trial_end = Date.now();

        const treesData = gridworld.trees.map(tree => ({
          x: tree.x,
          y: tree.y,
          reward: tree.reward,
          isVisible: tree.isVisible,
          leaves: tree.leaves,
          berries: tree.berries
        }));

        const trial_data = {
          ..._.omit(trial, 'on_finish', 'type', 'data'),
          sequence_uuid: sequence_uuid,
          tree_trajectory: gridworld.treeTrajectory,
          coordinate_trajectory: gridworld.coordinateTrajectory,
          remaining_steps: gridworld.remainingSteps,
          click_events: gridworld.clickEvents,
          trees_data: treesData,
          trial_start: trial_start,
          trial_end: trial_end
        };

        gridworld.destroy();
        display_element.innerHTML = '';

        this.jsPsych.finishTrial(trial_data);
      });
    }
  }

  ForagePredictPlugin.info = info;

  return ForagePredictPlugin;

})(jsPsychModule);