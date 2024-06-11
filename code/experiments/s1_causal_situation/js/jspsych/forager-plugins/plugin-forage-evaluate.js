var jsPsychForageEvaluate = (function(jspsych) {
  "use strict";

  const info = {
    name: "forage-evaluate",
    parameters: {
      data: {
        type: jspsych.ParameterType.OBJECT,
        default: undefined,
      },
      sequence_uuid: {
        type: jspsych.ParameterType.STRING,
        default: undefined,
      },
      questions: {
        type: jspsych.ParameterType.STRING,
        default: "How optimistic is this agent?",
      },
      slider_min: {
        type: jspsych.ParameterType.STRING,
        default: "Pessimistic",
      },
      slider_max: {
        type: jspsych.ParameterType.STRING,
        default: "Optimistic",
      },
      cue_duration: {
        type: jspsych.ParameterType.INT,
        default: 1500,
      },
      berries_collected: {
        type: jspsych.ParameterType.INT,
        default: undefined,
      },
      gridworld: {
        type: jspsych.ParameterType.OBJECT
      }
    },
  };

  /**
   * **FORAGE-EVALUATE**
   *
   * A jsPsych plugin for an evaluation trial in the Gridworld environment.
   *
   * @author [Justin Yang]
   */
  class ForageEvaluatePlugin {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }

    trial(display_element, trial) {
      const trial_start = Date.now();
      const outcome = trial.berries_collected >= gs.game_info.berries_needed;

      const agentOpposite = trial.data.agent_type === 'optimist' ? 'pessimist' : 'optimist';
      const agentName = gs.agent.names[trial.data.agent_type];
      const agentHSL = convertToHSL(gs.agent.colors[trial.data.agent_type]);
      const agentText = `<span style="color: ${agentHSL}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agentName}</span>`;
      const agentOppositeName = gs.agent.names[agentOpposite];
      const agentOppositeHSL = convertToHSL(gs.agent.colors[agentOpposite]);
      const agentOppositeText = `<span style="color: ${agentOppositeHSL}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${agentOppositeName}</span>`;
      
      const startName = (trial.data.agent_start_position[0] === 1 && trial.data.agent_start_position[1] === 1) ? 'NORTH' : 'SOUTH';
      const startOpposite = startName === 'NORTH' ? 'SOUTH' : 'NORTH';
      const hslKeyStart = (trial.data.agent_start_position[0] === 1 && trial.data.agent_start_position[1] === 1) ? 'Ncorner' : 'Scorner';
      const hslKeyStartOpposite = hslKeyStart === 'Ncorner' ? 'Scorner' : 'Ncorner';
      const hslStart = convertToHSL(gs.tile.colors[hslKeyStart]);
      const hslStartOpposite = convertToHSL(gs.tile.colors[hslKeyStartOpposite]);
      const startText = `<span style="color: ${hslStart}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${startName}</span>`;
      const startOppositeText = `<span style="color: ${hslStartOpposite}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${startOpposite}</span>`;
      const startSliderText = `<span style="color: ${hslStart}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">START</span>`;

      const slider_min = trial.slider_min.replace('{AGENT}', agentText).replace('{START}', startSliderText);
      const slider_max = trial.slider_max.replace('{AGENT}', agentText);

      // Set up the slider container
      const sliderContainer = document.createElement('div');
      sliderContainer.id = 'evalSliderContainer';
      sliderContainer.style = 'width: 90%; margin-top: auto; padding: 0px 20px 10px 20px; background-color: #fff; border-radius: 10px; text-align: center;';
      sliderContainer.innerHTML = gs.session_info.condition == 'causal_selection' ? '' : `<p style="font-size: 20px;">To what extent do you agree with the following:</p>`;

      trial.questions.forEach((question, index) => {
        const formattedQuestion = question
          .replace('{AGENT}', agentText)
          .replace('{!AGENT}', agentOppositeText)
          .replace('{START}', startText)
          .replace('{!START}', startOppositeText);
        
        sliderContainer.innerHTML += `
          <p style="font-size: 18px; margin-top: 10px; margin-bottom: 10px;">${formattedQuestion}</p>
          <div id="evalSlider${index}" class="evalSlider" style="width: 100%; margin-bottom: 10px;"></div>
          <div style="display: flex; justify-content: space-between;">
            <span>${slider_min}</span>
            <span>${slider_max}</span>
          </div>
        `;
      });

      // Submit button
      sliderContainer.innerHTML += `<button id="submitEval" class="jspsych-btn" disabled>Submit</button>`;

      // Append the slider container to the sidebar
      const sidebar = document.getElementById('sidebar');
      const harvestOutcome = document.createElement('div');
      const occluder = document.getElementById('occluder');

      const outcomeHTML = outcome ? `Hooray! ${agentText} got ${trial.berries_collected} berries.` : `Oh no! ${agentText} only got ${trial.berries_collected} berries.`;
      const instructionText = `Now, it\'s time to evaluate ${agentText}\'s harvest.`;

      harvestOutcome.id = 'harvestOutcome';
      harvestOutcome.innerHTML = outcomeHTML;

      occluder.innerHTML = `<span>${outcomeHTML}</span><br><span>${instructionText}</span>`;
      occluder.style.display = null;
      this.jsPsych.pluginAPI.setTimeout(() => { $('#occluder').fadeOut('fast'); }, trial.cue_duration);

      document.getElementById('instructionText').innerHTML = instructionText;
      document.getElementById('submitBtn').remove();

      sidebar.appendChild(sliderContainer);
      sidebar.prepend(harvestOutcome);

      document.getElementById('basketContainer').style.marginTop = trial.questions.length > 1 ? '-80px' : 'auto';

      // Initialize the sliders
      this.initializeSliders(trial.questions.length);

      // Button event listener
      const submitBtn = document.getElementById('submitEval');
      submitBtn.addEventListener('click', () => {
        const trial_end = Date.now();
        const response = trial.questions.map((_, i) => $(`#evalSlider${i}`).slider('option', 'value'));

        const trial_data = {
          ..._.omit(trial, 'on_finish', 'type', 'data'),
          rating: response,
          trial_start: trial_start,
          trial_end: trial_end
        };

        trial.gridworld.destroy();
        display_element.innerHTML = "";

        // End the trial and save the data
        this.jsPsych.finishTrial(trial_data);
      });
    }

    initializeSliders(numSliders) {
      const slidersMoved = Array(numSliders).fill(false);

      for (let i = 0; i < numSliders; i++) {
        $(`#evalSlider${i}`).slider({
          min: 0,
          max: 100,
          create: function() {
            $(this).find('.ui-slider-handle').hide();
          }
        }).slider("pips", {
          first: "pip",
          last: "pip",
          step: 100
        });

        $(`#evalSlider${i}`).on('slidestart', function() {
          $(this).find('.ui-slider-handle').show();
          slidersMoved[i] = true;
          if (slidersMoved.every(moved => moved)) {
            $('#submitEval').prop('disabled', false);
          }
        });
      }
    }
  }

  ForageEvaluatePlugin.info = info;

  return ForageEvaluatePlugin;

})(jsPsychModule);
