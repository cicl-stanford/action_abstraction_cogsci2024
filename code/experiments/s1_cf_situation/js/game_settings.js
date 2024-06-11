gs = {
  study_metadata: {
    dbname : 'action_abstraction',
    colname : 's2_causal_selection',
    iteration_name : 'demo',
    dev_mode : true,
  },
  session_data: {
    startInstructionTS: undefined,
    startPracticeTS: undefined,
    startExperimentTS: undefined,
    endExperimentTS: undefined,
    comprehensionAttempts: 0
  },
  session_info: {
    gameID: undefined,
    condition: 'trait_causal', // environment_counterfactual, trait_counterfactual, environment_causal, trait_causal, causal_selection
    on_finish: undefined,
  },
  prolific_info: {
    prolificID: undefined,
    prolificStudyID: undefined,
    prolificSessionID: undefined,
  },
  game_info: {
    berries_needed: 20,
    cue_duration: 1500,
    start_delay: 1000,
    end_delay: 500,
  },
  agent: {
    names: {
      optimist: 'HOPE',
      pessimist: 'PRUDENCE'
    },
    colors: {
      optimist: { h: 50, s: 100, l: 50 },
      pessimist: { h: 275, s: 100, l: 50 },
      optimist_hover: { h: 50, s: 100, l: 30 },
      pessimist_hover: { h: 275, s: 100, l: 30 }
    },
    size: {
      radius: 0.33,
      eye_radius: 0.3,
      pupil_radius: 0.5,
      smile_radius: 0.33,
    },
    animations: {
      speed: 15, // number of frames to travel 1 tile
      return_speed: 12,
      hover: {
        duration: 200,
      }
    }
  },
  tree: {
    colors: {
      visible: { h: 96, s: 42, l: 36 },
      invisible: { h: 144, s: 100, l: 12 },
      unreachable: { h: 0, s: 0, l: 30 },
      optimist_border: { h: 50, s: 100, l: 50 },
      pessimist_border: { h: 275, s: 100, l: 50 },
      optimist_border_predict: { h: 50, s: 5, l: 50 },
      pessimist_border_predict: { h: 275, s: 5, l: 50 },
      optimist_border_actual: { h: 50, s: 100, l: 50 },
      pessimist_border_actual: { h: 275, s: 100, l: 50 },
      berry: { h: 0, s: 100, l: 38 },
      berry_picked: { h: 0, s: 0.5, l: 60 },
      berry_border: { h: 0, s: 0, l: 0 }
    },
    size: {
      max_leaf_radius: 0.3, // maximum leaf radius relative to tile size
      min_leaf_radius: 0.70, // minimum leaf radius as a proportion of max_leaf_radius
      berry_radius: 0.24, // berry radius, as a proportion of the max foliage radius
      berry_border: 0.3, // size of the border, in pixels (i think)
    },
    animations: {
      shake: {
        duration: 400,
        amplitude: 5,
        speed: 4,
        duration_error: 300,
        amplitude_error: 3,
        speed_error: 6,
      },
      hover: {
        duration: 200,
        default_scale: 1.0,
        magnify_scale: 1.13,
        minimize_scale: 0.8
      }
    },
    min_leaves: 12,
    max_leaves: 16,
  },
  tile: {
    colors: {
      default: { h: 30, s: 55.2, l: 49 },
      Ncorner: { h: 240, s: 60, l: 42},
      Scorner: { h: 219, s: 53, l: 64 },
      optimist_path: { h: 50, s: 30, l: 70 },
      pessimist_path: { h: 275, s: 30, l: 70 },
      optimist_path_predict: { h: 50, s: 5, l: 70 },
      pessimist_path_predict: { h: 275, s: 5, l: 70 },
      optimist_path_actual: { h: 50, s: 50, l: 70 },
      pessimist_path_actual: { h: 275, s: 50, l: 70 },
    },
    size: {
      base: 0.94,
      path: 0.85,
      real: 0.68,
    },
    animations: {
      transition: {
        frames: 20, // # of frames to transition tile
        delay: 30 // # of milisseconds to offset each tiles' transition start
      },
      pulse: {
        period: 1000, // speed of one pulse
        separation: 6, // separation between pulses, in half-sine waves
        saturation_range: 30, // maximum saturation increase
        tile_range: 0.05 // maximum tile size increase
      },
    },
    corner_radius: 10,
  },
  basket: {
    canvas_width: 300,
    canvas_height: 200,
    position: {
      x: 150,
      y: 195,
      wall_offset_x: 95,
      wall_offset_y: 20,
    },
    size: {
      thickness: 10,
      bottom_width: 180,
      wall_height: 145,
    },
    berry: {
      x: 150,
      y: -10,
      x_spread: 70,
      y_spread: 10,
      size_min: 10,
      size_max: 13,
      restitution: 0.35,
      density: 0.002
    }
  },
  sliders: {
    environment_counterfactual: {
      prompts: [(wasSuccessful, agentTrait) => `{AGENT} would have <em><b>${wasSuccessful ? "failed" : "succeeded"}</em></b> if they had started on the {!START} entrance.`],
      slider_min: 'not at all',
      slider_max: (agentTrait) => 'strongly',
    },
    trait_counterfactual: {
      prompts: [(wasSuccessful, agentTrait) => `{AGENT} would have <em><b>${wasSuccessful ? "failed" : "succeeded"}</em></b> if they were as <em><b>${agentTrait === "optimist" ? "pessimistic" : "optimistic"}</em></b> as {!AGENT}.`],
      slider_min: 'not at all',
      slider_max: (agentTrait) => 'strongly',
    },
    environment_causal: {
      prompts: [(wasSuccessful, agentTrait) => `{AGENT}'s <em><b>${wasSuccessful ? "success" : "failure"}</em></b> was caused by starting on the {START} entrance.`],
      slider_min: 'not at all',
      slider_max: (agentTrait) => 'strongly',
    },
    trait_causal: {
      prompts: [(wasSuccessful, agentTrait) => `{AGENT}'s <em><b>${wasSuccessful ? "success" : "failure"}</em></b> was caused by their <em><b>${agentTrait === "optimist" ? "optimism" : "pessimism"}</em></b>.`],
      slider_min: 'not at all',
      slider_max: (agentTrait) => 'strongly',
    },
    causal_selection: {
        prompts: [
          (wasSuccessful, agentTrait) => `Why did {AGENT} <em><b>${wasSuccessful ? "succeed" : "fail"}</em></b>?`],
        slider_min: '{START}<br>location',
        slider_max: (agentTrait) => agentTrait === "optimist" ? "optimism" : "pessimism",
      },
  },
  background_color: '#FAEFD1'
};