
function setupGame() {
  const yellow_hsl = convertToHSL(gs.agent.colors['optimist']);
  const purple_hsl = convertToHSL(gs.agent.colors['pessimist']);
  const yellow_name = gs.agent.names['optimist'];
  const purple_name = gs.agent.names['pessimist'];
  const yellow_text = `<span style="color: ${yellow_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${yellow_name}</span>`
  const purple_text = `<span style="color: ${purple_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">${purple_name}</span>`
  const north_hsl = convertToHSL(gs.tile.colors['Ncorner']);
  const south_hsl = convertToHSL(gs.tile.colors['Scorner']);
  const north_text = `<span style="color: ${north_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">NORTH</span>`
  const south_text = `<span style="color: ${south_hsl}; text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;">SOUTH</span>`

  // Initialize jsPsych
  const jsPsych = initJsPsych({
    show_progress_bar: true,
    auto_update_progress_bar: false
  });

  /* #region setup data collection */

  // get experiment information from URL
  var queryString = window.location.search;
  var urlParams = new URLSearchParams(queryString);
  gs.prolific_info.prolificID = urlParams.get('PROLIFIC_PID');
  gs.prolific_info.prolificStudyID = urlParams.get('STUDY_ID');
  gs.prolific_info.prolificSessionID = urlParams.get('SESSION_ID');
  gs.session_info.gameID = UUID();
  gs.session_info.condition = trial_stims.condition;

  gs.session_info.on_finish = function (data) {
    var updatedData = _.omit(data, 'on_finish', 'on_start', 'on_timeline_finish', 'on_timeline_start', 'gridworld')
    console.log('emitting trial data', updatedData)

    const total_trials = jsPsych.getProgress().total_trials +
                          foragingProcedure.timeline.length * foragingProcedure.timeline_variables.length +
                          practiceProcedure.timeline.length * practiceProcedure.timeline_variables.length - 2
    jsPsych.setProgressBar(updatedData.trial_index / total_trials);
  };

  /* #endregion */

  /* #region study instructions */

  var preload = {
    type: jsPsychPreload,
    images: [
      'assets/img/agent_intro.png',
      'assets/img/grid_environment.png',
      'assets/img/trees.png',
      'assets/img/trees-with-mystery.png',
      'assets/img/mystery_trees.png',
      'assets/img/agent_traits-baseline.png',
      'assets/img/agent_traits_yellow.gif',
      'assets/img/agent_traits_purple.gif',
      'assets/img/start_locations.png',
      'assets/img/start_locations_with_agent.png',
      'assets/img/path-only.gif',
      'assets/img/result-only.gif',
      'assets/img/predict-static.png',
      'assets/img/predict.gif',
      'assets/img/undo-predict-static.png',
      'assets/img/undo-predict.gif',
      'assets/img/result.gif',
      'assets/comprehension/comprehension1_stim.png',
      'assets/comprehension/comprehension1_correct.png',
      'assets/comprehension/comprehension1_alt1.png',
      'assets/comprehension/comprehension1_alt3.png',
      'assets/comprehension/comprehension2_stim.png',
      'assets/comprehension/comprehension2_correct.png',
      'assets/comprehension/comprehension2_alt1.png',
      'assets/comprehension/comprehension2_alt2.png'
    ]
  }

  // Define landing page language
  const time = '20-30';
  var introductionInstructions = {
    type: jsPsychInstructions,
    pages: [
      `<p> Hello! Thank you for participating in this research.</p> \
      <p> We expect the study to last about ${time} minutes, including the time it takes to read these instructions.</p> \
      <i><p> Note: We recommend completing the study in Chrome. It has not been tested in other browsers.</p></i>`,
    ],
    show_clickable_nav: true,
    allow_keys: gs.study_metadata.dev_mode,
    allow_backward: true
  }

  var enterFullscreen = {
    type: jsPsychFullscreen,
    fullscreen_mode: true
  };

  // Define task instructions language
  var taskInstructionsHTML = [
    '<p>In this study, your task is to evaluate two different farmers, ' +
        yellow_text + ' and ' + purple_text + ', as they harvest berries from trees on their farm.</p>\
        <img height="300" src="assets/img/agent_intro.png">',
    '<p>The farm has a number of different <em>plots</em> that need harvesting. \
        </br>Each plot is <em>10</em> squares wide and <em>10</em> squares tall, just like this:</p>\
        <img height="550" src="assets/img/grid_environment.png">\
        <p>There are <em>berry trees</em> spread throughout the plot.\
        </br>Some squares have a tree ready for harvest, and some have no trees.</p>',
    '<img height="200" src="assets/img/trees.png">\
        <p>Every tree has a certain number of <em>berries</em> that can be harvested.\
        </br>The trees with the <em>fewest</em> berries have just <em>1</em>, the trees with the <em>most</em> have <em>9</em>.</p>',
    '<img height="200" src="assets/img/trees-with-mystery.png">\
        <p>However, some <em>mystery trees</em> have an <em>unknown</em> number of berries.</p>',
    '<p>When the farmers harvest from <em>mystery trees</em>, they don\'t know ahead of time how many berries they\'ll get.\
        </br>In fact, these trees are just like the others. Sometimes they have very few berries, and sometimes they have a lot.</p>\
        <img height="550" src="assets/img/mystery_trees.png">',
    '<p>When it comes to mystery trees, there\'s an important difference between ' + yellow_text + ' and ' + purple_text + '.</p>\
        <img height="300" src="assets/img/agent_traits-baseline.png">',
    '<p>' + yellow_text + ' is always <em>optimistic</em> about the mystery trees.\
        </br>' + yellow_text + ' expects them to be <em>full of berries</em> and doesn\'t want to miss out.</p>\
        <img height="300" src="assets/img/agent_traits_yellow.gif">',
    '<p>Meanwhile, ' + purple_text + ' is always <em>pessimistic</em> about the mystery trees.\
        </br>' + purple_text + ' expects them to have <em>very few berries</em> and doesn\'t want to waste time.</p>\
        <img height="300" src="assets/img/agent_traits_purple.gif">\
        <p><em>We\'ll show this graphic throughout the experiment, so no need to memorize which one is which</em>.</p>',
    '<p>When the farmers begin harvesting, they always start from one of two possible <em>entrances</em>\:\
        </br>' + north_text + ' entrance at the top left, or ' + south_text + ' entrance at the bottom right.</p>\
        <img height="550" src="assets/img/start_locations.png">',
    '<p>These are the same in every plot, but <em>which one</em> the farmer starts at is decided randomly.\
        </br>Sometimes they get lucky and have lots of trees near their entrance. Other times, not so much.</p>\
        <img height="550" src="assets/img/start_locations_with_agent.png">',
    '<p>What makes the farmers\' job tricky is that they can only go so far in a particular plot!\
        </br>In each plot, they can traverse <em>no more than 10 squares</em> before returning with their berries.\
        </br>So, they have to decide carefully what path they\'ll take to harvest as many berries as possible.</p>\
        <img height="550" src="assets/img/path-only.gif">',
    '<p>To make a profit, they need to harvest <em>at least 20 berries</em> from each plot.\
        </br>The more berries they get, the better their payout, even when they don\'t reach 20.</p>\
        <img height="550" src="assets/img/result-only.gif">',
    '<p>Your first task is to <em>predict the farmer\'s path</em> in a few different plots.\
        </br>To make a prediction, click on the trees in the order you think the farmer will harvest them.\
        </br>When you click a tree, squares in the plot will light up to show the farmer\'s path.\
        </br>As you make your prediction, a counter at the right will show how many steps they have left.\
        </br>Below is a static image of this part. <em>Click Next to see it in action</em>.</p>\
        <img height="550" src="assets/img/predict-static.png">',
    '<p><em>Here\'s a video!</p>\
        <img height="550" src="assets/img/predict.gif">',
    '<p>At any time, you can <em>change</em> your prediction by clicking the <em>previous tree</em> you guessed.\
        </br>This will move the farmer\'s predicted path back to that point.\
        </br>You can also click the <em>farmer</em> to reset your prediction completely.\
        </br>Below is a static image of this part. <em>Click Next to see it in action</em>.</p>\
        <img height="550" src="assets/img/undo-predict-static.png">',
    '<p><em>Here\'s a video!</p>\
        <img height="550" src="assets/img/undo-predict.gif">',
    '<p>Once you\'ve made your <em>prediction</em>, you can view the farmer\'s <em>true path</em>\
        to see how well they did and compare it to your prediction.</p>\
      <img height="550" src="assets/img/result.gif">',
    '<p>Let\'s get started!</p> \
      <p>We\'re going to do <em>six</em> prediction rounds to learn how each farmer harvests their plots.</p>'
  ];

  var taskInstructions = {
    type: jsPsychInstructions,
    pages: taskInstructionsHTML,
    show_clickable_nav: true,
    allow_keys: gs.study_metadata.dev_mode,
    allow_backward: true,
    on_start: function() {
      gs.session_data.startInstructionTS = Date.now();
    }
  }

  // Add practice/familiarization set
  var practiceSet = {
    // NB: prac1 and prac_rest are really similar, can probably just combine
    prac1_prompt: (color) => `<p>On the next screen, you\'ll see a plot that ${color} is harvesting.</p>`,
    prac_rest_prompt: (color) => `This time, you\'ll see a plot that ${color} is harvesting.`,
    prac_feedback: (correct, total, color, last) => `You got ${correct} out of the ${total} trees that ${color} harvested.\n\
                                        ${last ? '' : 'Let\'s do another one.'}`,
  };

  // NOTE: observed_trajectory is only the trees in the path, not the full path
  var pracData = _.shuffle([
    {
      "name": "trial_806",
      "rows": 10,
      "cols": 10,
      "agent_type": "optimist",
      "agent_start_position": [1, 1],
      "tree_positions": [[1, 2], [1, 7], [10, 4], [2, 10], [4, 8], [8, 2], [8, 5], [10, 9], [3, 6], [4, 4]],
      "tree_rewards": [5, 8, 1, 5, 8, 3, 4, 6, 5, 1],
      "tree_visibility": [1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
      "observed_trajectory": [[1, 2], [1, 7], [4, 8]],
      "total_steps": 10,
      "practice": true
    },{
      "name": "trial_980",
      "rows": 10,
      "cols": 10,
      "agent_type": "optimist",
      "agent_start_position": [1, 1],
      "tree_positions": [[6, 4], [8, 5], [9, 8], [2, 6], [4, 8], [2, 1], [1, 5], [10, 4], [7, 3], [5, 8]],
      "tree_rewards": [5, 6, 4, 1, 3, 3, 3, 5, 9, 6],
      "tree_visibility": [1, 0, 1, 0, 0, 1, 0, 1, 1, 1],
      "observed_trajectory": [[1, 5], [2, 6], [4, 8]],
      "total_steps": 10,
      "practice": true
    },{
      "name": "trial_830",
      "rows": 10,
      "cols": 10,
      "agent_type": "optimist",
      "agent_start_position": [10, 10],
      "tree_positions": [[5, 9], [8, 2], [6, 3], [1, 3], [10, 4], [7, 8], [10, 9], [3, 10], [10, 1], [9, 1]],
      "tree_rewards": [7, 6, 9, 4, 4, 8, 3, 5, 2, 9],
      "tree_visibility": [1, 0, 1, 1, 1, 1, 1, 1, 0, 0],
      "observed_trajectory": [[10, 9], [10, 4], [10, 1], [9, 1]],
      "total_steps": 10,
      "practice": true
    },{
      "name": "trial_761",
      "rows": 10,
      "cols": 10,
      "agent_type": "pessimist",
      "agent_start_position": [1, 1],
      "tree_positions": [[6, 6], [2, 8], [6, 4], [4, 2], [8, 8], [8, 9], [1, 4], [4, 4], [10, 8], [7, 10]],
      "tree_rewards": [9, 7, 9, 5, 4, 4, 2, 4, 3, 8],
      "tree_visibility": [1, 1, 1, 1, 1, 1, 0, 1, 1, 0],
      "observed_trajectory": [[4, 2], [4, 4], [6, 4], [6, 6]],
      "total_steps": 10,
      "practice": true
    },{
      "name": "trial_458",
      "rows": 10,
      "cols": 10,
      "agent_type": "pessimist",
      "agent_start_position": [10, 10],
      "tree_positions": [[1, 2], [9, 4], [3, 10], [10, 3], [8, 7], [3, 2], [10, 2], [5, 5], [3, 1], [4, 6]],
      "tree_rewards": [8, 5, 3, 2, 8, 4, 6, 6, 9, 7],
      "tree_visibility": [1, 1, 0, 0, 1, 1, 0, 1, 1, 1],
      "observed_trajectory": [[8, 7], [4, 6]],
      "total_steps": 10,
      "practice": true
    },{
      "name": "trial_536",
      "rows": 10,
      "cols": 10,
      "agent_type": "pessimist",
      "agent_start_position": [10, 10],
      "tree_positions": [[8, 1], [3, 1], [6, 1], [9, 10], [4, 1], [5, 1], [5, 5], [3, 3], [9, 4], [2, 8]],
      "tree_rewards": [4, 7, 1, 9, 1, 3, 8, 8, 8, 1],
      "tree_visibility": [1, 0, 1, 1, 0, 1, 1, 1, 0, 1],
      "observed_trajectory": [[9, 10], [5, 5]],
      "total_steps": 10,
      "practice": true
    }
  ]);

  var practiceProcedure = {
    timeline: [
      {
        type: jsPsychHtmlButtonResponse,
        stimulus: function() {
          let first_loop = jsPsych.timelineVariable('index') === 0;
          let color = jsPsych.timelineVariable('data').agent_type === 'optimist' ? yellow_text : purple_text;
          return first_loop ? practiceSet.prac1_prompt(color) : practiceSet.prac_rest_prompt(color)},
        choices: ['Continue'],
        margin_vertical: "20px"
      },
      _.extend({}, gs.study_metadata, gs.session_info, {
        type: jsPsychForagePredict,
        data: jsPsych.timelineVariable('data'),
        cue_duration: gs.game_info.cue_duration,
        progress_prompt: function() {
          return `Practice trial ${jsPsych.timelineVariable('index') + 1} of ${pracData.length}`
        }
      }),
      _.extend({}, gs.study_metadata, gs.session_info, {
        type: jsPsychForageObserve,
        data: jsPsych.timelineVariable('data'),
        tree_trajectory: function() { return getFromLastTrial('forage-predict', 'tree_trajectory') },
        coordinate_trajectory: function() { return getFromLastTrial('forage-predict', 'coordinate_trajectory') },
        trees_data: function() { return getFromLastTrial('forage-predict', 'trees_data') },
        sequence_uuid: function() { return getFromLastTrial('forage-predict', 'sequence_uuid') },
        berries_needed: gs.game_info.berries_needed,
        cue_duration: gs.game_info.cue_duration,
        start_delay: gs.game_info.start_delay,
        post_trial_gap: 500,
        progress_prompt: function() {
          return `Practice trial ${jsPsych.timelineVariable('index') + 1} of ${pracData.length}`
        }
      }),
      _.extend({}, gs.study_metadata, gs.session_info, {
        type: jsPsychHtmlButtonResponse,
        stimulus: function() {
          let pred_trees = getFromLastTrial('forage-predict', 'tree_trajectory').map(e => [e[0]+1, e[1]+1]);
          let real_trees = jsPsych.timelineVariable('data').observed_trajectory;
          let last = jsPsych.timelineVariable('index') === (pracData.length - 1);

          let color = jsPsych.timelineVariable('data').agent_type === 'optimist' ? yellow_text : purple_text;
          return practiceSet.prac_feedback(findCommonElements(pred_trees, real_trees).length, real_trees.length, color, last) },
        choices: ['Continue'],
        margin_vertical: "20px"
      }),
    ],
    on_timeline_start: function() {
        gs.session_data.startPracticeTS = Date.now();
    },
    timeline_variables: pracData.map((stim, idx) => ({ data: stim, index: idx })),
    // timeline_variables: trialData // TESTING ONLY
  }

  var practiceConclusion = {
    type: jsPsychHtmlButtonResponse,
    stimulus: '<p>Nice work!\
                </br>Now you\'ve had a chance to see how ' + yellow_text + ' and ' + purple_text + ' harvest their farm.\
                </br>On the next screen, you\'ll be asked <em>two</em> questions confirming that everything is crystal clear.</p>\
                <p>Please do your best.\
                </br>You will not be able to proceed to the next part of the experiment until you answer both questions correctly.',
    choices: ['Continue'],
    margin_vertical: "20px"
  }

  var comprehensionCheck1 = _.extend({}, gs.study_metadata, gs.session_info, {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p id = promptid>Which path would ${yellow_text} take in the plot below?</p>\
                <img src="assets/comprehension/comprehension1_stim.png" height="400">
                <p style="margin-bottom:0px;"><em>Click the most likely path.</em></p>`,
    choices: _.shuffle([
      '<img src="assets/comprehension/comprehension1_correct.png" height="300"><p style="margin:0px;">HARVESTED: 17</p>',
      '<img src="assets/comprehension/comprehension1_alt1.png" height="300"><p style="margin:0px;">HARVESTED: 15</p>',
      '<img src="assets/comprehension/comprehension1_alt3.png" height="300"><p style="margin:0px;">HARVESTED: 15</p>',
    ]),
    margin_vertical: '10px',
    margin_horizontal: '10px'
  });

  var comprehensionCheck2 = _.extend({}, gs.study_metadata, gs.session_info, {
    type: jsPsychHtmlButtonResponse,
    stimulus: `<p id = promptid>Which path would ${purple_text} take in the plot below?</p>\
                <img src="assets/comprehension/comprehension2_stim.png" height="400">
                <p style="margin-bottom:0px;"><em>Click the most likely path.</em></p>`,
    choices: _.shuffle([
      '<img src="assets/comprehension/comprehension2_correct.png" height="300"><p style="margin:0px;">HARVESTED: 28</p>',
      '<img src="assets/comprehension/comprehension2_alt1.png" height="300"><p style="margin:0px;">HARVESTED: 26</p>',
      '<img src="assets/comprehension/comprehension2_alt2.png" height="300"><p style="margin:0px;">HARVESTED: 28</p>'
    ]),
    margin_vertical: '10px',
    margin_horizontal: '10px'
  });

  var loop_prompt = _.extend({}, gs.study_metadata, gs.session_info, {
    type: jsPsychHtmlButtonResponse,
    stimulus: function() {
      var data = jsPsych.data.get()
      var resp1 = data.values()[data.values().length - 2].responsehtml.includes('correct');
      var resp2 = data.values()[data.values().length - 1].responsehtml.includes('correct');
      var resp = !resp1 && !resp2 ? 'You got both wrong.' : !resp1 ? 'You got the first one wrong.' : 'You got the second one wrong.'

      return `<p>Oh no! ${resp} Let\'s give it another shot.</p>\
      <p>For each question, pay close attention to the available paths and choose the one the farmer is <em>most likely</em> to take.</p>`
    },
    choices: ['Retry'],
    margin_vertical: "20px"
  });

  var ifLoop = {
    timeline: [loop_prompt],
    conditional_function: function() {
      var data = jsPsych.data.get()
      resp1 = data.values()[data.values().length - 2].responsehtml.includes('correct');
      resp2 = data.values()[data.values().length - 1].responsehtml.includes('correct');
      if (resp1 && resp2) {
        return false
      } else {
        gs.session_data.comprehensionAttempts += 1;
        return true
      }
    }
  }

  // redo familiarization if comprehension check failed
  var loopNode = {
    timeline: [comprehensionCheck1, comprehensionCheck2, ifLoop],
    // timeline: [practiceProcedure, comprehensionCheck1, comprehensionCheck2, ifLoop],
    loop_function: function(data) {
      resp1 = data.values()[data.values().length - 2].responsehtml.includes('correct');
      resp2 = data.values()[data.values().length - 1].responsehtml.includes('correct');

      if (resp1 && resp2) {
        return false
      } else {
        return true
      }
    }
  }

  var comprehensionConclusion = {
    type: jsPsychHtmlButtonResponse,
    stimulus: '<p>Great job! \
                </br>You\'ve learned everything you need to know about how ' + yellow_text + ' and ' + purple_text + ' harvest their farm.</p>\
                <p>Now, your job is to <em>evaluate</em> them as they harvest a <em>new</em> set of plots.\
                </br>In the next part, you\'ll see each farmer harvest <em>ten</em> new plots.\
                </br>As in the previous section, they\'re trying to get as many berries as possible.\
                </br>They need <em>20</em> berries to make a profit in a given plot.\
                </br>After each harvest, you\'ll be asked a question about how they performed against this goal.</p>',
    choices: ['Continue'],
    margin_vertical: "20px"
  }

  /* #endregion */

  /* #region study body */

  var getFromLastTrial = function (trialType, selector) {
    return jsPsych.data.get().filter({ trial_type: trialType }).last().select(selector).values[0]
  };

  var foragingProcedure = {
    timeline: [
      _.extend({}, gs.study_metadata, gs.session_info, {
        type: jsPsychForageObserve,
        data: jsPsych.timelineVariable('data'),
        sequence_uuid: function() { return UUID() },
        berries_needed: gs.game_info.berries_needed,
        cue_duration: gs.game_info.cue_duration,
        start_delay: gs.game_info.start_delay,
        end_delay: gs.game_info.end_delay,
        progress_prompt: function() {
          return `Trial ${jsPsych.timelineVariable('index') + 1} of ${trial_stims.stims.length}`
        }
      }),
      _.extend({}, gs.study_metadata, gs.session_info, {
        type: jsPsychForageEvaluate,
        data: jsPsych.timelineVariable('data'),
        sequence_uuid: function() { return getFromLastTrial('forage-observe', 'sequence_uuid') },
        gridworld: function() { return getFromLastTrial('forage-observe', 'gridworld') },
        post_trial_gap: 500,
        questions: function() {
          let agent_succeeded = getFromLastTrial('forage-observe', 'berries_collected') >= getFromLastTrial('forage-observe', 'berries_needed')
          return gs.sliders[gs.session_info.condition].prompts.map((prompt) => prompt(agent_succeeded, jsPsych.timelineVariable('data').agent_type))
        },
        slider_min: gs.sliders[gs.session_info.condition].slider_min,
        slider_max: function() { 
          return gs.sliders[trial_stims.condition].slider_max(jsPsych.timelineVariable('data').agent_type)
        },
        // slider_max: gs.sliders[gs.session_info.condition].slider_max,
        cue_duration: gs.game_info.cue_duration * 2,
        berries_collected: function() { return getFromLastTrial('forage-observe', 'berries_collected') },
      })
    ],
    on_timeline_start: function() {
      gs.session_data.startExperimentTS = Date.now();
    },
    timeline_variables: trial_stims.stims.map((stim, idx) => ({ data: stim, index: idx })),
  }

  /* #endregion */


  /* #region exit trials */

  // End of experiment, move to exit survey
  var preSurveyMessage = {
    type: jsPsychHtmlButtonResponse,
    stimulus: '<p>You\'ve completed the experiment!\
                </br>On the next page, you\'ll be shown a brief set of questions about how the experiment went.\
                </br>Once you submit your answers, you\'ll be redirected back to Prolific and credited for participation.</p>',
    choices: ['Continue'],
    margin_vertical: "20px"
  }

  // define survey trial
  var exitSurvey = _.extend({},
    gs.study_metadata,
    _.omit(gs.session_info, 'on_finish'),
    gs.prolific_info, {
      type: jsPsychSurvey,
      pages: [[
          {
              type: 'html',
              prompt: 'Please answer the following questions:',
          },
          {
              type: 'multi-choice',
              name: 'participantGender',
              prompt: "What is your gender?",
              options: ['Male', 'Female', 'Non-binary', 'Other'],
              columns: 0,
              required: true
          },
          {
              type: 'text',
              name: 'participantYears',
              prompt: 'How many years old are you?',
              placeholder: '18',
              textbox_columns: 5,
              required: true
          },
          {
            type: 'multi-choice',
            name: 'participantRace',
            prompt: 'What is your race?',
            options: ['White', 'Black/African American', 'American Indian/Alaska Native', 'Asian', 'Native Hawaiian/Pacific Islander', 'Multiracial/Mixed', 'Other'],
            columns: 0,
            required: true
          },
          {
            type: 'multi-choice',
            name: 'participantEthnicity',
            prompt: 'What is your ethnicity?',
            options: ['Hispanic', 'Non-Hispanic'],
            columns: 0,
            required: true
          },
          {
              type: 'multi-choice',
              name: 'inputDevice',
              prompt: 'Which of the following devices did you use to complete this study?',
              options: ['Mouse', 'Trackpad', 'Touch Screen', 'Stylus', 'Other'],
              columns: 0,
              required: true
          },
          {
              type: 'likert',
              name: 'judgedDifficulty',
              prompt: 'How difficult did you find this study?',
              likert_scale_min_label: 'Very Easy',
              likert_scale_max_label: 'Very Hard',
              likert_scale_values: [{ value: 1 }, { value: 2 }, { value: 3 }, { value: 4 }, { value: 5 }],
              required: true,
          },
          {
              type: 'likert',
              name: 'participantEffort',
              prompt: 'How much effort did you put into the game? Your response will not effect your final compensation.',
              likert_scale_min_label: 'Low Effort',
              likert_scale_max_label: 'High Effort',
              likert_scale_values: [{ value: 1 }, { value: 2 }, { value: 3 }, { value: 4 }, { value: 5 }],
              required: true,
          },
          {
              type: 'text',
              name: 'participantComments',
              prompt: "What factors influenced how you decided to respond? Do you have any other comments or feedback to share with us about your experience?",
              placeholder: "I had a lot of fun!",
              textbox_rows: 4,
              required: false
          },
          {
              type: 'text',
              name: "TechnicalDifficultiesFreeResp",
              prompt: "If you encountered any technical difficulties, please briefly describe the issue.",
              placeholder: "I did not encounter any technical difficulities.",
              textbox_rows: 4,
              required: false
          }
      ]],
      on_start: function() {
        gs.session_data.endExperimentTS = Date.now(); // collect end experiment time
      },
      on_finish: function(data) {
        var updatedData = _.extend({}, gs.session_data, _.omit(data, 'on_start')); // because gs.session_data is updated throught the experiment
        console.log('emitting trial data', updatedData)

        jsPsych.setProgressBar(1);
      }
  });

  // define goodbye trial
  var goodbye = {
      type: jsPsychInstructions,
      pages: function () {
          return [
              `<p>Thanks for participating in our experiment!</p>\
              <p>Please click the <em>Submit</em> button to complete the study.</p>
              <p>Once you click <em>Submit</em>, you will be redirected to Prolific and receive credit for your participation.</p>`
          ]
      },
      show_clickable_nav: true,
      allow_backward: false,
      button_label_next: '< Submit',
      on_finish: () => {
          window.onbeforeunload = null;

          // change URL to our study
          var completion_url = "https://github.com/cicl-stanford/action_abstraction_cogsci2024"

          window.open(completion_url, "_self")
      }
  };

  /* #endregion */

  trials = [];
  trials.push(preload);
  trials.push(introductionInstructions);
  trials.push(enterFullscreen);
  trials.push(taskInstructions);
  trials.push(practiceProcedure);
  trials.push(practiceConclusion);
  trials.push(loopNode);
  trials.push(comprehensionConclusion);
  trials.push(foragingProcedure);
  trials.push(preSurveyMessage);
  trials.push(exitSurvey);
  trials.push(goodbye);

  // Run the experiment
  jsPsych.run(trials);
}