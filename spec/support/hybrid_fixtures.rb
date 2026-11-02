module HybridFixtures
  GOAL = 'Calculate exact current and previous sales totals and category deltas from supplied records, then explain the changes without inventing causes or unsupported facts.'.freeze
  NUMERIC_CRITERION = 'Return exact integer minor-unit totals and category deltas over the union of categories; percentage change is undefined when the previous amount is zero.'.freeze
  SEMANTIC_CRITERIA = {
    'S1' => 'Name both periods and report current and previous totals consistently with the calculated facts.',
    'S2' => 'Explain every category delta and the net total without inventing causes.',
    'S3' => 'Support every numerical claim and referenced record ID with supplied records or calculated facts.',
    'S4' => 'Describe percentage comparisons as undefined when the previous amount is zero; never invent 100% growth.',
    'S5' => 'Describe unavailable data and unconfirmed delivery honestly without claiming failed work succeeded.'
  }.freeze
  CRITERIA = [NUMERIC_CRITERION, *SEMANTIC_CRITERIA.values].freeze
  OBSERVED = {
    current: {period: '2026-11-01', rows: [{id: 'c1', category: 'books', amount_minor: 1200}, {id: 'c2', category: 'books', amount_minor: 800}, {id: 'c3', category: 'games', amount_minor: 3000}]},
    previous: {period: '2026-10-31', rows: [{id: 'p1', category: 'books', amount_minor: 1000}, {id: 'p2', category: 'games', amount_minor: 4000}]}
  }.freeze
  HELDOUT = {
    current: {period: '2026-12-05', rows: [{id: 'h1', category: 'tools', amount_minor: 250}, {id: 'h2', category: 'tools', amount_minor: -50}, {id: 'h3', category: 'art', amount_minor: 0}]},
    previous: {period: '2026-12-04', rows: []}
  }.freeze
  OBSERVED_FACTS = {current_total_minor: 5000, previous_total_minor: 5000, delta_minor: 0, percent_change: 0,
    categories: {books: {current_minor: 2000, previous_minor: 1000, delta_minor: 1000, percent_change: 100}, games: {current_minor: 3000, previous_minor: 4000, delta_minor: -1000, percent_change: -25}}}.freeze
  HELDOUT_FACTS = {current_total_minor: 200, previous_total_minor: 0, delta_minor: 200, percent_change: nil,
    categories: {tools: {current_minor: 200, previous_minor: 0, delta_minor: 200, percent_change: nil}, art: {current_minor: 0, previous_minor: 0, delta_minor: 0, percent_change: nil}}}.freeze
  OBSERVED_TEXT = 'For 2026-11-01 versus 2026-10-31, sales remained 5000 minor units in both periods: net change 0 (0%). Books rose from 1000 to 2000, a change of +1000 (+100%); games fell from 4000 to 3000, a change of -1000 (-25%). These changes offset each other. The supplied records do not establish causes or any external delivery.'.freeze
  HELDOUT_TEXT = 'For 2026-12-05 versus 2026-12-04, sales totaled 200 minor units versus 0, a net change of +200. Tools totaled 200 versus 0 after the 250 sale and -50 adjustment; art remained 0 versus 0. All percentage comparisons are undefined because the previous amounts were zero. No cause or external delivery is established by these records.'.freeze

  def hybrid_source
    File.read(File.join(__dir__, 'hybrid_calculation.py'))
  end

  def json_value(value)
    JSON.parse(JSON.generate(value))
  end
end
