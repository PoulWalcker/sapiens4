# Independent examples: expected business text is frozen separately from generated code.
module FeedCodeFixtures
  GOAL = 'Summarize only newly observed feed records with their integer values, using the supplied prefix.'.freeze
  CRITERIA = ['Name each new record ID and its exact integer value; omit previously accepted records.', 'Use the supplied prefix and report no new records for a complete empty result.'].freeze
  PARAMETERS = {prefix: 'Feed'}.freeze
  OBSERVED_TEXT = 'Feed: r1 = 10; r2 = 20.'.freeze
  OVERLAP_TEXT = 'Feed: r3 = 30.'.freeze
  NEXT_TEXT = 'Feed: r4 = -5.'.freeze

  def feed_page(cursor, next_cursor, records)
    {schema: 1, source: 'sales-feed', cursor: cursor, nextCursor: next_cursor, complete: true, records: records}
  end

  def independent_feed_cases
    [
      {input: {taskInputs: PARAMETERS, page: feed_page('c0', 'c2', [{id: 'r1', value: 10}, {id: 'r2', value: 20}]), priorSeenIds: []}, expected: {status: 'ok', value: {text: OBSERVED_TEXT}}},
      {input: {taskInputs: PARAMETERS, page: feed_page('c2', 'page:next-z9', [{id: 'r2', value: 20}, {id: 'r3', value: 30}]), priorSeenIds: %w[r1 r2 historical-off-page]}, expected: {status: 'ok', value: {text: OVERLAP_TEXT}}},
      {input: {taskInputs: {prefix: 'Quiet'}, page: feed_page('opaque:empty', 'opaque:empty', []), priorSeenIds: ['historical-off-page']}, expected: {status: 'ok', value: {text: 'Quiet: no new records.'}}}
    ]
  end

  def feed_code_source
    File.read(File.join(__dir__, 'feed_code_body.py'))
  end
end
