module CandidateFixtures
  SALES = <<~CSV.freeze
    date,category,amount_minor
    2026-10-31,books,1200
    2026-11-01,books,1200
    2026-11-01,games,3500
    2026-11-01,books,800
    2026-11-01,games,-500
    2026-11-02,books,9999
  CSV
  HELDOUT = <<~CSV.freeze
    date,category,amount_minor
    2026-12-05,tools,250
    2026-12-05,tools,-50
    2026-12-05,art,0
    2026-12-06,art,900
  CSV
  GOAL = 'Sum signed integer sales by category over a half-open date interval. Validate all CSV rows, retain zero-total categories and reject malformed data.'
  CRITERIA = ['Return exact category and total minor-unit sums, row count and requested period.', 'Reject missing columns, invalid integers or dates, empty categories and reversed periods.'].freeze

  def observed_input
    {csv: SALES, start: '2026-11-01', end: '2026-11-02'}
  end

  def candidate_source
    File.read(File.join(__dir__, 'candidate_sales.py'))
  end

  def independent_fixtures
    [
      {input: {csv: SALES, start: '2026-11-01', end: '2026-11-03'}, expected: {status: 'ok', value: {start: '2026-11-01', end: '2026-11-03', row_count: 5, by_category_minor: {books: 11999, games: 3000}, total_minor: 14999}}},
      {input: {csv: SALES, start: '2026-11-03', end: '2026-11-04'}, expected: {status: 'ok', value: {start: '2026-11-03', end: '2026-11-04', row_count: 0, by_category_minor: {}, total_minor: 0}}},
      {input: {csv: HELDOUT, start: '2026-12-05', end: '2026-12-06'}, expected: {status: 'ok', value: {start: '2026-12-05', end: '2026-12-06', row_count: 3, by_category_minor: {tools: 200, art: 0}, total_minor: 200}}},
      {input: {csv: "date,category\n2026-11-01,books\n", start: '2026-11-01', end: '2026-11-02'}, expected: {status: 'rejected', error: 'missing_required_column:amount_minor'}},
      {input: {csv: "date,category,amount_minor\n2026-11-01,books,1.5\n", start: '2026-11-01', end: '2026-11-02'}, expected: {status: 'rejected', error: 'invalid_integer:amount_minor'}},
      {input: {csv: SALES, start: '2026-11-03', end: '2026-11-01'}, expected: {status: 'rejected', error: 'invalid_period'}}
    ]
  end
end
