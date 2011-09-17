#!/usr/local/bin/ruby
# source2html Eiffel Acceptance Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestEiffel < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def teardown
  end

  def test_basics
    text = read_file("acceptance/eiffelcode_e.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/eiffelcode_e.html")
    assert_match /language: Eiffel/, text
    assert_match /<em>\s*-- This is a comment<\/em>/, text    
    assert_match /<b>feature<\/b>/, text    
  end

  def test_reference
    text = read_file("acceptance/eiffelcode_e.html")
    assert_match /c: <a href=".*">COUNTER<\/a>/, text
  end

  def test_used
    text = read_file("acceptance/counter_e.html")
    assert_match /Used by:[^\n]*>eiffelcode</, text
  end

end

$last_suite.add_test (TestEiffel.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
