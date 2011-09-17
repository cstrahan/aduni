#!/usr/local/bin/ruby
# Ruby Unit Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestMarkup < RUNIT::TestCase
  include BasicTests

  def test_escapes
    text = read_file("acceptance/JavaCode_java.html")
    assert_match /itsStack.depth\(\) &gt; 0\)/, text
  end
end

$last_suite.add_test (TestMarkup.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
