#!/usr/local/bin/ruby
# Ruby Unit Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestColor < RUNIT::TestCase
  include BasicTests

  def setup
    if not File.exists? ("acceptance/tmp_color") then
      Dir.mkdir "acceptance/tmp_color"
      system ("./source2html acceptance/rubycode.rb " +
	      "--init acceptance/color.rc --dir acceptance/tmp_color")
    end
  end

  def test_color
    text = read_file "acceptance/tmp_color/rubycode_rb.html"
    assert_match /<b><font color="blue">def<\/font><\/b>/, text
    assert_match /<em><font color="red"># This is a comment<\/font><\/em>/, text
  end
end

$last_suite.add_test (TestColor.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
