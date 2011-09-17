#!/usr/local/bin/ruby
# Ruby Acceptance Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestElisp < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def teardown
  end

  def read_file(file_name)
    file = File.open(file_name)
    lines = file.readlines.join('')
    file.close
    lines
  end

  def test_basics
    text = read_file("acceptance/elispcode_el.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/elispcode_el.html")
    assert_match /language: Elisp/, text
    assert_match /<pre><em>; This is a comment<\/em>/, text    
    assert_match /<b>defun<\/b>/, text    
    assert_match /(<b>setq<\/b> hook 'simple-function)/, text    
  end

end

$last_suite.add_test (TestElisp.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
