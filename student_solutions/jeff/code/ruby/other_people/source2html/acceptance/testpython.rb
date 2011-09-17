#!/usr/local/bin/ruby
# Ruby Acceptance Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestPython < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def test_basics
    text = read_file("acceptance/pythoncode_py.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/parrot_py.html")
    assert_match /language: Python/, text
    assert_match /<pre><em># This is a comment<\/em>/, text    
    assert_match /<b>class<\/b>/, text    
    assert_match /<b>def<\/b>/, text    
  end

  def test_implicit
    text = read_file("acceptance/python_implicit.html")
    assert_match /language: Python/, text
    text = read_file("acceptance/python_implicit.html")
    assert_match /language: Python/, text
  end

  def test_references
    text = read_file("acceptance/pythoncode_py.html")
    assert_match /<a href=".*">parrot<\/a>/, text
  end

end

$last_suite.add_test (TestPython.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
