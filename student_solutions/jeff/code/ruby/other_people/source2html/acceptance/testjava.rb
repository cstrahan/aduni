#!/usr/local/bin/ruby

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestJava < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def test_basics
    text = read_file("acceptance/JavaCode_java.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/JavaCode_java.html")
    assert_match /language: Java/, text
    assert_match /<pre><em>\/\/ This is a comment<\/em>/, text    
    assert_match /<b>synchronized<\/b>/, text    
  end

  def test_references
    text = read_file("acceptance/JavaCode_java.html")
    assert_match /<a href=".*">Stack<\/a>/, text
  end

  def test_used
    text = read_file("acceptance/Stack_java.html")
    assert_match /Used by:/, text
  end

  def test_not_used
    text = read_file("acceptance/JavaCode_java.html")
    assert text !~ /Used by:[^\n]*JavaCode<\/a>/
  end
end

$last_suite.add_test (TestJava.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
