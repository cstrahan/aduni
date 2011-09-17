#!/usr/local/bin/ruby
# Ruby Acceptance Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestRuby < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def test_basics
    text = read_file("acceptance/rubycode_rb.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/rubycode_rb.html")
    assert_match /language: Ruby/, text
    assert_match /<pre><em># This is a comment<\/em>/, text    
    assert_match /<b>yield<\/b>/, text    
  end

  def test_reference
    text = read_file("acceptance/testruby_rb.html")
    assert_match /<a href=".*">acceptlib<\/a>/, text
    assert_match /acceptance\/<a href=".*">acceptlib<\/a>/, text
  end

  def test_used
    text = read_file("acceptance/acceptlib_rb.html")
    assert_match /Used by:.*testruby/, text
    assert_match /Used by:.*testeiffel/, text
  end

  def test_implicit
    text = read_file("acceptance/ruby_implicit.html")
    assert_match /language: Ruby/, text
    text = read_file("acceptance/ruby_implicit.html")
    assert_match /language: Ruby/, text
  end

end

$last_suite.add_test (TestRuby.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
