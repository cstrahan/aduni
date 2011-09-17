#!/usr/local/bin/ruby

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestPerl < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def test_basics
    text = read_file("acceptance/perlcode_pl.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/perlcode_pl.html")
    assert_match /language: Perl/, text
    assert_match /<pre><em># This is a comment<\/em>/, text    
    assert_match /<b>foreach<\/b>/, text    
  end

  def test_implicit
    text = read_file("acceptance/perl_implicit.html")
    assert_match /language: Perl/, text
  end

end

$last_suite.add_test (TestPerl.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
