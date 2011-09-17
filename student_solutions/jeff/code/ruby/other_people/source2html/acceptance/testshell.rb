#!/usr/local/bin/ruby

require 'runit/testcase'
require 'runit/cui/testrunner'

require 'acceptance/acceptlib'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestShell < RUNIT::TestCase
  include BasicTests

  def setup
    generate_html
  end

  def test_basics
    text = read_file("acceptance/shellcode_sh.html")
    basic_tests(text)
  end

  def test_language
    text = read_file("acceptance/shellcode_sh.html")
    assert_match /language: Shell/, text
    assert_match /<pre><em># This is a comment<\/em>/, text    
    assert_match /<b>done<\/b>/, text    
  end

  def test_implicit
    text = read_file("acceptance/shell_implicit.html")
    assert_match /language: Shell/, text
    text = read_file("acceptance/bash_implicit.html")
    assert_match /language: Shell/, text
    text = read_file("acceptance/ksh_implicit.html")
    assert_match /language: Shell/, text
  end

end

$last_suite.add_test (TestShell.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
