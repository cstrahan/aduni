#!/usr/local/bin/ruby
# Ruby Unit Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

$last_suite = RUNIT::TestSuite.new

# --------------------------------------------------------------------

class TestOptions < RUNIT::TestCase
  def setup
    if not File.exists? ("acceptance/tmp") then
      Dir.mkdir "acceptance/tmp"
    end
  end

  def test_version
    re = Regexp.new "source2html, Version \\d+\\.\\d+\\.\\d+(beta)?\n"
    assert_match re, `./source2html -V`
    assert_match re, `./source2html --version`
  end

  def test_verbose
    text = `./source2html --verbose acceptance/*.rb --dir acceptance/tmp`
    assert_match /Generating HTML for/, text
    assert_match /acceptlib:[a-z ]*testeiffel/, text
    assert_match /acceptlib:[a-z ]*testruby/, text
    assert_equal text, `./source2html -v acceptance/*.rb --dir acceptance/tmp`
  end

  def test_usage
    text = `./source2html --usage`
    assert_match /Usage:\s+source2html\s+\[/, text
    assert_equal text, `./source2html -h`
  end

  def test_help
    text = `./source2html --help`
    text.gsub!(".","")
    assert_match /Synopsis/, text
  end

  def check_dir_option(opt)
    fn = "acceptance/tmp/rubycode_rb.html"
    if File.exists?(fn) then
      File.delete(fn)
    end
    `./source2html #{opt} acceptance/tmp acceptance/rubycode.rb`
    assert File.exists?("acceptance/tmp/source.html")
    assert File.exists?("acceptance/tmp/rubycode_rb.html")
  end

  def test_dir_option
    check_dir_option('-d')
    check_dir_option('--dir')
  end

  def check_init_option(opt)
    ['alt_index.html', 'rubycode_rb.html'].each { |fn|
      path = "acceptance/tmp/#{fn}"
      if File.exists?(path) then
	File.delete(path)
      end
    }
    system "./source2html #{opt} acceptance/alt.rc --dir acceptance/tmp acceptance/rubycode.rb"
    assert File.exists?("acceptance/tmp/alt_index.html"), "Option = #{opt}"
  end

  def test_init_option
    check_init_option '--init'
    check_init_option '-i'
  end

end

$last_suite.add_test (TestOptions.suite)



# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
