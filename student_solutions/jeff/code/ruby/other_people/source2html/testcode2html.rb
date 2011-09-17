#!/usr/local/bin/ruby
# Ruby Unit Tests

require 'runit/testcase'
require 'runit/cui/testrunner'

load 'source2html'

$last_suite = RUNIT::TestSuite.new

module RUNIT
  module Assert
    def assert_not_match(str, re, message="")
      setup_assert
      if re =~ str
	msg = edit_message(message)
        r = to_str(re)
	s = to_str(str)
	msg.concat ("<" + r + "> matches <" + s + ">")
	raise_assertion_error(msg, 2)
      end
    end
    alias assert_not_matches assert_not_match
  end
end


# --------------------------------------------------------------------

class StringIo
  attr_reader :value

  def initialize
    @value = ""
  end

  def write(str)
    @value += str
  end
end

# --------------------------------------------------------------------

class TestFormatter < RUNIT::TestCase
  def setup
    @res = StringIo.new
    @fmt = Formatter.new(@res)
  end

  def teardown
    @fmt = nil
  end

  def test_stringio
    @res.write ("HI")
    assert_equal "HI", @res.value
  end

  def test_simple
    @fmt.emit_plain("Straight Text")
    assert_equal "Straight Text", @res.value
  end

  def test_plain
    @fmt.emit_plain("<p> Room & Board")
    assert_equal "&lt;p&gt; Room &amp; Board", @res.value
  end

  def test_raw
    @fmt.emit_raw("<h1>Header</h1>")
    assert_equal "<h1>Header</h1>", @res.value
  end

  def test_keyword_format
    @fmt.emit_keyword("def")
    assert_equal "<b>def</b>", @res.value
  end

  def test_comment_format
    @fmt.emit_comment("comment")
    assert_equal "<em>comment</em>", @res.value
  end

  def test_color
    @fmt.set_comment_style("<b><font color=\"grey\">", "</font></b>")
    @fmt.set_keyword_style("<b><font color=\"blue\">", "</font></b>")
    @fmt.emit_keyword("def")
    @fmt.emit_comment("hi")
    assert_match /<b><font color="grey">hi<\/font><\/b>/, @res.value
    assert_match /<b><font color="blue">def<\/font><\/b>/, @res.value
  end
end

$last_suite.add_test (TestFormatter.suite)



# --------------------------------------------------------------------

class TestSourceFile < RUNIT::TestCase
  def setup
    SourceFile.clear_list
    @src = SourceFile.new ("dir/x.rb")
  end

  def teardown
    @src = nil
  end

  def test_attributes
    assert_equal "dir/x.rb", @src.pathname
    assert_equal "rb", @src.suffix
    assert_equal "x", @src.basename
    assert_equal "x.rb", @src.filename
    assert_equal "x_rb.html", @src.htmlname
    assert_equal "x", @src.reference_target
  end

  def test_attributes_no_suffix
    s = SourceFile.new ("dir/zippy")
    assert_equal "dir/zippy", s.pathname
    assert_equal "", s.suffix
    assert_equal "zippy", s.basename
    assert_equal "zippy", s.filename
    assert_equal "zippy.html", s.htmlname
    assert_equal UnknownInfo, s.language.type
  end

  def test_attributes_no_dir
    s = SourceFile.new ("bob.java")
    assert_equal "bob.java", s.pathname
    assert_equal "java", s.suffix
    assert_equal "bob", s.basename
    assert_equal "bob.java", s.filename
    assert_equal "bob_java.html", s.htmlname
  end

  def test_language
    assert_equal RubyInfo, @src.language.type
    assert_equal EiffelInfo, SourceFile.new("x.e").language.type
    assert_equal ElispInfo,  SourceFile.new("x.el").language.type
    assert_equal JavaInfo,   SourceFile.new("x.java").language.type
    assert_equal PerlInfo,   SourceFile.new("x.pm").language.type
    assert_equal PerlInfo,   SourceFile.new("x.pl").language.type
    assert_equal RubyInfo,   SourceFile.new("x.rb").language.type
    assert_equal ShellInfo,  SourceFile.new("x.sh").language.type
    assert_equal ShellInfo,  SourceFile.new("x.ksh").language.type
  end

  def test_language_by_line
    @src.select_language_from_line("#!/usr/local/bin/ruby")
    assert_equal RubyInfo, @src.language.type
    @src.select_language_from_line("#!/usr/bin/perl")
    assert_equal PerlInfo, @src.language.type
    @src.select_language_from_line("#!/bin/sh")
    assert_equal ShellInfo, @src.language.type
    @src.select_language_from_line("#!/bin/ksh")
    assert_equal ShellInfo, @src.language.type
    @src.select_language_from_line("#!/bin/bash")
    assert_equal ShellInfo, @src.language.type
  end

  def test_lookup
    a = SourceFile.new ("path/to/a.java")
    assert_equal @src, SourceFile.by_name("x")
    assert_equal a, SourceFile.by_name("a")
  end

  def test_list
    a = SourceFile.new ("path/to/a.java")
    assert_equal [a, @src], SourceFile.list
  end

  def test_comments
    assert_matches "# ---", @src.language.comment_re
    assert_matches "() # ---", @src.language.comment_re
    assert_not_matches '#{x}', @src.language.comment_re
    assert_not_matches '#$x', @src.language.comment_re
    assert_not_matches '#@x', @src.language.comment_re
    assert_equal "# ---", ("# ---").sub (@src.language.comment_re, "#")
  end

  def test_package
    assert_equal "", @src.packagename
    @src.set_packagename("source2html.stuff")
    assert_equal "source2html.stuff", @src.packagename
  end
end

$last_suite.add_test (TestSourceFile.suite)


# --------------------------------------------------------------------

class TestReferences < RUNIT::TestCase
  def setup
    @ref = References.new
  end

  def teardown
    @ref = nil
  end

  def test_create
    assert @ref != nil
  end

  def test_one_ref
    @ref.add_reference("a", "b")
    assert_equal ["b"], @ref.referers_of("a")
  end

  def test_many_refs
    @ref.add_reference("a", "w")
    @ref.add_reference("a", "z")
    @ref.add_reference("a", "y")
    @ref.add_reference("a", "x")
    @ref.add_reference("b", "q")
    @ref.add_reference("b", "p")
    assert_equal ["w", "x", "y", "z"], @ref.referers_of("a")
    assert_equal ["p", "q"], @ref.referers_of("b")
  end

  def test_no_refs
    assert_nil @ref.referers_of("never referenced")
  end

  def test_targets
    count_source = SourceFile.new("count.e")
    @ref.add_reference_target("COUNT", count_source)
    assert_equal count_source, @ref.get_source_for_reference("COUNT")
    assert_nil @ref.get_source_for_reference("count")
    assert_nil @ref.get_source_for_reference("xyzzy")
  end

end

$last_suite.add_test (TestReferences.suite)


# --------------------------------------------------------------------

class TestReferenceAnalysis < RUNIT::TestCase
  def setup
    @src = SourceFile.new("Calc.java")
    @stk = SourceFile.new("Stack.java")
    @ref = References.new
    @ref.add_reference_target("Stack", @stk)
    @ra  = ReferenceAnalyzer.new(@ref)
  end

  def teardown
    @src = nil
    @stk = nil
    @ref = nil
  end

  def test_analyze_line
    @ra.analyze_line(@src, " s = Stack.new")
    assert_equal ["Calc"], @ref.referers_of("Stack")
  end

  def test_ruby_references
    rubylib_source = SourceFile.new ("rubylib.rb")
    @ref.add_reference_target("rubylib", rubylib_source)
    s = SourceFile.new ("main.rb")
    @ra.analyze_line(s, "require 'rubylib'\n")
    assert_equal ["main"], @ref.referers_of("rubylib")
  end

end

$last_suite.add_test (TestReferenceAnalysis.suite)


# --------------------------------------------------------------------

class TestJavaInfo < RUNIT::TestCase
  def setup
    @info = JavaInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('import')
    assert ! @info.is_keyword ('jim')
  end

  def test_parsing
    input = 'stack.push(5+9);'
    expected = %w'stack . push ( 5 + 9 );'
    while not expected.empty?
      input =~ @info.token_re
      tok, input = $1, $2
      assert_equal expected.shift, tok
    end
  end
end

$last_suite.add_test (TestJavaInfo.suite)


# --------------------------------------------------------------------

class TestRubyInfo < RUNIT::TestCase
  def setup
    @info = RubyInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('yield')
    assert ! @info.is_keyword ('jim')
  end
end

$last_suite.add_test (TestRubyInfo.suite)


# --------------------------------------------------------------------

class TestPythonInfo < RUNIT::TestCase
  def setup
    @info = PythonInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('finally')
    assert   @info.is_keyword ('lambda')
    assert ! @info.is_keyword ('jim')
  end
end

$last_suite.add_test (TestPythonInfo.suite)


# --------------------------------------------------------------------

class TestPerlInfo < RUNIT::TestCase
  def setup
    @info = PerlInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('continue')
    assert ! @info.is_keyword ('jim')
  end

  def test_suffixes
    assert_equal %w(pm pl).sort, @info.suffixes.sort
  end

end

$last_suite.add_test (TestPerlInfo.suite)


# --------------------------------------------------------------------

class TestEiffelInfo < RUNIT::TestCase
  def setup
    @info = EiffelInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('require')
  end
end

$last_suite.add_test (TestEiffelInfo.suite)


# --------------------------------------------------------------------

class TestElispInfo < RUNIT::TestCase
  def setup
    @info = ElispInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('cond')
  end
end

$last_suite.add_test (TestElispInfo.suite)


# --------------------------------------------------------------------

class TestShellInfo < RUNIT::TestCase
  def setup
    @info = ShellInfo.new
  end

  def teardown
    @info = nil
  end

  def test_keywords
    assert   @info.is_keyword ('fi')
  end
end

$last_suite.add_test (TestShellInfo.suite)


# --------------------------------------------------------------------

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.quiet_mode = true
  RUNIT::CUI::TestRunner.run ($last_suite)
end
