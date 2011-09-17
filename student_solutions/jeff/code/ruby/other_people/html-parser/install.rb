#!/usr/bin/env ruby

require 'rbconfig'
require 'find'
require 'ftools'
require 'getoptlong'

include Config

$srcdir = CONFIG["srcdir"]
$version = CONFIG["MAJOR"]+"."+CONFIG["MINOR"]
$libdir = File.join(CONFIG["libdir"], "ruby", $version)
$archdir = File.join($libdir, CONFIG["arch"])
$site_libdir = CONFIG["sitedir"]
if !$site_libdir
  $site_libdir = $:.find {|x| x =~ /site_ruby$/}
end
if !$site_libdir
  $site_libdir = File.join($libdir, "site_ruby")
end

def install_rb(libdir = "lib", files = nil)
  path = []
  dir = []
  if files
    path = files
    dir |= libdir
  else
    Find.find(libdir) do |f|
      next if (f = f[libdir.length+1..-1]) == nil
      path.push f if File.ftype(File.join(libdir, f)) == 'file'
      dir |= File.dirname(f)
    end
  end
  for f in dir
    if f == "."
      File::makedirs($site_libdir)
    else
      File::makedirs(File.join($site_libdir, f))
    end
  end
  for f in path
    File::install(File.join(libdir, f), File.join($site_libdir, f), nil, true)
  end
end

install_rb(".", ["formatter.rb", "html-parser.rb", "sgml-parser.rb"])
