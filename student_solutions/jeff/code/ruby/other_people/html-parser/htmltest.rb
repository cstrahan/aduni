#! /usr/local/bin/ruby

require "html-parser"
require "formatter"

def htmltest(data)
  w = DumbWriter.new
  f = AbstractFormatter.new(w)
  p = HTMLParser.new(f)
  p.feed(data)
  p.close
end

file = 'test.html'
if ARGV[0]
  file = ARGV[0]
end

fp = open(file, 'r')
data = fp.read()
fp.close

htmltest(data)
