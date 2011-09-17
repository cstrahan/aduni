require 'net/http'

if ARGV[0] == nil
  name = '209.6.192.245'
else
  name = ARGV[0]
end

puts "fetching index from #{name}"
h = Net::HTTP.new(name, 80)
resp, data = h.get('/index.pl', nil)
File.open("foo.html","w").puts(data)
puts "done"

