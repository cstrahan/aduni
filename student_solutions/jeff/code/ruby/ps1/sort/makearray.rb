# makearray.rb
# author: Jeffrey Radcliffe 
# date:   Mon Feb  5, 2001 12:54 PM

# makes an array of ARGV[0] numbers, 
# ranging randomly from 1 to ARGV[2]

aFile = File.new("sortme.txt", "w")
for i in 0..ARGV[0].to_i
  aFile.write("#{rand(ARGV[1].to_i)} ")
end
aFile.close

