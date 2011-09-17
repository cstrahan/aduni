#!/bin/env ruby

# install script for Rubric

def getInstallPath
  $:.each do |path|
    if path =~ /.*1.[67]/
      path
      return path.sub(/i[56]86-linux/, "")
    end
    return nil
  end
end

path = getInstallPath
puts  "Welcome to Rubric! This will just take a moment."
print "Okay to install rubric to this path? [#{path}] (y/n) "
response = gets
if response =~ /([Yy])/
  puts "Okay, installing now ..."
  # do install things
end
  
