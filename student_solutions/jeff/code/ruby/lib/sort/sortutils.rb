# ----------------------------
#       UTILITY METHODS
# ----------------------------
def swap(array, a, b)
  temp = array[a]
  array[a] = array[b]
  array[b] = temp
end

def display(array)
  array.each do |i|
    print i, " "
  end
  print "\n"  
end  
