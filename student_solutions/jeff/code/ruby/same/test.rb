class Foo
  def hello
    puts "hello"
  end
  def hello1
    puts "hello world!"
  end
end

$b = 'hello'
bob = Foo.new
bob.send([$b,1].join)
