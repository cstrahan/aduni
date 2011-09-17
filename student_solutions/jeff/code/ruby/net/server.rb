# a ridiculously simple little web server

require 'socket'

class WebSession
  def initialize(connection)
    @connection = connection
  end
  def write(string)
    @connection.write string
  end
  def standardPage(title)
    write "HTTP/1.1 200 OK\r\n"
    write "Content-Type: text/html\r\n\r\n"
    write "<html><head><title>#{title}</title></head><body>\n"
    write yield if block_given?
    write "</body></html>"
  end
end

class WebServer
  def initialize(port)
    @listen = TCPServer.new('localhost', port || 8080)
    puts "server initalized and listening"
  end
  def run
    loop do
      Thread.start(@listen.accept) do |aConnection|
	begin
	  session = WebSession.new(aConnection)

	  request = []
	  loop do
	    line = aConnection.gets.chomp("\r\n")
	    break if line.length == 0
	    request << line
	  end

	  session.standardPage("Your Request") {
	    "<h1>Your request was:</h1>\n" +
	      request.join('<br>') +
	      "<p>Thank you for testing our system."
	  }	    
	ensure
	  aConnection.close
	end
      end
    end
  end
end



class EchoServer
  def initialize(port)
    @listen = TCPServer.new('localhost', port || 8080)
    puts "server initalized and listening"
  end
  def run
    loop do
      Thread.start(@listen.accept) do |aConnection|
	begin
	  @session = WebSession.new(aConnection)

	  # get user name
	  write "Welcome! What is your name?"
	  prompt
	  @name = aConnection.gets.chomp("\r\n")
	  write "Good to meet you #{@name}"

	  loop do
	    prompt
	    line = aConnection.gets.chomp("\r\n")
	    puts "#{@name}: #{line}"
	    if line == "QUIT"
	      @session.write "Goodbye"
	      break
	    end
	  end

	ensure
	  aConnection.close
	end
      end
    end
  end

  def prompt
    @session.write "> "
  end

  def write(string)
    @session.write string + "\n"
  end
end
    
EchoServer.new(ARGV[0]).run
	    
  
			    
