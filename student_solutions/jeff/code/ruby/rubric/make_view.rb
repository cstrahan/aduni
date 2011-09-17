#-------------------------------------------------------------------------
# the uber-class
class RubricObject
  attr_writer :parent
  attr_reader :parent

  @@format = 'HTML'
  # what are the attributes we want all our objects to have?

  def initialize
    @parent = nil
  end

  # formatting methods
  def format
    begin
      # returns the object, appropriately formatted
      self.send ("format" + @@format)
    rescue
      # unknown formatting type!
      #      raise "Unknown format type"
    end
  end

  # formatting for HTML
  def formatHTML
      return "<!-- this object (" + self.to_s + ") cannot be formatted -->\n"
  end
  
  # formatting for VXML 
  def formatVXML
      return "<block>Sorry, this object cannot be formatted</block>\n"
  end

  def to_s
    "Object"
  end
end
#-------------------------------------------------------------------------
# creates a very basic html page
class Document
  def initialize(id, title, *objects)
    @title = title
    @objects = objects		# a number of RubricObjects
  end

  def makeView
    r = ""
    @objects.each do |obj|
      obj.parent = self
      r += obj.format
    end
    r += makeFooter
    return r
  end

  def makeFooter
    return "</body>\n" + "</html>"
  end

  def to_s
    makeView
  end
end
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
class RubricMethod < RubricObject
  def initialize(block)
    @block = block
  end
  
  def formatHTML
    eval @block
  end
end

# a basic form class (prototype)
class Form < RubricObject
  def initialize(target, *args)
    @target, @variables = target, args
  end

  def formatHTML
    r = '<form action="' + @target + '" method="GET" name="">' + "\n"
    # create the form fields
    @variables.each do |var|
      r += '<b>' + var + ':</b> ' +
	'<input size="30" name="' + var + '"' + 
	"><br>\n"
    end
    return r + '<input type="submit" name="submit">' + "\n" + '</form>' + "\n"
  end
end

#-------------------------------------------------------------------------
def basicTest
h =<<-FOO
 return "<html><head>\n" +
  "<title>" + @parent.title "</title>\n" +
  "</head>\n<body>\n" +
  "<center><h1>" + "</h1></center>\n"
FOO

  header = RubricMethod.new(h)
  # creates a form
  form = Form.new("http://www.alltheweb.com/cgi-bin/search", "query")

  # makes the document
  id = "index_html"
  test = Document.new(id, "Search the Web", header, form)

  # saves the document to a file
  f = File.open(id, "w")
  f.write test
  f.close
end
 
basicTest
