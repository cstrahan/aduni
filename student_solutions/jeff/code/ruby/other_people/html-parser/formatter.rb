class NullFormatter
  def initialize(writer=nil)
    if not writer
      writer = NullWriter.new
    end
    @writer = writer
  end
  def end_paragraph(blankline) end
  def add_line_break() end
  #def add_hor_rule(*args, **kw) end
  def add_hor_rule() end
  def add_label_data(format, counter, blankline=nil) end
  def add_flowing_data(data) end
  def add_literal_data(data) end
  def flush_softspace() end
  def push_alignment(align) end
  def pop_alignment() end
  def push_font(size, i, b, tt) end
  def pop_font() end
  def push_margin(margin) end
  def pop_margin() end
  def set_spacing(spacing) end
  def push_style(*styles) end
  def pop_style(n=1) end
  def assert_line_data(flag=1) end
end

class AbstractFormatter < NullFormatter

  def initialize(writer)
    @writer = writer
    @align = nil
    @align_stack = []
    @font_stack = []
    @margin_stack = []
    @spacing = nil
    @style_stack = []
    @nospace = true
    @softspace = false
    @para_end = true
    @parskip = 0
    @hard_break = true
    @have_label = false
  end

  def end_paragraph(blankline)
    if not @hard_break
      @writer.send_line_break
      @have_label = false
    end
    if (@parskip < blankline) and !@have_label
      @writer.send_paragraph(blankline - @parskip)
      @parskip = blankline
      @have_label = 0
    end
    @hard_break = true
    @nospace = true
    @para_end = true
    @softspace = false
  end

  def add_line_break
    if not (@hard_break or @para_end)
      @writer.send_line_break
      @have_label = false
      @parskip = 0
    end
    @hard_break = true
    @nospace = true
    @softspace = false
  end

  def add_hor_rule
    if not @hard_break
      @writer.send_line_break
    end
    @writer.send_hor_rule #(args, kw)
    @hard_break = true
    @nospace = true
    @have_label = false
    @para_end = false
    @softspace = false
    @parskip = 0
  end

  def add_label_data(format, counter, blankline = nil)
    if @have_label or !@hard_break
      @writer.send_line_break
    end
    if not @para_end
      @writer.send_paragraph(1)  #((blankline and 1) or 0)
    end
    if format.kind_of? String
      #@writer.send_label_data(format_counter(format, counter))
    else
      @writer.send_label_data(format)
    end
    @nospace = @have_label = @hard_break = @para_end = 1
    @softspace = @parskip = 0
  end

  def add_flowing_data(data)
    return if not data
    prespace = (/^\s/ =~ data)
    postspace = (/\s$/ =~ data)
    data = data.split.join(" ")
    if @nospace and data.length < 1
      return
    elsif prespace or @softspace
      if data.length == 0
        if not @nospace
          @softspace = true
          @parskip = 0
        end
        return
      end
      if not @nospace
        data = ' ' + data
      end
    end
    @hard_break = @nospace = @para_end = false
    @have_label = false
    @parskip = 0
    @softspace = postspace
    @writer.send_flowing_data(data)
  end

  def add_literal_data(data)
    return if not data
    if @softspace
      @writer.send_flowing_data("_")
    end
    @hard_break = (data[-1,1] == "\n")
    @nospace = @para_end = @softspace = false
    @have_label = false
    @parskip = 0
    @writer.send_literal_data(data)
  end

  def flush_softspace
    if @softspace
      @hard_break = false
      @para_end = false
      @have_label = false
      @softspace = false
      @parskip = 0
      @nospace = true
      @writer.send_flowing_data(' ')
    end
  end

  def push_font(size, i, b, tt)
    if @softspace
      @hard_break = false
      @para_end = false
      @softspace = false
      @nospace = true
      @writer.send_flowing_data(' ')
    end
    if @font_stack.length > 0
      csize, ci, cb, ctt = @font_stack[-1]
      if size == nil
        size = csize
      end
      if i == nil
        i = ci
      end
      if b == nil
        b = cb
      end
      if tt == nil
        tt = ctt
      end
    end
    font = [size, i, b, tt]
    @font_stack << font
    @writer.new_font(font)
  end

  def pop_font
    if @font_stack.length > 0
      @font_stack.pop
    end
    if @font_stack
      font = @font_stack[-1]
    else
      font = nil
    end
    @writer.new_font(font)
  end

  def push_margin(margin)
    @margin_stack << margin
    fstack = @margin_stack.compact
    if (!margin) && (fstack.length > 0)
      margin = fstack[-1]
    end
    @writer.new_margin(margin, fstack.length)
  end

  def pop_margin
    if @margin_stack
      @margin_stack.pop
    end
    fstack = @margin_stack.compact
    if fstack.length > 0
      margin = fstack[-1]
    else
      margin = nil
    end
    @writer.new_margin(margin, fstack.length)
  end

  def set_spacing(spacing)
    @spacing = spacing
    @writer.new_spacing(spacing)
  end

  def assert_line_data(flag=true)
    @nospace = !flag
    @hard_break = !flag
    @para_end = false
    @have_label = false
    @parskip = 0
  end

end

class NullWriter
  def initialize() end
  def flush() end
  def new_alignment(align) end
  def new_font(font) end
  def new_margin(margin, level) end
  def new_spacing(spacing) end
  def new_styles(styles) end
  def send_paragraph(blankline) end
  def send_line_break() end
  def send_hor_rule() end
  def send_label_data(data) end
  def send_flowing_data(data) end
  def send_literal_data(data) end
end

class DumbWriter < NullWriter

  def initialize(file=STDOUT, maxcol=72)
    @file = file
    @maxcol = maxcol
    super()
    reset
  end

  def reset
    @col = 0
    @atbreak = false
  end

  def send_paragraph(blankline)
    @file.write("\n" + "\n"*blankline)
    @col = 0
    @atbreak = false
  end

  def send_line_break
    @file.write("\n")
    @col = 0
    @atbreak = false
  end

  def send_hor_rule #(*args, **kw)
    @file.write("\n")
    @file.write('-'*@maxcol)
    @file.write("\n")
    @col = 0
    @atbreak = false
  end

  def send_literal_data(data)
    @file.write(data)
    i = data.rindex("\n")
    if i
      @col = 0
      data = data[i+1..-1]
      #data = string.expandtabs(data)
      @col = @col + data.length
      @atbreak = false
    end
  end

  def send_flowing_data(data)
    return if not data
    atbreak = (@atbreak || (/^\s/ =~ data))
    col = @col
    maxcol = @maxcol
    for word in data.split
      if atbreak
        if col + word.length >= maxcol
          @file.write("\n")
          col = 0
        else
          @file.write(' ')
          col = col + 1
        end
      end
      @file.write(word)
      col = col + word.length
      atbreak = true
    end
    @col = col
    @atbreak = (/\s$/ =~ data)
  end

end
