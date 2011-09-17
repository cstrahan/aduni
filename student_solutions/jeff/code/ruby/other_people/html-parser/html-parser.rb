# HTML parser

require 'sgml-parser'

class HTMLParser < SGMLParser

  def initialize(formatter, verbose=nil)
    super(verbose)
    @formatter = formatter
    @savedata = nil
    @isindex = 0
    @title = nil
    @base = nil
    @anchor = nil
    @anchorlist = []
    @nofill = 0
    @list_stack = []
  end


  def handle_data(data)
    if @savedata
      @savedata = @savedata + data
    else
      if @nofill != 0
        @formatter.add_literal_data(data)
      else
        @formatter.add_flowing_data(data)
      end
    end
  end

  def save_bgn
    @savedata = ''
  end

  def save_end
    data = @savedata
    @savedata = nil
    data = '' if data == nil
    if @nofill == 0
      data = data.split.join(" ")
    end
    return data
  end

  def anchor_bgn(href, name, type)
    @anchor = href
    if @anchor
      @anchorlist << href
    end
  end

  def anchor_end
    if @anchor
      #handle_data(format "[%d]", @anchorlist.length)
      @anchor = nil
    end
  end

  def handle_image(src, alt, *args)
    handle_data(alt)
  end

  def start_html(attrs) end
  def end_html() end

  def start_head(attrs) end
  def end_head() end

  def start_body(attrs) end
  def end_body() end

  def start_title(attrs)
    save_bgn
  end

  def end_title
    @title = save_end
  end

  def do_base(attrs)
    for a, v in attrs
      if a == 'href'
        @base = v
      end
    end
  end

  def do_isindex(attrs)
    @isindex = 1
  end

  def do_link(attrs)
  end

  def do_meta(attrs)
  end

  def do_nextid(attrs) # Deprecated
  end


  def start_h1(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font('h1', 0, 1, 0)
  end

  def end_h1
    @formatter.end_paragraph(1)
    @formatter.pop_font()
  end

  def start_h2(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font('h2', 0, 1, 0)
  end

  def end_h2
    @formatter.end_paragraph(1)
    @formatter.pop_font()
  end

  def start_h3(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font('h3', 0, 1, 0)
  end

  def end_h3
    @formatter.end_paragraph(1)
    @formatter.pop_font()
  end

  def start_h4(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font('h4', 0, 1, 0)
  end

  def end_h4
    @formatter.end_paragraph(1)
    @formatter.pop_font()
  end

  def start_h5(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font('h5', 0, 1, 0)
  end

  def end_h5
    @formatter.end_paragraph(1)
    @formatter.pop_font()
  end

  def start_h6(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font('h6', 0, 1, 0)
  end

  def end_h6
    @formatter.end_paragraph(1)
    @formatter.pop_font()
  end

  def do_p(attrs)
    @formatter.end_paragraph(1)
  end

  def start_pre(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_font(nil, nil, nil, 1)
    @nofill = @nofill + 1
  end

  def end_pre
    @formatter.end_paragraph(1)
    @formatter.pop_font()
    @nofill = @nofill - 1
    if @nofill < 0 then @nofill = 0 end
  end

  def start_xmp(attrs)
    start_pre(attrs)
    setliteral('xmp') # Tell SGML parser
  end

  def end_xmp
    end_pre
  end

  def start_listing(attrs)
    start_pre(attrs)
    setliteral('listing') # Tell SGML parser
  end

  def end_listing
    end_pre
  end

  def start_address(attrs)
    @formatter.end_paragraph(0)
    @formatter.push_font(nil, 1, nil, nil)
  end

  def end_address
    @formatter.end_paragraph(0)
    @formatter.pop_font()
  end

  def start_blockquote(attrs)
    @formatter.end_paragraph(1)
    @formatter.push_margin('blockquote')
  end

  def end_blockquote
    @formatter.end_paragraph(1)
    @formatter.pop_margin()
  end

  def start_ul(attrs)
    @formatter.end_paragraph(0)
    @formatter.push_margin('ul')
    @list_stack << ['ul', '*', 0]
  end

  def end_ul
    if @list_stack
      @list_stack.pop
    end
    @formatter.end_paragraph(0)
    @formatter.pop_margin
  end

  def do_li(attrs)
    @formatter.end_paragraph(0)
    if @list_stack && @list_stack.size > 0
      dummy, label, counter = top = @list_stack[-1]
      top[2] = counter = counter+1
    else
      label, counter = '*', 0
    end
    @formatter.add_label_data(label, counter)
  end

  def start_ol(attrs)
    @formatter.end_paragraph(0)
    @formatter.push_margin('ol')
    label = '1.'
    for a, v in attrs
      if a == 'type'
        if v.length == 1
          v = v + '.'
          label = v
        end
      end
    end
    @list_stack << ['ol', label, 0]
  end

  def end_ol
    if @list_stack
      @list_stack.pop
    end
    @formatter.end_paragraph(0)
    @formatter.pop_margin
  end

  def start_menu(attrs)
    start_ul(attrs)
  end

  def end_menu
    end_ul
  end

  def start_dir(attrs)
    start_ul(attrs)
  end

  def end_dir
    end_ul
  end

  def start_dl(attrs)
    @formatter.end_paragraph(1)
    @list_stack << ['dl', '', 0]
  end

  def end_dl
    ddpop(1)
    if @list_stack.length > 0
      @list_stack.pop
    end
  end

  def do_dt(attrs)
    ddpop
  end

  def do_dd(attrs)
    ddpop
    @formatter.push_margin('dd')
    @list_stack << ['dd', '', 0]
  end

  def ddpop(bl=0)
    @formatter.end_paragraph(bl)
    if @list_stack.length > 0
      if @list_stack[-1][0] == 'dd'
        @list_stack.pop
        @formatter.pop_margin
      end
    end
  end

  def start_cite(attrs) start_i(attrs) end
  def end_cite() end_i end

  def start_code(attrs) start_tt(attrs) end
  def end_code() end_tt end

  def start_em(attrs) start_i(attrs) end
  def end_em() end_i end

  def start_kbd(attrs) start_tt(attrs) end
  def end_kbd() end_tt end

  def start_samp(attrs) start_tt(attrs) end
  def end_samp() end_tt end

  def start_strong(attrs) start_b(attrs) end
  def end_strong() end_b end

  def start_var(attrs) start_i(attrs) end
  def end_var() end_i end

  def start_i(attrs)
    @formatter.push_font(nil, 1, nil, nil)
  end
  def end_i
    @formatter.pop_font
  end

  def start_b(attrs)
    @formatter.push_font(nil, nil, 1, nil)
  end
  def end_b
    @formatter.pop_font
  end

  def start_tt(attrs)
    @formatter.push_font(nil, nil, nil, 1)
  end
  def end_tt
    @formatter.pop_font
  end

  def start_a(attrs)
    href = nil
    name = nil
    type = nil
    for attrname, value in attrs
      value = value.strip
      if attrname == 'href'
        href = value
      end
      if attrname == 'name'
        name = value
      end
      if attrname == 'type'
        type = value.downcase
      end
    end
    anchor_bgn(href, name, type)
  end

  def end_a
    anchor_end
  end

  def do_br(attrs)
    @formatter.add_line_break
  end

  def do_hr(attrs)
    @formatter.add_hor_rule
  end

  def do_img(attrs)
    align = nil
    alt = '(image)'
    ismap = nil
    src = nil
    width = 0
    height = 0
    for attrname, value in attrs
      if attrname == 'align'
        align = value
      end
      if attrname == 'alt'
        alt = value
      end
      if attrname == 'ismap'
        ismap = value
      end
      if attrname == 'src'
        src = value
      end
      if attrname == 'width'
        width = Integer(value)
      end
      if attrname == 'height'
        height = Integer(value)
      end
    end
    handle_image(src, alt, ismap, align, width, height)
  end

  def do_plaintext(attrs)
    start_pre(attrs)
    setnomoretags # Tell SGML parser
  end

  def unknown_starttag(tag, attrs)
  end

  def unknown_endtag(tag)
  end

end
