#!/usr/bin/env ruby
# Jeff's blackjack monster machine

#======================================================================
class Card
  attr_reader :suit, :value
  def initialize(suit, value)
    @suit = suit
    @value = value
  end
  def <=> (other)
    return  1 if @value == 1	# aces on top
    return -1 if @value < other.value
    return  1 if @value > other.value
    return  0
  end

  # returns the card as a string
  def to_s
    s = @value
    case @value			# converts face cards...
    when 11 ; s = "J"		# all of this isn't really
    when 12 ; s = "Q"		# necessary, it just makes
    when 13 ; s = "K"		# the output a bit nicer.
    when 1  ; s = "A"
    end
    "#{s}#{suit}"
  end
end
#======================================================================
# represents a player
class Player
  attr_reader :name
  def initialize(name, deck)
    @name = name
    @deck = deck
    @hand = []
  end

  def draw
    @hand << @deck.draw
  end

  def hand_total
    total = 0
    @hand.each do |card|
      value = card.value
      value = 10 if (card.value >= 10)    # face cards
      value = 11 if (card.value == 1) and (total + 11 <= 21)  # aces 
      total += value
    end
    return total
  end

  def to_s
    "#{@name}: #{@hand.sort.join(' ')}\ntotal = #{hand_total}"
  end
end
#======================================================================
class Deck
  attr_reader :cards

  def initialize(numDecks)
    @numDecks = [numDecks, 1].max  # rules out negative decks
    @cards = []			   # same as @cards = Array.new
    @numDecks.times do		    
      ["S", "H", "D", "C"].each do |suit|
	for value in 1...14 
	  @cards << Card.new(suit, value)  # push new card on deck 
	end
      end
    end
  end

  # Using the best shuffling algorithm, just like yours! =) 
  def shuffle
    numCards = @cards.length
    @cards.each_index do |i|
      j = rand(numCards - i) + i
      temp = @cards[i]
      @cards[i] = @cards[j]
      @cards[j] = temp
    end
  end

  # draws a card
  def draw
    @cards.pop
  end

  # returns the deck as a string
  def to_s
    "A deck(#{@numDecks}) of cards:\n#{@cards.join(' ')}"
  end
end
#======================================================================
# runs some tests
class Test
  def initialize
    deck = Deck.new(2)
    deck.shuffle

    player1 = Player.new("Jeffrey", deck)
    player2 = Player.new("Scott", deck)
    2.times do
      player1.draw
      player2.draw 
    end
    puts player1, player2
    player1.draw unless player1.hand_total >= 16
    puts player1, player2
  end
end

Test.new
	


