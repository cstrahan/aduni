LOAD DATA
INFILE *
APPEND
INTO TABLE quotations
FIELDS TERMINATED BY ','
(quotation_id, insertion_date, author_name, category, quote CHAR(4000))
BEGINDATA
64,1998-12-28,Philip K. Dick,reality,Reality is the stuff that doesn't go away just because you stop believing in it.
65,1998-12-29,Kenneth Olsen,predictions,There is no reason for any individual to have a computer in their home.
66,1998-12-29,Thomas Watson,predictions,I think there is a world market for maybe five computers.
67,1998-12-29,Lord Kelvin,predictions,Heavier-than-air flying machines are impossible.
68,1998-12-29,Walt Disney,reality,It's kind of fun to do the impossible.
69,1998-12-29,William Gibson,reality,I don't have to write about the future. For most people the present is enough like the future to be pretty scary .
70,1998-12-30,Joseph Kennedy,unreality,I have no political ambitions for myself or my children.
72,1998-12-30,Yogi Berra,advice,You got to be careful if you don't know where you're going because you might not get there.
73,1998-12-30,Walt Disney,confessions,I love Mickey Mouse more than any woman I have ever known.
74,1998-12-31,Al Capone,advice,You can get more with a kind word and a gun than you can with a kind word alone.
75,2000-01-29,Mel Brooks,reality,Tragedy is when I cut my finger. Comedy is when you fall down an open manhole cover and die.
76,2000-01-29,Jean Cocteau,art,Art produces ugly things which frequently become more beautiful with time. Fashion on the other hand produces beautiful things which always become ugly with time.
77,2000-01-29,John Moore,computers,He who hasn't hacked assembly language as a youth has no heart. He who does as an adult has no brain.
78,2000-01-29,Leo Buscaglia (divorced),love,Perfect love is rare indeed - for to be a lover will require that you continually have the subtlety of the very wise the flexibility of the child the sensitivity of the artist the understanding of the philosopher the acceptance of the saint the tolerance of the scholar and the fortitude of the certain.
79,2000-01-29,Brooke Shields,reality,If you're killed you've lost a very important part of your life.
