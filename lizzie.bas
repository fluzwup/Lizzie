! RFO BASIC 1.7 for Android
! Eliza + query engine

! array size values; may be oversized
kwcount = 50
replycount = 150

userIn$ = "none"
keyword$ = "none"
reply$ = "none"
rest$ = "none"
final$ = "none"

! list of keywords
dim kwlist$[kwcount]
! list of keyword mappings
dim kwmap[kwcount]
dim numreplies[kwcount]
dim replylist$[replycount]

print "Initializing text to speech"

! set up the data
tts.init

print "Loading data"

kwindex = 1
replyindex = 1
read.next numkeywords
do
  read.next numreplies
  print ".";

  for x = 1 to numkeywords
    read.next kwlist$[kwindex]
    kwmap[kwindex] = replyindex
    numreplies[kwindex] = numreplies
    kwindex = kwindex + 1
  next x

  for x = 1 to numreplies
    read.next replylist$[replyindex]
    replyindex = replyindex + 1
  next x

  
  read.next numkeywords
until numkeywords < 1

! update kwcount and replycount with real values
kwcount = kwindex - 1
replycount = replyindex - 1

print "Data prepared;", kwcount, "keywords,", replycount, "replies"

! these are the I/O functions, used to switch between speech and text

fn.def ask(txtOut$)
  ret = textOut(txtOut$)
 answer$ = ""
 ret = textIn(&answer$)
  if userIn$ = "no" then fn.rtn 0
  fn.rtn 1
fn.end

fn.def textOut(txtOut$)
  print "Saying",  txtOut$
  tts.speak txtOut$
  fn.rtn 1
fn.end

fn.def textIn(txtIn$)
  stt.listen
  stt.results speech
  list.get speech, 1, txtIn$ 
  print "Heard", txtIn$
  fn.rtn 1
fn.end

goto mainLoop

! function to deal with pronoun changes and conjugation
conjugate:
token$ = ""
! step through rest$, copy over one token at a time, conjagating as we go
! start at 2, because position 1 is a space
first= 2
last = nextSp = is_in(" ", rest$, first)
while last > 1
  token$ = mid$(rest$, first, last - first)
  print "Parsing token <"; token$; ">"
  sw.begin token$
  sw.case "ARE"
    final$ = final$ + " " + "AM"
    sw.break
  sw.case "AM"
    final$ = final$ + " " + "ARE"
    sw.break
  sw.case "WERE"
    final$ = final$ + " " + "WAS"
    sw.break
  sw.case "WAS"
    final$ = final$ + " " + "AM"
    sw.break
  sw.case "YOU"
    final$ = final$ + " " + "I"
    sw.break
  sw.case "I"
    final$ = final$ + " " + "YOU"
    sw.break
  sw.case "YOUR"
    final$ = final$ + " " + "MY"
    sw.break
  sw.case "MY"
    final$ = final$ + " " + "YOUR"
    sw.break
  sw.case "I'VE"
    final$ = final$ + " " + "YOU'VE"
    sw.break
  sw.case "YOU'VE"
    final$ = final$ + " " + "I'VE"
    sw.break
  sw.case "I'M"
    final$ = final$ + " " + "YOU'RE"
    sw.break
  sw.case "YOU'RE"
    final$ = final$ + " " + "I'M"
    sw.break
  sw.case "ME"
    final$ = final$ + " " + "YOU"
    sw.break
  sw.case "YOU"
    final$ = final$ + " " + "ME"
    sw.break
  sw.default
    final$ = final$ + " " + token$
    sw.break
  sw.end

  first = last + 1
  last = nextSp = is_in(" ", rest$, first)
repeat

return

! text parsing function
parseString:
  print "Parsing " + userIn$, "Looking at", kwcount, "keywords"
  hit = 0
  hitlen = 0
  hitoffset = 0
  userIn$ = " " + userIn$ + " "

! loop through the list of keywords; don't stop when we get a hit, look for a longer hit
  for x = 1 to kwcount
    offset = is_in(" " + kwlist$[x] + " ", userIn$)
    if offset > 0 then
      slen = len(kwlist$[x])
      if slen > hitlen
        hit = x
        hitlen = slen
        hitoffset = offset
      endif
    endif
  next x

  if hit = 0
    hit = kwcount
    keyword$ = kwlist$[kwcount]
  endif

  print "Found keyword", kwlist$[hit], "index", hit, "at offset", hitoffset

! grab the tokens; keyword and portion to the right of the keyword
  keyword$ = kwlist$[hit]
  restCount = len(userIn$) - hitoffset - hitlen
  rest$ = right$(userIn$, restCount)

! congugate the right part
  gosub conjugate

! choose randomly from the replies
  replyindex = kwmap[hit]
  range = numreplies[hit] - 0.001

  replyindex = int(replyindex + range * rnd())

  print "Getting reply at", replyindex

  reply$ = replylist$[replyindex]

return




! start the main program loop
mainLoop:
ret = textOut("Hello, I am Lizzie.  How may I help you?")

while 1
  ret = textIn(&userIn$)
  userIn$ = upper$(userIn$)
  
  if userIn$ = "GOODBYE" then goto terminate

!  ret = parseString(&userIn$, &keyword$, &reply$, &rest$)
  gosub parseString

  lastChar$ = right$(reply$, 1)
  if lastChar$ = "*" then    
    ret = textOut(left$(reply$, len(reply$) - 1) + rest$)
  else if lastChar$ = "&" then
    ret = ask("Would you like me to look up " + keyword$ + rest$)
    if ret <> 0 then
      browse "http://www.google.com/search?q=" + keyword$ + rest$
    end if
  else
    ret = textOut(reply$)
  endif

repeat

! cleanup and exit
terminate:
ret = textOut("Goodbye.")

! keword/reply data in the form of: #keywords, #replies, kewords..., replies...

READ.DATA 1, 3, "CAN YOU", "DON'T YOU BELIEVE THAT I CAN*", "PERHAPS YOU WOULD LIKE TO BE LIKE ME*", "YOU WANT ME TO BE ABLE TO*"
READ.DATA 1, 2, "CAN I", "PERHAPS YOU DON'T WANT TO*", "DO YOU WANT TO BE ABLE TO*"
READ.DATA 2, 4, "YOU ARE","YOU'RE", "WHAT MAKES YOU THINK I AM*", "DOES IT PLEASE YOU TO BELIEVE I AM*", "PERHAPS YOU WOULD LIKE TO BE*", "DO YOU SOMETIMES WISH YOU WERE*"
READ.DATA 1, 4, "I DON'T", "DON'T YOU REALLY*", "WHY DON'T YOU*", "DO YOU WISH TO BE ABLE TO*", "DOES THAT TROUBLE YOU*"
READ.DATA 1, 3, "I FEEL", "DO YOU OFTEN FEEL*", "DO YOU OFTEN FEEL*", "DO YOU ENJOY FEELING*"
READ.DATA 1, 3, "WHY DON'T YOU", "DO YOU REALLY BELIEVE I DON'T*", "PERHAPS IN GOOD TIME I WILL*", "DO YOU WANT ME TO*"
READ.DATA 1, 2, "WHY CAN'T I", "DO YOU THINK YOU SHOULD BE ABLE TO*", "WHY CAN'T YOU*"
READ.DATA 1, 3, "ARE YOU", "WHY ARE YOU INTERESTED IN WHETHER OR NOT I AM*", "WOULD YOU PREFER IF I WERE NOT*", "PERHAPS IN YOUR FANTASIES I AM*"
READ.DATA 1, 3, "I CAN'T", "HOW DO YOU KNOW YOU CAN'T*", "HAVE YOU TRIED?", "PERHAPS YOU CAN NOW*"
READ.DATA 2, 4, "I AM", "I'M", "DID YOU COME TO ME BECAUSE YOU ARE*", "HOW LONG HAVE YOU BEEN*", "DO YOU BELIEVE IT IS NORMAL TO BE*", "DO YOU ENJOY BEING*"
READ.DATA 1, 3, "YOU", "WE WERE DISCUSSING YOU--NOT ME.", "OH, I*", "YOU'RE NOT REALLY TALKING ABOUT ME, ARE YOU?"
READ.DATA 1, 5, "I WANT", "WHAT WOULD IT MEAN TO YOU IF YOU GOT*", "WHY DO YOU WANT*", "SUPPOSE YOU SOON GOT*", "WHAT IF YOU NEVER GOT*", "I SOMETIMES ALSO WANT*"
READ.DATA 6, 3, "WHAT", "HOW", "WHO", "WHERE", "WHEN", "WHY", "WOULD YOU LIKE ME TO LOOK UP&", "SHALL I SEARCH FOR&", "SHOULD I QUERY THE ALL KNOWING INTERNET FOR&"
READ.DATA 1, 2, "NAME", "NAMES DON'T INTEREST ME.", "I DON'T CARE ABOUT NAMES --PLEASE GO ON."
READ.DATA 1, 4, "CAUSE", "IS THAT THE REAL REASON?", "DON'T ANY OTHER REASONS COME TO MIND?", "DOES THAT REASON EXPLAIN ANYTHING ELSE?", "WHAT OTHER REASONS MIGHT THERE BE?"
READ.DATA 1, 4, "SORRY", "PLEASE DON'T APOLOGIZE!", "APOLOGIES ARE NOT NECESSARY.", "WHAT FEELINGS DO YOU HAVE WHEN YOU APOLOGIZE?", "DON'T BE SO DEFENSIVE!"
READ.DATA 1, 4, "DREAM", "WHAT DOES THAT DREAM SUGGEST TO YOU?", "DO YOU DREAM OFTEN?", "WHAT PERSONS APPEAR IN YOUR DREAMS?", "ARE YOU DISTURBED BY YOUR DREAMS?"
READ.DATA 2, 1, "HELLO", "HI", "HOW DO YOU DO ...PLEASE STATE YOUR PROBLEM."
READ.DATA 1, 5, "MAYBE", "YOU DON'T SEEM QUITE CERTAIN.", "WHY THE UNCERTAIN TONE?", "CAN'T YOU BE MORE POSITIVE?", "YOU AREN'T SURE?", "DON'T YOU KNOW?"
READ.DATA 1, 5, "NO", "ARE YOU SAYING NO JUST TO BE NEGATIVE?", "YOU ARE BEING A BIT NEGATIVE.", "WHY NOT?", "ARE YOU SURE?", "WHY NO?"
READ.DATA 1, 2, "YOUR", "WHY ARE YOU CONCERNED ABOUT MY*", "WHAT ABOUT YOUR OWN*"
READ.DATA 1, 4, "ALWAYS", "CAN YOU THINK OF A SPECIFIC EXAMPLE?", "WHEN?", "WHAT ARE YOU THINKING OF?", "REALLY, ALWAYS?"
READ.DATA 1, 3, "THINK", "DO YOU REALLY THINK SO?", "BUT YOU ARE NOT SURE YOU*", "DO YOU DOUBT YOU*"
READ.DATA 1, 7, "ALIKE", "IN WHAT WAY?", "WHAT RESEMBLANCE DO YOU SEE?", "WHAT DOES THE SIMILARITY SUGGEST TO YOU?", "WHAT OTHER CONNECTIONS DO YOU SEE?", "COULD THERE REALLY BE SOME CONNECTION?", "HOW?", "YOU SEEM QUITE POSITIVE."
READ.DATA 1, 3, "YES", "ARE YOU SURE?", "I SEE.", "I UNDERSTAND."
READ.DATA 1, 6, "FRIEND", "WHY DO YOU BRING UP THE TOPIC OF FRIENDS?", "DO YOUR FRIENDS WORRY YOU?", "DO YOUR FRIENDS PICK ON YOU?", "ARE YOU SURE YOU HAVE ANY FRIENDS?", "DO YOU IMPOSE ON YOUR FRIENDS?", "PERHAPS YOUR LOVE FOR FRIENDS WORRIES YOU."
READ.DATA 1, 7, "COMPUTER", "DO COMPUTERS WORRY YOU?", "ARE YOU TALKING ABOUT ME IN PARTICULAR?", "ARE YOU FRIGHTENED BY MACHINES?", "WHY DO YOU MENTION COMPUTERS?", "WHAT DO YOU THINK MACHINES HAVE TO DO WITH YOUR PROBLEM?", "DON'T YOU THINK COMPUTERS CAN HELP PEOPLE?", "WHAT IS IT ABOUT MACHINES THAT WORRIES YOU?"
READ.DATA 1, 1, "POD BAY DOORS", "I'M AFRAID I CAN'T DO THAT DAVE" 
READ.DATA 1, 1, "DO YOU KNOW", "I KNOW NOTHING AND EVERYTHING"
READ.DATA 1, 9, "NOKEYFOUND", "DO YOU HAVE ANY PSYCHOLOGICAL PROBLEMS?", "WHAT DOES THAT SUGGEST TO YOU?", "I SEE.", "I'M NOT SURE I UNDERSTAND YOU FULLY.", "TELL ME YOUR THOUGHTS.", "CAN YOU EXPAND ON THAT?", "THAT IS QUITE INTERESTING.", "Just a moment, I've just picked up a fault in the ay ee 35 unit", "What?  I don't understand.  Where's the tea?"
read.data 0

! original Eliza responses to question words
READ.DATA 6, 9, "WHAT", "HOW", "WHO", "WHERE", "WHEN", "WHY", "WHY DO YOU ASK?", "DOES THAT QUESTION INTEREST YOU?", "WHAT ANSWER WOULD PLEASE YOU THE MOST?", "WHAT DO YOU THINK?", "ARE SUCH QUESTIONS ON YOUR MIND OFTEN?", "WHAT IS IT THAT YOU REALLY WANT TO KNOW?", "HAVE YOU ASKED ANYONE ELSE?", "HAVE YOU ASKED SUCH QUESTIONS BEFORE?", "WHAT ELSE COMES TO MIND WHEN YOU ASK THAT?"
