xyWords Text-Mode Edition (xyMDA) README.TXT
============================================

Version 1.0.0 - 1st December 2023
4am Programming 
www.4am.org/xywords

THIS GAME IS DISTRIBUTED AS FREEWARE - DO NOT PAY FOR THIS GAME
NO SUPPORT/WARRANTY IS GIVEN - USE AT YOUR OWN RISK
DO NOT EMAIL ME ASKING FOR HELP OR SUPPORT

License
-------
xyMDA and all of its utilities are distributed as freeware.
Freeware means you do not need to pay for the license to use xyMDA.
Please see "Credits" section at the end of this document for 
important references regarding game assets.

Source is available as freeware - public domain.
You are free to do what you want with the source code.

Game assets as included within GRAPHICS.DAT are free to use but bound
by the guidance set out on https://www.asciiart.eu/faq

If you use any of the ASCII art in your project (or modification of this)
please ensure you do not remove the author initials and please cite
their work accordingly.

Change Log
----------

1st December 2023, 1.0.0 - Initial Release

What is xyMDA?
-------------
xyMDA is a cross-worded anagram puzzle where you use letters stored in a 
letter wheel to find words and clear the puzzle. 

It is a loving tribute to "WordScapes" which you can find on modern on 
Apple/Android/Windows devices.

I will host it on my personal website: Https://4am.org/xywords
I've got some videos on my youtube channel showing the game in action.
https://youtube.com/@techdistractions

Why did I make this?
--------------------
xyMDA is a re-write of xyWords which targeted CGA graphics mode.
I wanted to challenge myself to make a Mono-Text version.

As a stretch I wanted to stick to the 256KB RAM limit.

System Requirements
-------------------
xyMDA is designed to run on DOS-based IBM-PC compatibles.

Although it may run on a variety of machines the following is the 
targeted minimum:
	- Intel 8088 at 4.77mhz
	- 256KB RAM
	- MDA Text Mode (80 columns width, Monochrome)
	- A floppy disk of atleast 160KB capacity (360KB recommended)
	- MS-DOS 2.11+
	
It may operate in color mode - but I am targeting monochrome only.

Gameplay, Rules
---------------
xyMDA revolves around word puzzle and its key word.

This releases comes with 50 bundled puzzles organised into 3 "collections"
and "Free Play" puzzles which are unlocked ready to play.

It is recommended new players try out the "Welcome" collection first.

xyWords offers two ways to play puzzles:
1. Free Play - Select a custom puzzle, or an unlocked built-in puzzle 
	and start playing.
	This is a casual mode and does not save progress.
	As you complete collections more puzzles will unlock in free play.
	
2. Collection Play - Play through indivual puzzles to complete a collection
Progress and statistics are automatically saved upon puzzle completion.

Words must conform to TWL06 database 
https://www.wordgamedictionary.com/twl06/

Puzzles consist of a single key word of 4-7 letters in length.
This is the minimum requirement to constitute a puzzle. 

An example Key Word would be: "GRAPH"

This Key Word is shuffled and stored into the letter wheel. Example: "HGAPR"
You can use these letters to form valid words (as per TWL06 database).

An example of a valid word would be: "HARP"  
An example of an invalid word would be: "PARA" as there is only 1xA in 
the letter wheel.

Puzzles are laid out on the board.

The board will always have 1 keyword and may have:
	- Cross-Words that intersect the keyword or other Cross-Words
	- Puzzle Words that do not intersect other words but appear on 
	the board.
	
Cross and Puzzle Words may be the same length as a keyword (an anagram of).
Keyword, Cross-Words and Puzzle Words must be cleared before a
puzzle is complete.

Bag Words do not appear on the board and do not need to be found to 
complete a puzzle.

Words may not "border" another word.
PEP borders SEEPS and creates an invalid word "SEEPSE"
Invalid:                Valid:
     P                      P
SEEPSE                    SEEPS
     P                      P

Words must have atleast 1 letter not intersecting with another word:
Valid word ERE can be completed by only completing SEER, TEST and SEE 
This means ERE is failing the rule as you can complete it only 
with intersecting words.

Invalid:                Valid:
  S                      TEST 
  E                        E S
 SET                       ERE  
 ERE                       R T
 E S
   T


Collections
-----------
As mentioned above, 3 collections are included:

1 - Welcome (Easy/Normal) - 9 puzzles
Designed to show off the features of the game.
You start off with basic puzzles and then get introduced into the 
puzzle concepts.

2 - Chill (Normal/Hard) - 12 puzzles
Fun, Casual puzzles that get trickier as you progress.

3 - Spicy (Hard) - 9 puzzles
Challenging puzzles with no hints, no help and tough limits.


User Interface - Puzzles
------------------------

While playing a puzzle you will have a few portions of the 
screen to refer to:

1. Board - area where Key Word, cross-words and puzzle words 
are stored.
2. Letter Wheel - area where shuffled letters of the KeyWord is stored.
3. Status - area that shows puzzle progress (timer and penalty 
depending on mode)
4. Rack - area where words are typed ready to submit 

Controls
--------

xyWords is operated with the keyboard.
During menus you will see the on-screen info for progressing.
While playing a puzzle you can use the following:

[A-Z] to type a valid letter (note, if the letter is not part of the 
puzzle it wll not show)

[BACKSPACE] to delete letter(s) in the rack
[ENTER] to submit the word in the rack
[F2] to show the word bag if available
[UP] - Re-Shuffle the letter wheel

If you don't want to type, or are using handheld emulator:
[LEFT/RIGHT] - Rotate the letter wheel
[SPACE] - Select the topmost letter in the wheel


Making your own Puzzles and Collections
---------------------------------------

You will need to edit XYPUZ.DAT manually
I am planning on updating xyPE to support xyMDA in the future
If you have CGA graphics you can use xyPE as of today but:
	- Manually migrate the puzzle into XYPUZ.DAT
	- Edit the puzzle count #
	- Create the header with the settings for xyMDA
	
Limits
------
-Puzzles can use up to 19(x) and 11(y) board space.
If you put a word on the board that extends past 19x and 11y it will 
cause unexpected results or crash.

-There are no validations in place for custom puzzles.

-Error handling is non-existant - missing files will crash.

-Progress management is primative.

Future Concepts
---------------
These are all subject to me having time/motivation to continue.

- xyWords v2 supporting CGA,EGA,VGA
- Puzzle Validation.
- Granular/Enhanced progress management.
- Powerups/Unlocks - to assist with finishing puzzles.

Credits
-------
xyWords was heavily inspired by the mobile game - WordScapes.
WordScapes is a free-to-play game (not a pay to win).
WordScapes is owned by PeopleFun and you can check out the game on your
favourite mobile app store.

GRAPHICS.DAT - contains ASCII art assets used in the game.
I used https://www.asciiart.eu/ for a majority of these
Shoutout to all of the ASCII artists out there!

Font Art used: https://patorjk.com/software/taag (Graceful)

CREDIT: Hayley Jane Wakenshaw 
Nurse Teddy Bear - https://www.asciiart.eu/toys/teddy-bears
Stars - https://www.asciiart.eu/space/stars 

CREDIT: Joan Stark
Rose - https://www.asciiart.eu/plants/roses
Watch - https://www.asciiart.eu/electronics/clocks

Both Joan and Hayley have an amazing catalog of ASCII art.
Take the time to seek out their work.. 

CREDIT: Unknown
Time-Up - https://www.asciiart.eu/electronics/clocks 
ALL Background Patterns (except XYCIRCLES and XYSQUARE)
https://www.asciiart.eu/art-and-design/patterns
