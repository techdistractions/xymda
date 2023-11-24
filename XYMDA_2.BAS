DECLARE SUB ViewHelp ()
DECLARE SUB KillPlayer ()
DECLARE SUB SavePlayer ()
DECLARE FUNCTION GetFirstSplit$ (inTxt$, SplitBy$)
DECLARE SUB ViewStats ()
DECLARE FUNCTION BigTime$ (TotalSeconds&)
DECLARE FUNCTION DateString$ (inText$, inStyle%)
DECLARE FUNCTION GetTimeStamp$ ()
DECLARE SUB PuzRecord ()
DECLARE SUB HelpUI (inVal%)
DECLARE FUNCTION SubmitTry% ()
DECLARE SUB LetterWheel ()
DECLARE FUNCTION TimeString$ (inSec%)
DECLARE SUB UpdateSUI (Section%, uVal%, uStr$)
DECLARE SUB LoadArt (Header$)
DECLARE FUNCTION IntUP% (inVal%, inDiv%)
DECLARE FUNCTION GetDifficulty$ (inVal%)
DECLARE FUNCTION GetUnlock$ (inX%)
DECLARE SUB GameSelect (inPass%)
DECLARE SUB DrawProgressBar (inA%, inB%, PlotX%, PlotY%, PStart$, PEnd$, PFull$, PPart$)
DECLARE SUB ExitGame ()
DECLARE FUNCTION GetKey$ ()
DECLARE SUB DrawArt (StartX%, StartY%, OverWriteFlag%)
DECLARE FUNCTION FillString$ (useLen%, inText$)
DECLARE FUNCTION BrowsePlayer% ()
DECLARE SUB SelectPlayer (UseID%)
DECLARE SUB PlayerInit (FileHandle!)
DECLARE SUB LoadPuzzle ()
DECLARE SUB DecorateTable (inPat$)
DECLARE SUB LoadColl (useCol%)
DECLARE SUB ShowPuzzle ()
DECLARE FUNCTION GetMem$ ()
DECLARE SUB DatInit (FileHandle!, Progress!)
DECLARE SUB GetScreen (AreaStartX%, AreaStartY%, AreaEndX%, AreaEndY%)
DECLARE SUB ClearLine (StartX%, StartY%, EndX%, EndY%)
DECLARE FUNCTION JumbleString$ (inTxt$)
DECLARE SUB DrawLine (StartX%, StartY%, EndX%, EndY%, LineChar%)
DECLARE SUB DrawBox (StartX%, StartY%, EndX%, EndY%, BoxStyle%)
DECLARE FUNCTION StrSplit$ (uVal%, uSplitBy$, uText$)
DECLARE SUB UIBorderDraw (StartC%)
DECLARE SUB UpdateBorder (WordStr$, WordType%, WordOrient%, WordX%, WordY%)
DECLARE FUNCTION BorderConnect! (YPos%, XPos%, SegFlag%)
DECLARE SUB ShowWord (WordStr$, WordType%, WordOrient%, WordX%, WordY%, MaskFlag%)
DECLARE SUB xyBannerDraw (uRow%, uCol%)
DECLARE SUB ScreenBorderDraw ()

'xyWords for Monochrome Display Adapter [MDA]
'Targets: 8088 @ 4.77mhz, 256KB RAM, 160KB FDD, MDA Text-Mode, PC-Speaker, DOS 2.11+

'Variables (Non-Array)
COMMON SHARED LastErr%, FPCount%, CollCount%, PuzCount%, FocusPuzCount%, CollSelect%, PuzSelect%, FocusCollCount%, pMDASettings$, FocusPlayer%
COMMON SHARED SUICol%, PuzTimeTotal%, PuzTimeCount%, PuzInvTotal%, PuzInvCount%, PuzBagWordsTotal%, PuzBagWordsCount%, PuzBoardWordsTotal%
COMMON SHARED PuzKeyWordTime%, PuzUWordCount%, PuzCWordCount%, PuzKeyWord$, PuzBuffer$, PuzWheel$, EnableSound%, PuzBoardWordsCount%, CWCol%
COMMON SHARED FocusCollProg$, FocusCollPuz%, SkipFlag%, HelpUIView%, HelpUIPage%, HelpUIPages%, CX%, StartCX%, EndCX%, PuzRecordChange$, inText$

'Arrays
COMMON SHARED SplitWords$(), TextHold$(), FPList$(), CollHead$(), CollData$(), FocusColl$(), PuzHead$(), PuzData$(), FocusPuz$(), FocusPuzWL$(), pMDA%()
COMMON SHARED PlayerHead$(), FocusPlayerProg$(), FocusPlayerStat$(), FPMap%(), Art$(), FocusPuzF%()
COMMON SHARED DoType%(), DoOrient%(), UseX%(), UseY%(), DoCommon%(), PlayList$(), UIList$(), PlayerCulmStat&(), SPos!(), Report$()
'Static

'$DYNAMIC
DIM SHARED SplitWords$(128), TextHold$(6), FPList$(128), CollHead$(128), CollData$(128), FocusColl$(128), PuzHead$(256), PuzData$(128), FocusPuz$(64), FocusPuzWL$(32), pMDA%(33)
DIM SHARED PlayerHead$(64), FocusPlayerProg$(64), FocusPlayerStat$(256), FPMap%(10), Art$(10), FocusPuzF%(32)
DIM SHARED DoType%(32), DoOrient%(32), UseX%(32), UseY%(32), DoCommon%(32), PlayList$(256), UIList$(64), PlayerCulmStat&(16), SPos!(16), Report$(32)

REM $STATIC
FUNCTION BigTime$ (TotalSeconds&)
'Converts seconds into a #d #h #m #s formatted string
Days& = TotalSeconds& \ 86400: Hours& = (TotalSeconds& MOD 86400) \ 3600
Minutes& = ((TotalSeconds& MOD 86400) MOD 3600) \ 60: seconds& = TotalSeconds& MOD 60

OutString$ = "": IF Days& > 0 THEN OutString$ = LTRIM$(RTRIM$(STR$(Days&))) + "d "
IF Hours& = 0 AND Days& > 0 THEN OutString$ = OutString$ + "0h "
IF Hours& > 0 THEN OutString$ = OutString$ + LTRIM$(RTRIM$(STR$(Hours&))) + "h "
OutString$ = OutString$ + LTRIM$(RTRIM$(STR$(Minutes&))) + "m " + LTRIM$(RTRIM$(STR$(seconds&))) + "s"
IF LEFT$(OutString$, 2) = "0m" THEN OutString$ = MID$(OutString$, 4)
BigTime$ = RTRIM$(OutString$) 'Trim if just seconds and return
END FUNCTION

FUNCTION DateString$ (inText$, inStyle%)
'Convert DDMMYYYY to different formats
SELECT CASE MID$(inText$, 3, 2) 'Format date string
        CASE "01": uDate$ = "Jan"
        CASE "02": uDate$ = "Feb"
        CASE "03": uDate$ = "Mar"
        CASE "04": uDate$ = "Apr"
        CASE "05": uDate$ = "May"
        CASE "06": uDate$ = "Jun"
        CASE "07": uDate$ = "Jul"
        CASE "08": uDate$ = "Aug"
        CASE "09": uDate$ = "Sep"
        CASE "10": uDate$ = "Oct"
        CASE "11": uDate$ = "Nov"
        CASE "12": uDate$ = "Dec"
END SELECT
V% = VAL(LEFT$(inText$, 2))
SELECT CASE inStyle%
        CASE 1: DateString$ = LEFT$(inText$, 2) + "/" + MID$(inText$, 3, 2) + "/" + RIGHT$(inText$, 4)
        CASE 2: DateString$ = LTRIM$(RTRIM$(STR$(V%))) + " " + uDate$ + " " + RIGHT$(inText$, 4)
END SELECT
END FUNCTION

SUB DrawProgressBar (inA%, inB%, PlotX%, PlotY%, PStart$, PEnd$, PFull$, PPart$)
'Obtain total percentage by comparing inA% to target inB%
P% = INT(((inA% / inB%) * 100)): Temp$ = "" 'blank

SELECT CASE P%
        CASE IS > 99: Temp$ = PFull$ + PFull$ + PFull$ + PFull$ + PFull$'5  -100%
        CASE IS > 89: Temp$ = PFull$ + PFull$ + PFull$ + PFull$ + PPart$  '4.5 - 90%
        CASE IS > 79: Temp$ = PFull$ + PFull$ + PFull$ + PFull$ + " "  '4 - 80%
        CASE IS > 69: Temp$ = PFull$ + PFull$ + PFull$ + PPart$ + " "  '3.5 - 70%
        CASE IS > 59: Temp$ = PFull$ + PFull$ + PFull$ + STRING$(2, " ")  '3 - 60%
        CASE IS > 49: Temp$ = PFull$ + PFull$ + PPart$ + STRING$(2, " ")  '2.5 - 50%
        CASE IS > 39: Temp$ = PFull$ + PFull$ + STRING$(3, " ") '2 - 40%
        CASE IS > 29: Temp$ = PFull$ + PPart$ + STRING$(3, " ")  '1.5 - 30%
        CASE IS > 19: Temp$ = PFull$ + STRING$(4, " ")  '1 - 20%
        CASE IS > 9: Temp$ = PPart$ + STRING$(4, " ")   '.5 - 10%
END SELECT

LOCATE PlotY%, PlotX%: PRINT PStart$; Temp$; : LOCATE PlotY%, PlotX% + 7: PRINT PEnd$;
END SUB

FUNCTION GetFirstSplit$ (inTxt$, SplitBy$)
'Returns the first value in a split string
i% = INSTR(inTxt$, SplitBy$) - 1
IF i% > 1 THEN : uTxt$ = LEFT$(inTxt$, i%): GetFirstSplit$ = uTxt$
END FUNCTION

SUB InitGameDat
'Load settings from GAME.DAT eventually

SUICol% = 48 'Status UI Column
EnableSound% = 1 'Enable PC Speaker Sound
SkipFlag% = 1 'Dont redraw banners

'Use Down for Backspace (controller use)

'Enable trace output

'Predefine color mappings and use variables instead (future)

'Sound PlayList$() 'Future: Use GAME.DAT

PlayList$(1) = "T160L16EDEDBDC A" 'Found KeyWord
PlayList$(2) = "L32D32D32E32" 'OWord
PlayList$(3) = "L32D32D32E32" 'PWord
PlayList$(4) = "T160O1L20GAAA" 'XWord
PlayList$(5) = "L32MLGN1MLML" 'Not Found/Invalid
PlayList$(6) = "L32MLGN1MLML" 'Duplicate
PlayList$(7) = "T180 O4 L20 ABA>ABA<ABA>ABA" '<20 Sec remain
PlayList$(8) = "T160O1L8CDEDCDL4ECC" 'Puzzle Clear
PlayList$(9) = "T180L8O4EF#G#ABAG#F#ED#BDC#DEBDC#DEF" 'Collection Complete
PlayList$(10) = "T240 L8 O5 EDEDBDC" 'Sparkle sound for setting a record
PlayList$(11) = "T160O1L24CDA" 'Sound Enabled
END SUB

SUB KillPlayer

'Updates PLAYER.DAT and removes Current Player
F = FREEFILE: OPEN "PLAYER.TMP" FOR OUTPUT AS #F
G = FREEFILE: OPEN "PLAYER.DAT" FOR INPUT AS #G
Flag% = 1 'Default to write
DO
        LINE INPUT #G, inText$: inText$ = LTRIM$(RTRIM$(inText$))
        IF LEN(inText$) > 2 THEN
                SELECT CASE LEFT$(inText$, 2)
                CASE "[P" 'Look for the P#
                Temp$ = StrSplit$(1, "|", PlayerHead$(FocusPlayer%))
                IF StrSplit$(1, "|", inText$) = Temp$ THEN Flag% = 0'Don't write, current player
                CASE "[/"
                        Temp$ = "[/" + StrSplit$(1, "|", MID$(PlayerHead$(FocusPlayer%), 2)) + "]"
                        IF Temp$ = inText$ THEN : Flag% = 1: GOTO KP.EOFCheck  'Done
                END SELECT
        END IF

KP.FlagCheck:
        IF Flag% = 1 THEN PRINT #F, inText$ 'Just write it

KP.EOFCheck:
        IF EOF(G) THEN EXIT DO
LOOP
CLOSE #F: CLOSE #G
KILL "PLAYER.DAT"
NAME "PLAYER.TMP" AS "PLAYER.DAT"

'Clean up PlayerHead$(0)
V% = VAL(PlayerHead$(0))
IF FocusPlayer% = V% THEN 'trim
        V% = V% - 1: PlayerHead$(0) = LTRIM$(RTRIM$(STR$(V%)))
ELSE 're-order then trim
        FOR X% = FocusPlayer% TO V%: PlayerHead$(X%) = PlayerHead$(X% + 1): NEXT X%
        V% = V% - 1: PlayerHead$(0) = LTRIM$(RTRIM$(STR$(V%)))
END IF

END SUB

SUB MMenu
        ClearLine 10, 16, 60, 19
        'Player Select
        'View Records & Statistics
        'Game Configuration
        'Help & Information
        
        SLEEP
        ClearLine 2, 16, 79, 24
END SUB

SUB PutScreen (AreaStartX%, AreaStartY%)
'Whatever is stored in TextHold$ will be output a line at a time
FOR Y% = AreaStartY% TO AreaEndY%
        LOCATE Y%, AreaStartX%: PRINT TextHold$(Y%);
NEXT Y%
ERASE TextHold$ 'release memory back

END SUB

SUB SavePlayer
'Updates PLAYER.DAT based on the Current Player
'Player Stats, Collection Progress and Puzzle Records
F = FREEFILE: OPEN "PLAYER.TMP" FOR OUTPUT AS #F
G = FREEFILE: OPEN "PLAYER.DAT" FOR INPUT AS #G
FoundFP% = 0
Flag% = 1 'Default to write
DO
        LINE INPUT #G, inText$: inText$ = LTRIM$(RTRIM$(inText$))
        IF LEN(inText$) > 2 THEN
                SELECT CASE LEFT$(inText$, 2)
                CASE "[P" 'Look for the P#
                Temp$ = StrSplit$(1, "|", PlayerHead$(FocusPlayer%))
                IF StrSplit$(1, "|", inText$) = Temp$ THEN
                        Flag% = 2'Don't write, current player
                        END IF
                CASE "[/"
                        Temp$ = "[/" + StrSplit$(1, "|", MID$(PlayerHead$(FocusPlayer%), 2)) + "]"
                        IF Temp$ = inText$ THEN : Flag% = 1: GOTO SP.EOFCheck  'Done
                END SELECT
        END IF

SP.FlagCheck:

        SELECT CASE Flag%
        CASE 1: PRINT #F, inText$ 'Just write it

        CASE 2 'Found the head of FocusPlayer% - Write this
                V% = VAL(FocusPlayerProg$(0))
                PRINT #F, PlayerHead$(FocusPlayer%): PRINT #F, "{COLLECTIONS}"
                FOR X% = 1 TO V%: IF FocusPlayerProg$(X%) <> "" THEN PRINT #F, FocusPlayerProg$(X%)
                NEXT X%
                PRINT #F, "{/COLLECTIONS}": PRINT #F, "{PUZZLES}"
                V% = VAL(FocusPlayerStat$(0))
                FOR X% = 1 TO V%
                        IF FocusPlayerStat$(X%) <> "" THEN PRINT #F, FocusPlayerStat$(X%)
                NEXT X%
                PRINT #F, "{/PUZZLES}": PRINT #F, "[/"; StrSplit$(1, "|", MID$(PlayerHead$(FocusPlayer%), 2)); "]"
                FoundFP% = 1: Flag% = 0  'Reset
        END SELECT

        

SP.EOFCheck:
        IF EOF(G) THEN
                SELECT CASE FoundFP%
                        CASE 0: Flag% = 2: GOTO SP.FlagCheck
                        CASE 1: EXIT DO
                END SELECT
        END IF

LOOP
CLOSE #F: CLOSE #G
KILL "PLAYER.DAT"
NAME "PLAYER.TMP" AS "PLAYER.DAT"
EXIT SUB


END SUB

SUB SyncPlayerStat
'Synchronise Culmulative stats to the FocusPlayer
Temp$ = StrSplit$(1, "|", PlayerHead$(FocusPlayer%))
FOR X% = 1 TO 4: SplitWords$(X% + 4) = LTRIM$(RTRIM$(STR$(PlayerCulmStat&(X%)))): NEXT X%
SplitWords$(4) = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
PlayerHead$(FocusPlayer%) = "" 'Rewrite focusplayer
FOR X% = 1 TO 7: PlayerHead$(FocusPlayer%) = PlayerHead$(FocusPlayer%) + SplitWords$(X%) + "|": NEXT X%
PlayerHead$(FocusPlayer%) = PlayerHead$(FocusPlayer%) + SplitWords$(8) + "]"
END SUB

FUNCTION TimeString$ (inSec%)
'Converts seconds into 0:00
        tSec% = inSec% MOD 60: tMin% = INT(inSec% / 60): uSec$ = MID$(STR$(tSec%), 2): IF LEN(uSec$) = 1 THEN uSec$ = "0" + uSec$'add leading 0
        TimeString$ = MID$(STR$(tMin%), 2) + ":" + uSec$
END FUNCTION

SUB ViewHelp
UseFile$ = "QHELP.TXT" 'Quick help

VH.Top:
ClearLine 2, 2, 79, 24
G = FREEFILE: SPosLines! = 1: WideX% = 1: REDIM SPos!(1024)

OPEN UseFile$ FOR INPUT AS #G
DO
        SPos!(SPosLines!) = SEEK(G)
        LINE INPUT #G, Temp$: SPosLines! = SPosLines! + 1
        L% = LEN(Temp$): IF L% > WideX% THEN WideX% = L%
        IF EOF(G) THEN : SEEK #G, 1: EXIT DO
LOOP

StartX% = 4: EndX% = 76: StartY% = 4: EndY% = 23: DisableX% = 0
WideX% = WideX% + 1: FCursorX% = 1: FCursorY% = 1'Starting Cursor position (and fix last char not showing)

WideX% = 80

'Arrows
COLOR 0, 7: LOCATE 2, 2: PRINT STRING$(78, " ");
SELECT CASE UseFile$
CASE "QHELP.TXT": LOCATE 2, 2: PRINT "[Quick Help]   [F2] Full README";
CASE "README.TXT": LOCATE 2, 2: PRINT "[F1] Quick Help   [README]";
END SELECT
LOCATE 3, 2: PRINT STRING$(78, " ");
LOCATE 4, 79:  PRINT CHR$(24);
LOCATE 23, 79: PRINT CHR$(25); : LOCATE 24, 2: PRINT CHR$(27);
LOCATE 24, 79: PRINT CHR$(26); : : COLOR 7, 0
COLOR 0, 7: LOCATE 3, 58: PRINT "PGUP/DN Change Page";
LOCATE 2, 58: PRINT CHR$(24); CHR$(25); CHR$(27); CHR$(26); " Scroll - ESC Quit"; : COLOR 7, 0


VH.RefreshVis:
IF FCursorX% < 1 THEN FCursorX% = 1
FOR Y% = 5 TO 22: LOCATE Y%, 79: PRINT "ฒ"; : NEXT Y%
IF FCursorY% < 1 THEN FCursorY% = 1
PCT% = INT((FCursorY% / SPosLines!) * 18)
LOCATE 5 + PCT%, 79: PRINT "ฐ";
PCT% = ((FCursorX% + 72) / WideX%) * 75
IF PCT% < 1 THEN PCT% = 1
IF PCT% > 75 THEN PCT% = 75

LOCATE 24, 3: PRINT STRING$(76, "ฒ"); : LOCATE 24, 3 + PCT%: PRINT "ฐ";

IF SPos!(FCursorY%) > 0 THEN
        SEEK #G, SPos!(FCursorY%): REDIM Report$(73 * 20): CY% = 0
        ClearLine StartX%, StartY%, EndX%, EndY%
        FOR YY% = FCursorY% TO FCursorY% + 19
                LINE INPUT #G, Report$(YY%): MaxX% = EndX% - StartX%
                IF LEN(Report$(YY%)) < MaxX% THEN MaxX% = LEN(Report$(YY%))
                uTemp$ = MID$(Report$(YY%), FCursorX%)
                IF LEN(uTemp$) > EndX% - StartX% THEN uTemp$ = LEFT$(uTemp$, EndX% - StartX%)
                LOCATE StartY% + CY%, StartX%: PRINT uTemp$;
                CY% = CY% + 1
                IF EOF(G) THEN EXIT FOR
        NEXT YY%
END IF

VH.ViewGetKey:
SELECT CASE GetKey$
        CASE CHR$(0) + CHR$(59) 'F1 for QHELP
                IF UseFile$ <> "QHELP.TXT" THEN : UseFile$ = "QHELP.TXT": GOTO VH.Top
        CASE CHR$(0) + CHR$(60) 'F2 for README
                IF UseFile$ <> "README.TXT" THEN : UseFile$ = "README.TXT": GOTO VH.Top
        CASE "1": FCursorY% = 54: GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(71): FCursorX% = 1: GOTO VH.RefreshVis
        CASE CHR$(27): GOTO VH.ViewExit
        CASE CHR$(0) + CHR$(72) 'Up
                FCursorY% = FCursorY% - 1
                IF FCursorY% < 1 THEN FCursorY% = 1
                GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(80) 'Down
                IF FCursorY% < 1 THEN FCursorY% = 1
                FCursorY% = FCursorY% + 1
                IF FCursorY% > SPosLines! THEN FCursorY% = SPosLines! - 20
                GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(73) 'PgUp
                FCursorY% = FCursorY% - 19
                IF FCursorY% < 1 THEN FCursorY% = 1
                GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(81) 'PgDn
                FCursorY% = FCursorY% + 19
                IF FCursorY% > SPosLines! THEN FCursorY% = SPosLines! - 20
                GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(71): FCursorX% = 1: GOTO VH.RefreshVis'Home
        CASE CHR$(0) + CHR$(79): FCursorX% = WideX% - (EndX% - StartX%): GOTO VH.RefreshVis 'End
        CASE CHR$(0) + CHR$(115) 'CTRL+LEFT
        FCursorX% = FCursorX% - 10: IF FCursorX% < 1 THEN FCursor% = 1
        GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(116) 'CTRL+RIGHT
        FCursorX% = FCursorX% + 10
        IF FCursorX% < WideX% - (EndX% - StartX%) THEN
                GOTO VH.RefreshVis
                ELSE
                FCursorX% = WideX% - (EndX% - StartX%)
                GOTO VH.RefreshVis
        END IF
       
        CASE CHR$(0) + CHR$(75) 'Left
        IF FCursorX% > 1 THEN FCursorX% = FCursorX% - 1: GOTO VH.RefreshVis
        CASE CHR$(0) + CHR$(77) 'Right
        IF FCursorX% < WideX% - (EndX% - StartX%) THEN
                FCursorX% = FCursorX% + 1: GOTO VH.RefreshVis
                ELSE
                FCursorX% = WideX% - (EndX% - StartX%)
                GOTO VH.RefreshVis 'End
        END IF
END SELECT
GOTO VH.ViewGetKey

VH.ViewExit:
ERASE Report$
ERASE SPos!
CLOSE #G: COLOR 7, 0

END SUB

SUB ViewStats
'Generates Stats into StatsArray&, compiles XYSTATS.TXT and then views it.
'Memory Usage is a concern here as PLAYER.DAT gets incremently larger
'TODO: The compile report function could use some optimisation

'StatsArray& is a multi-dimension array based on fields and players
'(1) Unique Free Play Puzzles Complete [Total = FPCount%] - Manual count of players {PUZZLES} records for FPList$() puzzles
'(2) Cumulative Time Played (seconds&) - Taken from PlayerHead$()
'(3) Cumulative Words Found - Taken from PlayerHead$()
'(4) Cumulative Times a Puzzle has been completed (including replays) - Taken from PlayerHead$()
'(5) Cumulative Errors made - Taken from PlayerHead$()
'(6) Unique Puzzles Complete - Count of {PUZZLES} for the player
'(7) Average Words Per Minute - Taken as an average from {PUZZLES} for the player
'(8) Average Key Time - Taken as an average from {PUZZLES} for the player
'(9) Average Bag Words - Taken as an average from {PUZZLES} for the player.
'TODO: omit puzzles that dont have bag words
'(9+1-CollCount%) - Grab Collection Progress [Total = CollData$()]
'(9+CollCount%+((1-PuzCount%)*6)+0) - Grab Puzzle Records for all players - Times Completed
'(9+CollCount%+((1-PuzCount%)*6)+1) - Grab Puzzle Records for all players - Quickest Completion Time
'(9+CollCount%+((1-PuzCount%)*6)+2) - Grab Puzzle Records for all players - Fastest WPM
'(9+CollCount%+((1-PuzCount%)*6)+3) - Grab Puzzle Records for all players - Keyword Time [1st time]
'(9+CollCount%+((1-PuzCount%)*6)+4) - Grab Puzzle Records for all players - Least Errors
'(9+CollCount%+((1-PuzCount%)*6)+5) - Grab Puzzle Records for all players - Most Bag Words

'Setup UI, Toolbar & Status Indicator
ClearLine 2, 2, 78, 24
COLOR 0, 7: LOCATE 2, 3: PRINT STRING$(76, " "); : LOCATE 2, 3: PRINT "xyWords Records & Statistics";
COLOR 0, 7: inText$ = "Load File": LOCATE 2, 40: PRINT STRING$(38, " "); : LOCATE 2, 77 - LEN(inText$): PRINT inText$; : COLOR 7, 0

'Generate stats into StatsArray&
F = FREEFILE: OPEN "PLAYER.DAT" FOR INPUT AS #F
pCount% = VAL(PlayerHead$(0)) 'Get Player Count
REDIM fLine$(32), fSeek&(16)
'UArray% is set to the static 9 stats fields then a variable CollCount% amount
'Then we make enough space for all of the puzzles with up to 6 related stats each
UArray% = 9 + CollCount% + (PuzCount% * 6): U% = UArray% * pCount%
DIM StatsArray&(UArray%, pCount%) 'Set the Array grid
FOR X% = 1 TO pCount% 'Get the file seek locations of the player headers in PLAYER.DAT
        DO
                LastPos& = SEEK(F): LINE INPUT #F, inText$: inText$ = RTRIM$(LTRIM$(inText$))
                IF PlayerHead$(X%) = inText$ THEN
                        fSeek&(X%) = LastPos&  'Grab the position behind the header
                        Temp$ = StrSplit$(1, "|", MID$(PlayerHead$(X%), 2, LEN(PlayerHead$(X%)) - 2))    'Splitwords of Playehead: 5-Time,6-Word,7-Comp,8-Err
                        'While we are at it grab StatsArray&(2-5)
                        FOR Z% = 5 TO 8: StatsArray&(Z% - 3, X%) = VAL(SplitWords$(Z%)): NEXT Z% 'Store with offsets into StatsArray
                        EXIT DO
                END IF
        LOOP
NEXT X%
SEEK #F, 1

FOR X% = 1 TO pCount% 'Do this for each Player
COLOR 0, 7: inText$ = "Load Player " + LTRIM$(RTRIM$(STR$(X%))): LOCATE 2, 38: PRINT STRING$(38, " "); : LOCATE 2, 77 - LEN(inText$): PRINT inText$; : COLOR 7, 0:
EndPlayer$ = "[/" + StrSplit$(1, "|", MID$(PlayerHead$(X%), 2)) + "]"
Flag% = 0: SEEK #F, fSeek&(X%) 'Go to header of Player X%
DO
VS.NextRead:
LINE INPUT #F, inText$: inText$ = RTRIM$(LTRIM$(inText$)): LastPos& = SEEK(F)
        SELECT CASE inText$  'Set Flag for Parser
                CASE "{COLLECTIONS}": Flag% = 1: GOTO VS.NextRead
                CASE "{/COLLECTIONS}": Flag% = 0: GOTO VS.NextRead
                CASE "{PUZZLES}": Flag% = 2: GOTO VS.NextRead
                CASE "{/PUZZLES}": Flag% = 0: GOTO VS.NextRead
                CASE EndPlayer$: Flag% = 0: EXIT DO
        END SELECT
                
        SELECT CASE Flag%
                CASE 1 'Collection - find and store the value in StatsArray&
                        FOR A% = 1 TO CollCount%
                                TempA$ = GetFirstSplit$(MID$(CollHead$(A%), 2), "|")
                                TempB$ = GetFirstSplit$(inText$, "|")
                                IF TempA$ = TempB$ THEN
                                        StatsArray&(9 + A%, X%) = LEN(StrSplit$(2, "|", inText$))
                                END IF
                        NEXT A%
                CASE 2 'Puzzle
                        K% = 9 + CollCount% 'Get Starting position in array for Puzzles
                        FOR A% = 1 TO PuzCount%
                                UseK% = K% + ((A% - 1) * 6)
                                TempA$ = GetFirstSplit$(MID$(PuzHead$(A%), 2), "|")
                                TempB$ = GetFirstSplit$(inText$, "|")
                               
                                IF TempA$ = TempB$ THEN 'PopulatePlayer X% values for puzzle(1)PUZNAME|(2)TIMES|(3)LASTTIME|(4)PTIME|(5)WPM|(6)KEYTIME|(7)INV|(8)BAG
                                        TempB$ = StrSplit$(1, "|", inText$) 'Now split it
                                        T& = VAL(SplitWords$(2)): StatsArray&(UseK% + 1, X%) = T&'Times Completed
                                        FOR Z% = 2 TO 6: T& = VAL(SplitWords$(Z% + 2)): StatsArray&(UseK% + Z%, X%) = T&: NEXT Z%
                                        FOR B% = 1 TO FPCount%  'Grab Unique Free Plays completed
                                                IF TempB$ = FPList$(B%) THEN StatsArray&(1, X%) = StatsArray&(1, X%) + 1
                                        NEXT B%
                                        StatsArray&(6, X%) = StatsArray&(6, X%) + 1 'Increment unique puzzle complete
                                        StatsArray&(7, X%) = StatsArray&(7, X%) + VAL(SplitWords$(5))'Add WPM Score
                                        StatsArray&(8, X%) = StatsArray&(8, X%) + VAL(SplitWords$(6))'Add Keytime Score
                                        StatsArray&(9, X%) = StatsArray&(9, X%) + VAL(SplitWords$(8))'Add BagCount Score
                                END IF
                        NEXT A%
        END SELECT
        LOOP
        'Retrofit averages for this player - avoid DIV/0
        IF StatsArray&(6, X%) > 0 THEN FOR Z% = 7 TO 9: StatsArray&(Z%, X%) = StatsArray&(Z%, X%) \ StatsArray&(6, X%) + 1: NEXT Z%
NEXT X%

CLOSE #F

'Compile the report (optimise this!)
COLOR 0, 7: inText$ = "Compile Report": LOCATE 2, 40: PRINT STRING$(39, " "); : LOCATE 2, 78 - LEN(inText$): PRINT inText$; : COLOR 7, 0

'Load Player Names for quick reference
REDIM PName$(12): FOR B% = 1 TO pCount%: PName$(B%) = StrSplit$(2, "|", PlayerHead$(B%)): NEXT B%

G = FREEFILE
OPEN "XYSTATS.TXT" FOR OUTPUT AS #G

'//Free Play Progress
'uCell$ = "Free Play Progress": uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, "ี"; STRING$(20, "อ"); "ธ": PRINT #G, "ณ"; uCell$; "ณ": PRINT #G, "ิ"; STRING$(20, "อ"); "พ"
PRINT #G, "xyWords Statistics Report": PRINT #G, "อออออออออออออออออออออออออ"
PRINT #G, "": PRINT #G, "Generated: "; TIME$; " "; DATE$: PRINT #G, "": PRINT #G, ""

PRINT #G, "ฺ"; STRING$(15, "ฤ"); "ย"; STRING$(5, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ย"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ฟ"
PRINT #G, "ณ"; : uCell$ = "Free Play": uCell$ = uCell$ + STRING$(15 - LEN(uCell$), " "): PRINT #G, uCell$; "ณ"; "Total";
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = PName$(B%): uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"
PRINT #G, "ร"; STRING$(15, "ฤ"); "ล"; STRING$(5, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ล"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ด"
PRINT #G, "ณ"; : uCell$ = "Progress": uCell$ = uCell$ + STRING$(15 - LEN(uCell$), " "): PRINT #G, uCell$; "ณ"; : uCell$ = STR$(FPCount%): uCell$ = uCell$ + STRING$(5 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(1, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"
PRINT #G, "ภ"; STRING$(15, "ฤ"); "ม"; STRING$(5, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ม"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ู"

'//Cumulative Stats
PRINT #G, "": PRINT #G, ""'Double CRLF for section split
'uCell$ = "Cumulative Stats": uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, "ี"; STRING$(20, "อ"); "ธ": PRINT #G, "ณ"; uCell$; "ณ": PRINT #G, "ิ"; STRING$(20, "อ"); "พ"

PRINT #G, "ฺ"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ย"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ฟ"
PRINT #G, "ณ"; : uCell$ = "Cumulative Stats": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = PName$(B%): uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"
PRINT #G, "ร"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ล"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ด"

PRINT #G, "ณ"; : uCell$ = "Time Played": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = BigTime$(StatsArray&(2, B%)): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Words Found": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(3, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Complete/Replay Puzzle": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(4, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Total Errors": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(5, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ภ"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ม"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ู"

'//Puzzle Stats
PRINT #G, "": PRINT #G, ""'Double CRLF for section split
'uCell$ = "Puzzle Stats": uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, "ี"; STRING$(20, "อ"); "ธ": PRINT #G, "ณ"; uCell$; "ณ": PRINT #G, "ิ"; STRING$(20, "อ"); "พ"

PRINT #G, "ฺ"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ย"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ฟ"
PRINT #G, "ณ"; : uCell$ = "Puzzle Stats": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = PName$(B%): uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"
PRINT #G, "ร"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ล"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ด"

PRINT #G, "ณ"; : uCell$ = "Unique Puzzles Complete": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(6, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Average WPM": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(7, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Average Key Time": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(8, B%)))) + "sec":  uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Average Bag Words": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(9, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ภ"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ม"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ู"


'//Collection Progress
PRINT #G, "": PRINT #G, ""'Double CRLF for section split
'uCell$ = "Collection Progress": uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, "ี"; STRING$(20, "อ"); "ธ": PRINT #G, "ณ"; uCell$; "ณ": PRINT #G, "ิ"; STRING$(20, "อ"); "พ"

'Collection Grid
PRINT #G, "ฺ"; STRING$(20, "ฤ"); "ย"; STRING$(5, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ย"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ฟ"
PRINT #G, "ณ"; : uCell$ = "Collection Puzzles": uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, uCell$; "ณ"; "Total";
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = PName$(B%): uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"
PRINT #G, "ร"; STRING$(20, "ฤ"); "ล"; STRING$(5, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ล"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ด"
FOR A% = 10 TO 9 + CollCount% 'Collection Stats
        uCell$ = MID$(GetFirstSplit$(CollHead$(A% - 9), "|"), 2): uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, "ณ"; uCell$;
        uCell$ = CollData$(A% - 9): uCell$ = uCell$ + STRING$(5 - LEN(uCell$), " "): PRINT #G, "ณ"; uCell$;
        FOR B% = 1 TO pCount%: uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(A%, B%)))): uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, "ณ"; uCell$; : NEXT B%: PRINT #G, "ณ"
NEXT A%
PRINT #G, "ภ"; STRING$(20, "ฤ"); "ม"; STRING$(5, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ม"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ู"
'//Collection Progress

'//Puzzle Records
FOR P% = 1 TO PuzCount%
UseK% = 9 + CollCount% + ((P% - 1) * 6)
PRINT #G, "": PRINT #G, ""'Double CRLF for section split
'uCell$ = "Puzzle Records": uCell$ = uCell$ + STRING$(20 - LEN(uCell$), " "): PRINT #G, "ี"; STRING$(20, "อ"); "ธ": PRINT #G, "ณ"; uCell$; "ณ": PRINT #G, "ิ"; STRING$(20, "อ"); "พ"

PRINT #G, "ฺ"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ย"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ฟ"
PRINT #G, "ณ"; : uCell$ = GetFirstSplit$(MID$(PuzHead$(P%), 2), "|") + " Stats": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = PName$(B%): uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"
PRINT #G, "ร"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ล"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ด"

PRINT #G, "ณ"; : uCell$ = "Times Completed": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = RTRIM$(LTRIM$(STR$(StatsArray&(UseK% + 1, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Fastest Time": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = BigTime$(StatsArray&(UseK% + 2, B%)): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "1st Keyword Time": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = BigTime$(StatsArray&(UseK% + 3, B%)):   uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Least Errors": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(UseK% + 4, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ณ"; : uCell$ = "Most Bag Words": uCell$ = uCell$ + STRING$(25 - LEN(uCell$), " "): PRINT #G, uCell$;
FOR B% = 1 TO pCount%: PRINT #G, "ณ"; : uCell$ = LTRIM$(RTRIM$(STR$(StatsArray&(UseK% + 5, B%)))): : uCell$ = uCell$ + STRING$(12 - LEN(uCell$), " "): PRINT #G, uCell$; : NEXT B%: PRINT #G, "ณ"

PRINT #G, "ภ"; STRING$(25, "ฤ"); : FOR B% = 1 TO pCount%: PRINT #G, "ม"; STRING$(12, "ฤ"); : NEXT B%: PRINT #G, "ู"
NEXT P%

CLOSE #G

COLOR 0, 7: inText$ = "Load Report": LOCATE 2, 40: PRINT STRING$(39, " "); : LOCATE 2, 79 - LEN(inText$): PRINT inText$; : COLOR 7, 0
G = FREEFILE
SPosLines! = 1: WideX% = 1
REDIM SPos!(1024)

OPEN "XYSTATS.TXT" FOR INPUT AS #G
DO
        SPos!(SPosLines!) = SEEK(G)
        LINE INPUT #G, Temp$: SPosLines! = SPosLines! + 1
        L% = LEN(Temp$): IF L% > WideX% THEN WideX% = L%
        IF EOF(G) THEN : SEEK #G, 1: EXIT DO
LOOP

StartX% = 4: EndX% = 76: StartY% = 4: EndY% = 23: DisableX% = 0
WideX% = WideX% + 1: FCursorX% = 1: FCursorY% = 1'Starting Cursor position (and fix last char not showing)

'Arrows
LOCATE 4, 79: COLOR 0, 7: PRINT CHR$(24);
LOCATE 23, 79: PRINT CHR$(25); : LOCATE 24, 2: PRINT CHR$(27);
LOCATE 24, 79: PRINT CHR$(26); : : COLOR 7, 0
COLOR 0, 7: LOCATE 2, 35: PRINT CHR$(24); CHR$(25); CHR$(27); CHR$(26); " Scroll - ESC Quit";

VS.RefreshVis:
IF FCursorX% < 1 THEN FCursorX% = 1
COLOR 0, 7: inText$ = "Ready" + STR$(FRE("") \ 1024) + "K": LOCATE 2, 60: PRINT STRING$(19, " "); : LOCATE 2, 79 - LEN(inText$): PRINT inText$; : COLOR 7, 0
FOR Y% = 5 TO 22: LOCATE Y%, 79: PRINT "ฒ"; : NEXT Y%
PCT% = INT((FCursorY% / SPosLines!) * 18)
LOCATE 5 + PCT%, 79: PRINT "ฐ";
PCT% = ((FCursorX% + 72) / WideX%) * 75
IF PCT% < 1 THEN PCT% = 1
IF PCT% > 75 THEN PCT% = 75

LOCATE 24, 3: PRINT STRING$(76, "ฒ"); : LOCATE 24, 3 + PCT%: PRINT "ฐ";

IF SPos!(FCursorY%) > 0 THEN
        SEEK #G, SPos!(FCursorY%): REDIM Report$(73 * 20): CY% = 0
        ClearLine StartX%, StartY%, EndX%, EndY%
        FOR YY% = FCursorY% TO FCursorY% + 19
                LINE INPUT #G, Report$(YY%): MaxX% = EndX% - StartX%
                IF LEN(Report$(YY%)) < MaxX% THEN MaxX% = LEN(Report$(YY%))
                uTemp$ = MID$(Report$(YY%), FCursorX%)
                IF LEN(uTemp$) > EndX% - StartX% THEN uTemp$ = LEFT$(uTemp$, EndX% - StartX%)
                LOCATE StartY% + CY%, StartX%: PRINT uTemp$;
                CY% = CY% + 1
                IF EOF(G) THEN EXIT FOR
        NEXT YY%
END IF

VS.ViewGetKey:
SELECT CASE GetKey$
        CASE CHR$(0) + CHR$(71): FCursorX% = 1: GOTO VS.RefreshVis
        CASE CHR$(27): GOTO VS.ViewExit
        CASE CHR$(0) + CHR$(72) 'Up
                FCursorY% = FCursorY% - 1
                IF FCursorY% < 1 THEN FCursorY% = 1
                GOTO VS.RefreshVis
        CASE CHR$(0) + CHR$(80) 'Down
                FCursorY% = FCursorY% + 1
                IF FCursorY% > SPosLines! THEN FCursorY% = SPosLines! - 20
                GOTO VS.RefreshVis
        CASE CHR$(0) + CHR$(73) 'PgUp
                FCursorY% = FCursorY% - 19
                IF FCursorY% < 1 THEN FCursorY% = 1
                GOTO VS.RefreshVis
        CASE CHR$(0) + CHR$(81) 'PgDn
                FCursorY% = FCursorY% + 19
                IF FCursorY% > SPosLines! THEN FCursorY% = SPosLines! - 20
                GOTO VS.RefreshVis
        CASE CHR$(0) + CHR$(71): FCursorX% = 1: GOTO VS.RefreshVis'Home
        CASE CHR$(0) + CHR$(79): FCursorX% = WideX% - (EndX% - StartX%): GOTO VS.RefreshVis 'End
        CASE CHR$(0) + CHR$(115) 'CTRL+LEFT
        FCursorX% = FCursorX% - 10: IF FCursorX% < 1 THEN FCursor% = 1
        GOTO VS.RefreshVis
        CASE CHR$(0) + CHR$(116) 'CTRL+RIGHT
        FCursorX% = FCursorX% + 10
        IF FCursorX% < WideX% - (EndX% - StartX%) THEN
                GOTO VS.RefreshVis
                ELSE
                FCursorX% = WideX% - (EndX% - StartX%)
                GOTO VS.RefreshVis
        END IF
        
        CASE CHR$(0) + CHR$(75) 'Left
        IF FCursorX% > 1 THEN FCursorX% = FCursorX% - 1: GOTO VS.RefreshVis
        CASE CHR$(0) + CHR$(77) 'Right
        IF FCursorX% < WideX% - (EndX% - StartX%) THEN
                FCursorX% = FCursorX% + 1: GOTO VS.RefreshVis
                ELSE
                FCursorX% = WideX% - (EndX% - StartX%)
                GOTO VS.RefreshVis 'End
        END IF
END SELECT
GOTO VS.ViewGetKey

VS.ViewExit:
ERASE Report$
ERASE SPos!
CLOSE #G: COLOR 7, 0
END SUB

SUB xyBannerDraw (uRow%, uCol%)
'Credit:  https://patorjk.com/ Stop font
COLOR 15, 0: LOCATE uRow%, uCol%: PRINT "             _  _  _                _      "; : LOCATE uRow% + 1, uCol%: PRINT "            | || || |              | |     ";
LOCATE uRow% + 2, uCol%: PRINT " _   _ _   _| || || | ___   ____ _ | | ___ "; : LOCATE uRow% + 3, uCol%: PRINT "( \ / | | | | ||_|| |/ _ \ / ___/ || |/___)";
LOCATE uRow% + 4, uCol%: PRINT " ) X (| |_| | |___| | |_| | |  ( (_| |___ |"; : LOCATE uRow% + 5, uCol%: PRINT "(_/ \_)\__  |\______|\___/|_|   \____(___/ ";
LOCATE uRow% + 6, uCol%: PRINT "      (____/ "; : COLOR 7, 0: PRINT " ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ "; : LOCATE uRow% + 7, uCol%: PRINT "                         Text-Mode Edition";
END SUB

