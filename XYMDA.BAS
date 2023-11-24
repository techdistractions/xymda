DECLARE SUB ViewHelp ()
DECLARE SUB KillPlayer ()
DECLARE SUB MMenu ()
DECLARE FUNCTION GetFirstSplit$ (inTxt$, SplitBy$)
DECLARE SUB ViewStats ()
DECLARE FUNCTION BigTime$ (totalSeconds&)
DECLARE SUB SyncPlayerStat ()
DECLARE FUNCTION DateString$ (inText$, inStyle%)
DECLARE SUB SavePlayer ()
DECLARE SUB InitGameDat ()
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

'xyWords Text-Mode Edition (xyMDA)
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

'$DYNAMIC
DIM SHARED SplitWords$(128), TextHold$(6), FPList$(128), CollHead$(128), CollData$(128), FocusColl$(128), PuzHead$(256), PuzData$(128), FocusPuz$(64), FocusPuzWL$(32), pMDA%(33)
DIM SHARED PlayerHead$(64), FocusPlayerProg$(64), FocusPlayerStat$(256), FPMap%(10), Art$(10), FocusPuzF%(32)
DIM SHARED DoType%(32), DoOrient%(32), UseX%(32), UseY%(32), DoCommon%(32), PlayList$(256), UIList$(64), PlayerCulmStat&(16), SPos!(16), Report$(32)

'Screen Mode Check
SCREEN 0: CLS : DEF SEG = 0: Vmode% = PEEK(&H449): VCols% = PEEK(&H44A)
IF VCols% <> 80 THEN
        PRINT Vmode%; "xyWords requires 80 column text mode": PRINT "Y to Continue, other key to exit"
        IF GetKey$ = "Y" THEN : CLS : GOTO SkipSMode
        END 'Quit if other key is pressed
END IF

SkipSMode:
InitGameDat 'Load GAME.DAT for settings

'Setup Intro Screen
DrawBox 1, 1, 80, 25, 2 'Draw Screen Border [double]

'xyLogo
xyBannerDraw 2, 19: LOCATE 12, 18: PRINT "cross-word anagrams puzzle for DOS compatibles": LOCATE 13, 18: PRINT "   with MDA or Text-Only display adapters"

'Loading area
DrawBox 26, 16, 54, 18, 1 'StartX%,StartY%,EndX%,EndY%,BoxStyle%:LOCATE 17, 31

FileName$ = "xypuz.dat" 'Initialise the XYPUZ dataset
ON ERROR GOTO HandlerFile 'Compact fileexists checker using local scoped on error trap
F = FREEFILE: LastErr% = 0: OPEN FileName$ FOR INPUT AS #F
ON ERROR GOTO 0'Disable handler
IF LastErr% <> 0 THEN CLOSE #F: CLS : PRINT "FATAL ERROR: Cannot load XYPUZ.DAT": END
LastErr% = 0 'We are still alive, is OK
DatInit F, 1'Load and Parse XYPUZ.DAT and provide progress

PDInit:
LOCATE 17, 31: PRINT "Player Database   "; : FileName$ = "player.dat"'Initialise Player dataset
ON ERROR GOTO HandlerFile 'Compact fileexists checker using local scoped on error trap
F = FREEFILE: LastErr% = 0: OPEN FileName$ FOR INPUT AS #F
ON ERROR GOTO 0'Disable handler
IF LastErr% <> 0 THEN
        CLOSE #F: SkipFlag% = 2
        C% = BrowsePlayer%
        GOTO PDInit
END IF
LastErr% = 0 'We are still alive, everything is OK
PlayerInit F 'Populate PlayerHead$()

Main.InitStart:
'BrowsePlayer exit goto here
'MMenu 'Main Menu

Main.PlayerSelect:
'Browse & Select a Player
FocusPlayer% = BrowsePlayer%  'Browse for a Player
IF FocusPlayer% = 0 THEN GOTO Main.InitStart
SelectPlayer FocusPlayer% 'Select it and load profile

'Game Select Screen
Main.PlayerSelect.Game:
GameSelect 0
IF CollSelect% = 0 AND PuzSelect% = 0 THEN GOTO Main.PlayerSelect
SkipFlag% = 0'xyBanner is gone, redraw
DrawBox 30, 11, 50, 13, 2: ClearLine 31, 12, 49, 12: LOCATE 12, 32: PRINT "Loading..";

Main.LoadGame:
LoadPuzzle 'Load the Puzzle data (based on CollSelect% & PuzSelect%)

'Check if pMDASettings$ is of an expected length - if not then default values
IF LEN(pMDASettings$) < 14 THEN
pMDASettings$ = "15" + "00" + "07" + "00" + "02" + "00" + "07" + "BRICKS"
END IF

'Parse the settings
FOR X% = 1 TO 7: pMDA%(X%) = VAL(MID$(pMDASettings$, ((X% * 2) - 2) + 1, 2)): NEXT X%
pDecor$ = LTRIM$(RTRIM$(MID$(pMDASettings$, 15))) 'Get decorate string

IF pDecor$ <> "" AND pMDA%(5) = 2 THEN LoadArt "[BACKGROUND=" + pDecor$ + "]" 'Background in GRAPHICS.DAT
CLS : COLOR pMDA%(1), pMDA%(2): DrawBox 1, 1, 80, 25, 2: UIBorderDraw 50'Draw Screen and UI border
COLOR pMDA%(3), pMDA%(4): ShowPuzzle 'Draw the puzzle grid and mask

'Decorate Board/Table
SELECT CASE pMDA%(5)
CASE 1: COLOR pMDA%(6), pMDA%(7): DecorateTable pDecor$: COLOR 7, 0'Optional background processing
CASE 2
        COLOR pMDA%(6), pMDA%(7): DrawArt 2, 2, 0: COLOR 7, 0 'Draw the loaded art
        ERASE Art$ 'Unload it
END SELECT

'Status UI for Puzzle
SUICol% = 48: UpdateSUI 0, 0, ""'Initialise SUI
HelpUI 0 'Show help by default
PuzWheel$ = JumbleString$(PuzKeyWord$): LetterWheel  'Initialise the Letter wheel

'TypeUI init
KeyWordLength% = LEN(PuzKeyWord$) 'Set the length of the keyword
CWCol% = 50 + (7 - KeyWordLength%) 'Adjust the starting Col for the TypeUI
COLOR pMDA%(1), pMDA%(2)
LOCATE 23, CWCol% - 3: PRINT "³"; : LOCATE 22, CWCol% - 3: PRINT "Ú"; : LOCATE 24, CWCol% - 3: PRINT "À"; :
FOR X = 1 TO (KeyWordLength% * 2) STEP 2: LOCATE 22, CWCol% - 3 + X: PRINT "Ä"; : LOCATE 24, CWCol% - 3 + X: PRINT "Ä"; : NEXT X
FOR X = 2 TO (KeyWordLength% * 2) - 1 STEP 2: LOCATE 22, CWCol% - 3 + X: PRINT "Â"; : LOCATE 24, CWCol% - 3 + X: PRINT "Á"; :  LOCATE 23, CWCol% - 3 + X: PRINT "³"; : NEXT X
LOCATE 22, CWCol% - 3 + (KeyWordLength% * 2): PRINT "¿"; : LOCATE 24, CWCol% - 3 + (KeyWordLength% * 2): PRINT "Ù"; : LOCATE 23, CWCol% - 3 + (KeyWordLength% * 2): PRINT "³";
COLOR pMDA%(3), pMDA%(4): FOR X = 1 TO (KeyWordLength% * 2) STEP 2: LOCATE 23, CWCol% - 3 + X: PRINT " "; : NEXT X
PuzTimeCount% = 0: LastSec% = TIMER MOD 60: PuzBuffer$ = "": COLOR 7, 0

DO 'Core game timer
Coretimer:
        'Increment PuzTimeCount%
        Now% = TIMER MOD 60
        IF Now% <> LastSec% THEN 'Do this only when the seconds change
                PuzTimeCount% = PuzTimeCount% + 1
                UpdateSUI 1, 0, "": LastSec% = Now% 'Update Timer UI & Sync
                IF PuzTimeTotal% > 0 THEN 'Time up!
                        IF PuzTimeCount% >= PuzTimeTotal% THEN : GameErrorFlag% = 1: GOTO GameHandle.PuzFail
                END IF
        END IF

uKey$ = UCASE$(INKEY$) 'Get the key pressed

IF QuitFlag% = 1 THEN 'Waiting on Y/N to quit (no pause)
        SELECT CASE uKey$
                CASE "Y", "y": QuitFlag% = 0
                CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
                GOTO Main.PlayerSelect.Game
                CASE "N", "n": QuitFlag% = 0: HelpUI 0
        END SELECT
        GOTO Coretimer
END IF

SELECT CASE uKey$ 'Core game key handler
        CASE CHR$(0) + CHR$(62) 'PC Speaker Sound Toggle
        SELECT CASE EnableSound%
                CASE 0: EnableSound% = 1: PLAY "MB" + PlayList$(11): LOCATE 23, 64: COLOR 15: PRINT "F4"; : COLOR 7: PRINT " Mute Sound  ";
                CASE 1: EnableSound% = 0: LOCATE 23, 64: COLOR 15: PRINT "F4"; : COLOR 7: PRINT " Enable Sound";
        END SELECT
        CASE CHR$(27) 'Instaquit (put a confirm flag here later)
        LOCATE 24, 64: COLOR 16, 7: PRINT " Quit? [Y/N] "; : QuitFlag% = 1: COLOR 7, 0
        CASE CHR$(8): B% = LEN(PuzBuffer$): IF B% > 0 THEN LOCATE 23, (CWCol% - 4) + (B% * 2): COLOR pMDA%(3), pMDA%(4): PRINT " "; : COLOR pMDA%(3), pMDA%(4): B% = B% - 1: PuzBuffer$ = LEFT$(PuzBuffer$, B%)'Backspace
        CASE CHR$(13) 'Enter
        IF B% > 2 THEN 'If something is in the buffer
                'Submit the try
                FOR X% = 1 TO B%: LOCATE 23, (CWCol% - 4) + (X% * 2): COLOR pMDA%(3), pMDA%(4): PRINT " "; : COLOR pMDA%(3), pMDA%(4): NEXT X%'Clear buffer
                SELECT CASE SubmitTry% 'Attempt the PuzBuffer$
                        CASE 0: IF PuzBoardWordsCount% >= PuzBoardWordsTotal% THEN GOTO GameHandle.PuzComplete 'OK  - check for complete triggers
                        CASE 1: IF PuzInvTotal% > 0 THEN IF PuzInvCount% >= PuzInvTotal% THEN : GameErrorFlag% = 2: GOTO GameHandle.PuzFail'No invalid tries remain
                END SELECT
                PuzBuffer$ = "": B% = 0 'Reset flags
        END IF
        CASE CHR$(0) + CHR$(72): PuzWheel$ = JumbleString$(PuzWheel$): LetterWheel  'Up shuffle
        CASE CHR$(0) + CHR$(77): PuzWheel$ = RIGHT$(PuzWheel$, 1) + LEFT$(PuzWheel$, LEN(PuzWheel$) - 1): LetterWheel'Right rotate
        CASE CHR$(0) + CHR$(75): PuzWheel$ = MID$(PuzWheel$, 2) + LEFT$(PuzWheel$, 1): LetterWheel'Left rotate
        CASE CHR$(0) + CHR$(83): IF B% > 0 THEN FOR X% = 1 TO B%: LOCATE 23, (CWCol% - 4) + (X% * 2): COLOR 0, 7: PRINT " "; : COLOR 7, 0: NEXT X%: PuzBuffer$ = "": B% = 0'Clear buffer
        CASE " ": uKey$ = UCASE$(LEFT$(PuzWheel$, 1)): GOTO uKeyinsert 'Select top letter from wheel
        CASE CHR$(0) + CHR$(59): IF HelpUIView% = 1 AND PuzBagWordsTotal% > 0 THEN HelpUI 0 'F1
        CASE CHR$(0) + CHR$(60): IF HelpUIView% = 0 AND PuzBagWordsTotal% > 0 THEN HelpUI 1 'F2
        CASE CHR$(0) + CHR$(73) 'PgUp - wordbag - go up a page
        IF HelpUIView% = 1 AND PuzBagWordsCount% > 0 AND HelpUIPages% > 1 AND HelpUIPage% > 1 THEN
                HelpUIPage% = HelpUIPage% - 1: ClearLine 64, 12, 79, 13: StartCX% = ((HelpUIPage% - 1) * 8) + 1: EndCX% = StartCX% + 8: IF EndCX% > CX% THEN EndCX% = CX% 'decrease and show up to X lines
                FOR X% = StartCX% TO EndCX%: LOCATE 14 + X%, 64: PRINT UIList$(X%); : NEXT X%: LOCATE 23, 64: PRINT "PGUP/DN - Page"; : LOCATE 24, 64: PRINT HelpUIPage%; "of"; HelpUIPages%;
        END IF
        CASE CHR$(0) + CHR$(81) 'PgDn - wordbag - go down a page
        IF HelpUIView% = 1 AND PuzBagWordsCount% > 0 AND HelpUIPages% < HelpUIPage% THEN
                HelpUIPage% = HelpUIPage% + 1: ClearLine 64, 12, 79, 13: StartCX% = ((HelpUIPage% - 1) * 8) + 1: EndCX% = StartCX% + 8: IF EndCX% > CX% THEN EndCX% = CX% 'decrease and show up to X lines
                FOR X% = StartCX% TO EndCX%: LOCATE 14 + X%, 64: PRINT UIList$(X%); : NEXT X%: LOCATE 23, 64: PRINT "PGUP/DN - Page"; : LOCATE 24, 64: PRINT HelpUIPage%; "of"; HelpUIPages%;
        END IF

        CASE "A" TO "Z" 'Try to add to buffer
uKeyinsert:         'GOTO from space bar (controller input, not keypress)
       
        COLOR pMDA%(3), pMDA%(4)
        B% = LEN(PuzBuffer$): L% = LEN(PuzKeyWord$): C% = INSTR(1, PuzKeyWord$, uKey$)'Find the first instance
        IF C% > 0 THEN 'Correct letter, add some conditions to see if its valid..
             IF B% = 0 THEN 'First letter - VALID! ADD
                PuzBuffer$ = PuzBuffer$ + uKey$: LOCATE 23, CWCol% - 2: PRINT uKey$; : B% = B% + 1
             ELSE 'Check count of uKey$ in PuzBuffer$ and if >1 do additional check
                     BufferCheck% = 0
                     FOR X% = 1 TO B%
                        Test$ = MID$(PuzBuffer$, X%, 1): IF Test$ = uKey$ THEN BufferCheck% = BufferCheck% + 1
                     NEXT X%
                     IF BufferCheck% = 0 THEN 'VALID! ADD
                        PuzBuffer$ = PuzBuffer$ + uKey$: LOCATE 23, (CWCol% - 2) + (B% * 2): PRINT uKey$; : B% = B% + 1
                     ELSE 'Check how many times uKey$ exists in PuzKeyWord$
                             Flag% = 0
                             FOR X% = C% TO L%
                                Test$ = MID$(PuzKeyWord$, X%, 1): IF Test$ = uKey$ THEN Flag% = Flag% + 1
                             NEXT X%
                             IF BufferCheck% < Flag% THEN 'VALID! ADD
                               PuzBuffer$ = PuzBuffer$ + uKey$: LOCATE 23, (CWCol% - 2) + (B% * 2): PRINT uKey$; : B% = B% + 1
                             END IF
                     END IF
             END IF
        END IF
        
END SELECT
LOOP


END 'Sub-programs only
HandlerFile:
LastErr% = 1 'Return an error
RESUME NEXT


GameHandle.PuzComplete: 'Trigger: Puzzle has been completed
COLOR 7, 0: CLS : vSelect% = 0'Reset Flags
PlayerCulmStat&(1) = PlayerCulmStat&(1) + PuzTimeCount%
PlayerCulmStat&(3) = PlayerCulmStat&(3) + 1

'Show Art First
COLOR 15, 0: LoadArt "[XYWORDS=PUZCOMPLETE]": X% = 40 - IntUP(LEN(Art$(1)), 2): DrawArt X%, 3, 1: DrawBox X% - 2, 2, X% + 36, 11, 1: LOCATE 22, 35: COLOR 10'Load/Draw Art and Border

SyncPlayerStat 'Sync and Save Records
PuzRecord

'Stats Parser
LOCATE 15, 11: COLOR 7, 0: PRINT "Time Taken: "; TimeString$(PuzTimeCount%);
LOCATE 16, 11: PRINT "Speed:"; IntUP((((PuzBagWordsCount% + PuzBoardWordsCount%) / PuzTimeCount%) * 60), 1); "WPM"
LOCATE 17, 11: PRINT "Invalid/Duplicate Attempts:"; PuzInvCount%; : LOCATE 18, 11
IF PuzBagWordsTotal% > 0 THEN
        PRINT "Bag Words:"; PuzBagWordsCount%; "of"; PuzBagWordsTotal%
ELSE
        PRINT "Bag Words: None in this puzzle";
END IF
LOCATE 19, 11: PRINT "Keyword Found at: "; TimeString$(PuzKeyWordTime%);

'Record Triggers
IF PuzRecordChange$ <> "" THEN
        FOR X% = 1 TO LEN(PuzRecordChange$)
        LOCATE 14 + X%, 3: COLOR 10
        SELECT CASE MID$(PuzRecordChange$, X%, 1)
                CASE "T", "W", "I", "B": PRINT "RECORD"; : COLOR 7
        END SELECT
        NEXT X%
END IF

'Next Puzzle UI
SELECT CASE CollSelect%
CASE 0 'FreePlay
        LOCATE 13, 11: COLOR 0, 7: PRINT "Free Play (#"; PuzSelect%; "of"; FPCount%; ") "; FPList$(PuzSelect%); " Complete!": LOCATE 19, 50: COLOR 0, 7
        IF PuzSelect% < FPCount% THEN
                vSelect% = 1: PRINT " Next Up: "; FPList$(PuzSelect% + 1); " ": COLOR 7, 0 ': SyncPlayerStat: PuzRecord
                LOCATE 21, 50: PRINT "Press; Y for next puzzle"; : LOCATE 22, 50: PRINT "ESC to return to menu"; : LOCATE 23, 50: PRINT "R to retry this puzzle"; : DrawBox 48, 17, 78, 25, 1
        ELSE
                vSelect% = 2: LOCATE 18, 50: PRINT "Final Free Play Puzzle!"; : COLOR 7, 0: DrawBox 47, 16, 78, 21, 2
                LOCATE 19, 50: PRINT "Any key to to continue";
        END IF
       
CASE IS > 0'Collection
        Flag% = 0: inText$ = StrSplit$(1, "|", MID$(CollHead$(CollSelect%), 2))
        FOR X% = 1 TO VAL(FocusPlayerProg$(0)) 'Search for PRR
                Temp$ = StrSplit$(1, "|", FocusPlayerProg$(X%))
                uCollName$ = Temp$
                IF inText$ = Temp$ THEN : Flag% = X%: EXIT FOR
        NEXT X%
        
        SELECT CASE Flag% 'Write new or update progress record
                CASE 0: 'always write - repurpose Flag% 0 = not unlock, 1 = unlock
                        FocusPlayerProg$(VAL(FocusPlayerProg$(0)) + 1) = inText$ + "|#": FocusPlayerProg$(0) = LTRIM$(RTRIM$(STR$(VAL(FocusPlayerProg$(0)) + 1)))'New
                        IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(8)
                        Flag% = 1 'Unlocked
                CASE IS > 0: 'Check first
                        IF FocusCollPuz% > LEN(SplitWords$(2)) THEN 'Update
                                FocusPlayerProg$(Flag%) = FocusPlayerProg$(Flag%) + "#" 'Update
                                Flag% = 1 'Unlocked
                                IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(8)
                        ELSE
                                Flag% = 0 'Replayed
                        END IF
        END SELECT

        IF Flag% = 1 THEN SavePlayer
        

IF FocusCollPuz% < FocusCollCount% THEN 'Offer Next Puzzle
        vSelect% = 3: inText$ = StrSplit$(1, "|", FocusColl$(FocusCollPuz% + 1))
        'COLOR 15, 0: LoadArt "[XYWORDS=PUZCOMPLETE]": X% = 40 - IntUP(LEN(Art$(1)), 2): DrawArt X%, 3, 1: DrawBox X% - 2, 2, X% + 36, 11, 1: LOCATE 22, 35: COLOR 10'Load/Draw Art and Border
        'Have we unlocked a new puzzle?
        LOCATE 22, 35: COLOR 10
        IF Flag% = 1 THEN
                PRINT "Unlocked a Puzzle: "; inText$;
                LoadArt "[XYWORDS=STAR]": COLOR 18: DrawArt 60, 12, 1: COLOR 7
                IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(10)
        ELSE
                PRINT "Next Puzzle: "; inText$;
        END IF
        DrawBox 34, 21, 75, 25, 1: LOCATE 23, 35: COLOR 7: PRINT "Press Y to play the next puzzle"; : LOCATE 24, 35: PRINT "Press ESC to return to the menu";

        FOR X% = 1 TO PuzCount%   'Advance to next Puzzle
                Temp$ = StrSplit$(1, "|", MID$(PuzHead$(X%), 2))
                IF Temp$ = inText$ THEN : FocusCollPuz% = FocusCollPuz% + 1: PuzSelect% = X%: EXIT FOR
        NEXT X%
ELSE
        vSelect% = 4 'Collection Complete
        LOCATE 22, 60: PRINT "Press a key..";
        SELECT CASE Flag%
                CASE 0 'Already played - just put the message
                        
                CASE 1 'Celebrate then put message
                        i$ = GetKey$
                        CLS : LoadArt "[XYWORDS=COLWIN]": COLOR 10: DrawArt 19, 2, 1
                        PLAY "MB" + PlayList$(9)
                        LoadArt "[XYWORDS=TICK]": DrawArt 70, 2, 1: DrawArt 70, 9, 1
                        LoadArt "[XYWORDS=STAR]": DrawArt 1, 5, 1: COLOR 7
                        LOCATE 16, 10: PRINT "Congratulations!"; : COLOR 7: LOCATE 18, 10
                        PRINT "You have completed the "; uCollName$; " Collection"; : LOCATE 23, 50: PRINT "Press any key to continue..";
        END SELECT
END IF
END SELECT 'End of Puzzle Complete!
   

'Key Handler
GameHandle.ExitKey:
Temp$ = GetKey$
SELECT CASE vSelect%
        CASE 0
        CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
        GOTO Main.PlayerSelect.Game
        CASE 1 'FP: Y Next ESCQuit Retry
                SELECT CASE Temp$
                CASE CHR$(27)
                        CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
                        GOTO Main.PlayerSelect.Game
                CASE "Y", "y" 'Next puz
                       DrawBox 30, 11, 50, 13, 2: ClearLine 31, 12, 49, 12: LOCATE 12, 32: PRINT "Loading..";
                       PuzSelect% = PuzSelect% + 1
                       GOTO Main.LoadGame

                CASE "R", "r" 'Retry
                       DrawBox 30, 11, 50, 13, 2: ClearLine 31, 12, 49, 12: LOCATE 12, 32: PRINT "Loading..";
                       GOTO Main.LoadGame
                END SELECT
        CASE 2 'FP: Any key quit
                CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
                GOTO Main.PlayerSelect.Game
        CASE 3 'C: YNext ESCQuit
        SELECT CASE Temp$
                CASE "Y", "y":  GOTO Main.LoadGame
                CASE CHR$(27)
                CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
                GOTO Main.PlayerSelect.Game
        END SELECT
        CASE 4 'C: Any key -> Celebrate (if) -> Quit
        CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
        GOTO Main.PlayerSelect.Game
END SELECT
GOTO GameHandle.ExitKey 'Loop back

GameHandle.PuzFail: 'A fail condition has been met
SELECT CASE GameErrorFlag%
CASE 1 'TimeOut
        CLS : COLOR 15, 0: LoadArt "[XYWORDS=PUZFAILTIME]": DrawArt 2, 2, 1: PRINT : COLOR 15: LOCATE 10, 3: PRINT "Time Limit Reached!"; : COLOR 7
        PlayerCulmStat&(1) = PlayerCulmStat&(2) + PuzTimeCount%
        DrawBox 2, 9, 32, 16, 1: SyncPlayerStat: SavePlayer
LOCATE 12, 3: PRINT "Would you like to retry?"; : LOCATE 14, 3: PRINT "Press Y to Retry"; : LOCATE 15, 3: PRINT "Any other key returns to menu";

CASE 2 'Invalid Words Limit Reached
        CLS : COLOR 15, 0: LoadArt "[XYWORDS=PUZFAILINV]": DrawArt 40 - (LEN(Art$(1)) / 2), 2, 1: LOCATE 13, 3: PRINT "Invalid try limit reached!";
        LoadArt "[XYWORDS=TEDDY]": DrawArt 60, 13, 1
        COLOR 7, 0: LOCATE 15, 3: PRINT "Would you like to retry?"; : LOCATE 16, 3: PRINT "Press Y to Retry"; : LOCATE 17, 3: PRINT "Any other key returns to menu";
        PlayerCulmStat&(1) = PlayerCulmStat&(2) + PuzTimeCount%
        DrawBox 2, 12, 32, 19, 1: SyncPlayerStat: SavePlayer
        'Put the attempted puzzle stats on the right

END SELECT


Temp$ = GetKey$
IF UCASE$(Temp$) = "Y" THEN
        CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
        GOTO Main.LoadGame
ELSE
        CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]
        GOTO Main.PlayerSelect.Game
END IF

REM $STATIC
FUNCTION BorderConnect (YPos%, XPos%, SegFlag%)
BorderConnect = 176 'Default Fail block

'Determine ASCII code for specified SegFlag% for a TilePosition (YPos%,XPos%)

'Each TilePosition has 4 surrounding border areas to calculate
'There are always 4 additional ones with Ä and ³ always surrounding the TilePosition
'   1Ä3    SegFlag% = 1,2,3&4
'   ³X³
'   2Ä4

SELECT CASE SegFlag% 'Select border focus position for segment
        CASE 1: YPos% = YPos% - 1: XPos% = XPos% - 1'Focus up/left
        CASE 2: YPos% = YPos% + 1: XPos% = XPos% - 1'Focus down/left
        CASE 3: YPos% = YPos% - 1: XPos% = XPos% + 1'Focus up/right
        CASE 4: YPos% = YPos% + 1: XPos% = XPos% + 1'Focus down/right
END SELECT

'Grab surrounding segments, default to 32 if on edge of playfield
TempBelow% = 32: IF YPos% < 24 THEN TempBelow% = SCREEN(YPos% + 1, XPos%)
TempRight% = 32: IF XPos% < 44 THEN TempRight% = SCREEN(YPos%, XPos% + 1)
TempLeft% = 32: IF XPos% > 2 THEN TempLeft% = SCREEN(YPos%, XPos% - 1)
TempAbove% = 32: IF YPos% > 2 THEN TempAbove% = SCREEN(YPos% - 1, XPos%)

SELECT CASE SegFlag% 'Conditions to generate BorderConnect response
        CASE 1 'Upper left - Possible; 197Å , 218Ú ,195Ã, 194Â
                IF TempLeft% = 32 AND TempAbove% = 32 THEN BorderConnect = 218  'Ú
                IF TempLeft% = 196 AND TempAbove% = 179 THEN BorderConnect = 197  'Å
                IF TempLeft% = 196 AND TempAbove% = 32 THEN BorderConnect = 194 'Â
                IF TempLeft% = 32 AND TempAbove% = 179 THEN BorderConnect = 195 'Ã

        CASE 2 'Lower left - Possible; 192À,197Å,193Á,195Ã
                'TempAbove% and TempRight% are redundant
               IF TempLeft% = 32 AND TempBelow% = 32 THEN BorderConnect = 192 'À
               IF TempLeft% = 196 AND TempBelow% = 179 THEN BorderConnect = 197 'Å
               IF TempLeft% = 196 AND TempBelow% = 32 THEN BorderConnect = 193 'Á
               IF TempLeft% = 32 AND TempBelow% = 179 THEN BorderConnect = 195 'Ã
       
        CASE 3 'Upper right - Possible; 191¿,194Â,180´,197Å
                'TempBelow% and TempLeft% are redundant
                IF TempRight% = 32 AND TempAbove% = 32 THEN BorderConnect = 191 '¿
                IF TempRight% = 196 AND TempAbove% = 179 THEN BorderConnect = 197 'Å
                IF TempRight% = 196 AND TempAbove% = 32 THEN BorderConnect = 194 'Â
                IF TempRight% = 32 AND TempAbove% = 179 THEN BorderConnect = 180 '´
                        

        CASE 4 'Lower right - Possible; 217Ù,193Á,180´,197Å
                'TempAbove% and TempLeft% are redundant
                IF TempRight% = 32 AND TempBelow% = 32 THEN BorderConnect = 217 'Ù
                IF TempRight% = 196 AND TempBelow% = 32 THEN BorderConnect = 193 'Á
                IF TempRight% = 196 AND TempBelow% = 179 THEN BorderConnect = 197 'Å
                IF TempRight% = 32 AND TempBelow% = 179 THEN BorderConnect = 180 '´
END SELECT

END FUNCTION

FUNCTION BrowsePlayer%

SELECT CASE SkipFlag%
CASE 1: GOTO BrowsePlayer.UI
CASE 2: GOTO Player.New
END SELECT
BrowsePlayer.Start:
CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen

BrowsePlayer.UI:
'Displays available player profiles and provides a small UI to manage
BrowsePlayer% = 1: ShowStats% = 0

Browse.MainMenu:
ClearLine 2, 11, 79, 24 'Prepare Viewport

'Draw Main Menu
MenuD% = 48: COLOR 7, 0: DrawBox MenuD% - 2, 11, MenuD% + 29, 23, 2
LOCATE 13, MenuD%: PRINT "["; : COLOR 0, 7: PRINT CHR$(24) + CHR$(25); : COLOR 7, 0: PRINT "] Select Player";
LOCATE 14, MenuD%: PRINT "["; : COLOR 0, 7: PRINT "ENTER"; : COLOR 7, 0: PRINT "] Start Playing!";
LOCATE 16, MenuD%: PRINT "["; : COLOR 0, 7: PRINT "F1"; : COLOR 7, 0: PRINT "] Create New Player";
LOCATE 17, MenuD%: PRINT "["; : COLOR 0, 7: PRINT "F2"; : COLOR 7, 0: PRINT "] Edit Selected Player";
LOCATE 19, MenuD%: PRINT "["; : COLOR 0, 7: PRINT "F5"; : COLOR 7, 0: PRINT "] View Stats & Records";
LOCATE 20, MenuD%: PRINT "["; : COLOR 0, 7: PRINT "F10"; : COLOR 7, 0: PRINT "] View Help & Info";
LOCATE 21, MenuD%: PRINT "["; : COLOR 0, 7: PRINT "ESC"; : COLOR 7, 0: PRINT "] Exit to DOS";

MenuC% = 10: PlayerCount% = VAL(PlayerHead$(0))
IF PlayerCount% > 9 THEN PlayerCount% = 9: 'Hardcoded block for 9 players, otherwise patch for pagination
LOCATE 11, MenuC%: COLOR 9: PRINT "Player Select Menu"; : COLOR 7
FOR C% = 1 TO PlayerCount%: LOCATE 12 + C%, MenuC%: COLOR 7, 0: PRINT "[ ]"; " "; StrSplit$(2, "|", PlayerHead$(C%)); : NEXT C%

vSelect% = 1: PrevSelect% = 1

'Player Select Menu
Player.vSelect:
IF PrevSelect% <> vSelect% THEN LOCATE 12 + PrevSelect%, MenuC% + 1: PRINT " "; 'Clear old selection
IF vSelect% > 0 AND vSelect% <= PlayerCount% THEN 'New Selection
        LOCATE 12 + vSelect%, MenuC% + 1: COLOR 7: PRINT CHR$(16);
ELSE
        IF vSelect% > PlayerCount% THEN vSelect% = 1 'Send to top
        IF vSelect% < 1 THEN vSelect% = PlayerCount% 'Send to bottom
        GOTO Player.vSelect
END IF

SELECT CASE GetKey$ 'Menu Key
        CASE CHR$(27): ExitGame 'ESC
        CASE CHR$(0) + CHR$(72), "W", "w", "8": PrevSelect% = vSelect%: vSelect% = vSelect% - 1'Up
        CASE CHR$(0) + CHR$(80), "S", "s", "2": PrevSelect% = vSelect%: vSelect% = vSelect% + 1'Down
        CASE CHR$(13):  BrowsePlayer% = vSelect%: ClearLine 2, 11, 79, 24: LOCATE 11, 2: COLOR 0, 7: PRINT "Please Wait.."; : COLOR 7, 0: EXIT FUNCTION'Enter
        CASE CHR$(0) + CHR$(59) 'F1 - Create New
                GOTO Player.New
        CASE CHR$(0) + CHR$(60)'F2 - Modify
                SelectPlayer vSelect%
                GOTO Player.Modify
        CASE CHR$(0) + CHR$(63) 'F5 - Stats
                ViewStats
                GOTO BrowsePlayer.Start
        CASE CHR$(0) + CHR$(68) 'F10 - Help & Info
                ViewHelp
                GOTO BrowsePlayer.Start
END SELECT
GOTO Player.vSelect 'Loop back around

Player.Modify: 'Small sub to view and edit PlayerHead$()
MenuD% = 40: MenuC% = 9: ClearLine 2, 11, 79, 24'Prepare Viewport
LOCATE 11, MenuC%: COLOR 9: PRINT "Edit Player"; : COLOR 7
Temp$ = StrSplit$(1, "|", PlayerHead$(vSelect%))

LOCATE 15, 50: PRINT "Enter up to 10 characters";
LOCATE 17, 50: PRINT "["; : COLOR 0, 7: PRINT "<Ù"; : COLOR 7, 0: PRINT "] Save";
LOCATE 18, 50: PRINT "["; : COLOR 0, 7: PRINT "F3"; : COLOR 7, 0: PRINT "] Delete Player";
LOCATE 19, 50: PRINT "["; : COLOR 0, 7: PRINT "ESC"; : COLOR 7, 0: PRINT "] Quit back to Select";

B& = VAL(SplitWords$(5))
COLOR 0, 7: DrawBox MenuC% - 2, 13, MenuC% + 26, 20, 1: COLOR 7, 0
LOCATE 15, MenuC%:  PRINT "Player name: "; SplitWords$(2);
LOCATE 16, MenuC%:  PRINT "Created on: "; DateString$(SplitWords$(3), 2);
LOCATE 17, MenuC%: PRINT "Last played: "; DateString$(SplitWords$(4), 2);
LOCATE 18, MenuC%:  PRINT "Play time: "; BigTime$(B&); : COLOR 7, 0

Player.EditSubMenu: 'Handle all of the Edit/Show items here

Buffer$ = SplitWords$(2) 'Clear buffer

Player.EditBuf:                          
LOCATE 15, 22: COLOR 0, 7: PRINT Buffer$; STRING$(10 - LEN(Buffer$), " "); : COLOR 7, 0

Player.EditKey:
i$ = GetKey$
A% = ASC(i$): OK% = 0
SELECT CASE i$
        CASE CHR$(27): GOTO Browse.MainMenu 'Exit
        CASE CHR$(8) 'Backspace Buffer
        IF LEN(Buffer$) > 0 THEN
                Buffer$ = LEFT$(Buffer$, LEN(Buffer$) - 1): GOTO Player.EditBuf
        ELSE
                GOTO Player.EditKey
        END IF
        CASE CHR$(13) 'Save and Exit
        Buffer$ = LTRIM$(RTRIM$(Buffer$))
        IF Buffer$ <> "" AND Buffer$ <> SplitWords$(2) THEN 'Check and Save

                Flag% = 0: pCount% = VAL(PlayerHead$(0))
                Temp$ = SplitWords$(2) 'save old name in case
                FOR X% = 1 TO pCount%
                IF StrSplit$(2, "|", PlayerHead$(X%)) = Buffer$ THEN : Flag% = 1: EXIT FOR
                NEXT X%

                IF Flag% = 1 THEN 'Fail
                        COLOR 0, 7: LOCATE 23, 10: PRINT Buffer$; " Name already exists! (key..)": COLOR 7, 0
                        SLEEP: LOCATE 23, 10: PRINT STRING$(50, " "); : Buffer$ = Temp$: GOTO Player.EditBuf
                END IF

                inText$ = StrSplit$(1, "|", PlayerHead$(vSelect%))
                SplitWords$(2) = Buffer$
                FOR X% = 2 TO 8: inText$ = inText$ + "|" + SplitWords$(X%): NEXT X%
                IF RIGHT$(inText$, 1) <> "]" THEN inText$ = inText$ + "]" 'catch in case
                FocusPlayer% = vSelect%
                PlayerHead$(vSelect%) = inText$ 'Update
                SavePlayer 'Update PLAYER.DAT
                GOTO Browse.MainMenu
        ELSE 'No change to name - cancel
                GOTO Browse.MainMenu
        END IF
     
        CASE CHR$(0) + CHR$(61) 'Kill
     
        'UI Prompt
        V% = VAL(PlayerHead$(0))
        IF V% = 1 THEN ' FAIL! Its the only player
                LOCATE 22, 5: COLOR 10: PRINT "Cannot delete the only player!"; : LOCATE 23, 5: COLOR 10: PRINT "Any key to continue"; : COLOR 7
                i$ = GetKey$
                LOCATE 22, 5: PRINT STRING$(50, " "); : LOCATE 23, 5: PRINT STRING$(50, " ");
                GOTO Player.EditKey
        END IF
     
        LOCATE 22, 5: COLOR 10: PRINT "Are you sure you want to DELETE this player?";
        LOCATE 23, 5: PRINT "!ALL STATS AND RECORDS FOR THIS PLAYER WILL BE LOST!";
        LOCATE 24, 5: PRINT "Press 'K' to DELETE - any other key cancels";
                SELECT CASE GetKey$
                          CASE "K", "k": GOTO BP.Kill
                END SELECT

                'Anything else
                LOCATE 22, 5: PRINT STRING$(70, " "); : LOCATE 23, 5: PRINT STRING$(70, " "); : LOCATE 24, 5: PRINT STRING$(70, " "); : GOTO Player.EditKey

BP.Kill:
                FocusPlayer% = vSelect%
                KillPlayer
                GOTO BrowsePlayer.Start

END SELECT

SELECT CASE A% 'Allow 48-57, 65-90, 97-122 ,1-6 ,168-254,32
       CASE 48 TO 57, 65 TO 90, 97 TO 123, 1 TO 6, 168 TO 254, 32, 14, 19: OK% = 1
END SELECT
IF OK% = 1 AND LEN(Buffer$) < 10 THEN Buffer$ = Buffer$ + CHR$(A%): GOTO Player.EditBuf:

GOTO Player.EditKey'LOOP around

Player.New: 'Create a new player

 ClearLine 2, 11, 79, 24 'Prepare Viewport Mini CLS

LOCATE 11, 5: COLOR 9: PRINT "Create Player!";

'Have we got 9 players already?
VX% = VAL(PlayerHead$(0))
IF VX% >= 9 THEN
        LOCATE 17, 5: COLOR 10: PRINT "ERROR: You cannot create more than 9 players!";
        LOCATE 18, 5: PRINT "Press any key to exit"; : COLOR 7
        i$ = GetKey$
        GOTO BrowsePlayer.Start
END IF


LOCATE 13, 5: PRINT "Enter a Name"; : COLOR 7: PRINT ":"; : COLOR 0, 7: PRINT " ";

LOCATE 16, 5: COLOR 7, 0: PRINT "Use the keyboard to enter a name";
LOCATE 18, 5: PRINT "A-Z, 0-9 and Space is allowed";
LOCATE 19, 5: PRINT "Some KeyCodes are supported too "; CHR$(3)
LOCATE 21, 5: PRINT "[ENTER] Save Player";
IF SkipFlag% <> 2 THEN
        LOCATE 22, 5: PRINT "[ESC] Quit without Save";
        ELSE
        LOCATE 22, 5: PRINT "[ESC] Quit";
END IF
DrawBox 3, 15, 40, 24, 1
LoadArt "[XYWORDS=NEWPLAYER]"
COLOR 10: DrawArt 62, 9, 1: COLOR 7

Buffer$ = "" 'Clear buffer
BrowsePlayer.DispBuf:
LOCATE 13, 19: COLOR 0, 7: PRINT Buffer$; STRING$(10 - LEN(Buffer$), " "); : COLOR 7, 0
BrowsePlayer.GetKeyA:
i$ = GetKey$
A% = ASC(i$): V% = 1: OK% = 0
SELECT CASE i$
        CASE CHR$(27)
        IF SkipFlag% <> 2 THEN
                COLOR 7, 0: GOTO BrowsePlayer.Start 'Quit No Save
        ELSE    'Cannot continue no PLAYER.DAT
                CLS : PRINT "Quitting - you need to create a player to use xyWords": END
        END IF
        CASE CHR$(8) 'Backspace Buffer
        IF LEN(Buffer$) > 0 THEN
                Buffer$ = LEFT$(Buffer$, LEN(Buffer$) - 1): GOTO BrowsePlayer.DispBuf
        ELSE
                GOTO BrowsePlayer.GetKeyA
        END IF
        CASE CHR$(13)  'Save and Exit
        Buffer$ = RTRIM$(LTRIM$(Buffer$))
        IF Buffer$ <> "" THEN

                'If we are initialising PLAYER.DAT
                IF SkipFlag% = 2 THEN
                        G = FREEFILE: OPEN "PLAYER.DAT" FOR OUTPUT AS #G
                        TempA$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
                        Temp$ = "[P1" + "|" + Buffer$ + "|" + TempA$ + "|" + TempA$ + "|0|0|0|0]"
                        PRINT #G, Temp$: PRINT #G, "{COLLECTIONS}": PRINT #G, "{/COLLECTIONS}": PRINT #G, "{PUZZLES}": PRINT #G, "{/PUZZLES}": PRINT #G, "[/P1]"
                        CLOSE #G
                        PlayerHead$(0) = "1": PlayerHead$(1) = Temp$: FocusPlayer% = 1
                        SavePlayer
                        SkipFlag% = 0
                        EXIT FUNCTION
                END IF

                'Else - refer to existing playerhead

                T% = VAL(PlayerHead$(0)): V% = T% 'Get Current Player Count
                FOR Z% = 1 TO V% + 1 'P#'s to try
                        Flag% = 0 'Default to not found
                        FOR X% = 1 TO V% 'Check against existing list
                                IF GetFirstSplit$(MID$(PlayerHead$(X%), 2), "|") = "P" + LTRIM$(RTRIM$(STR$(Z%))) THEN
                                        Flag% = 1: EXIT FOR 'Fail! Try again
                                END IF
                        NEXT X%
                        IF Flag% = 0 THEN : V% = Z%: EXIT FOR
                NEXT Z%

                TempA$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
                Temp$ = "[P" + LTRIM$(RTRIM$(STR$(V%))) + "|" + Buffer$ + "|" + TempA$ + "|" + TempA$ + "|0|0|0|0]"
                PlayerHead$(0) = LTRIM$(RTRIM$(STR$(T% + 1)))
                PlayerHead$(T% + 1) = Temp$
                FocusPlayer% = T% + 1
                SavePlayer
                GOTO BrowsePlayer.Start
        END IF
      
END SELECT
SELECT CASE A% 'Allow 48-57, 65-90, 97-122 ,1-6 ,168-254,32
       CASE 48 TO 57, 65 TO 90, 97 TO 123, 1 TO 6, 168 TO 254, 32, 14, 19: OK% = 1
END SELECT
IF OK% = 1 AND LEN(Buffer$) < 10 THEN Buffer$ = Buffer$ + CHR$(A%): GOTO BrowsePlayer.DispBuf

GOTO BrowsePlayer.GetKeyA

END FUNCTION

SUB ClearLine (StartX%, StartY%, EndX%, EndY%)
'Simple routine for clearing line(s) or part thereof
FOR Y% = StartY% TO EndY%: Temp$ = STRING$(EndX% - StartX% + 1, " "): LOCATE Y%, StartX%: PRINT Temp$; : NEXT Y%
END SUB

SUB DatInit (FileHandle, Progress)
'Use #FileHandle to read the open file
Flag% = 0: FPCount% = 0: CollCount% = 0: PuzCount% = 0'Reset flag and free play counter
CK% = 41 'get current location for progress

DO
LINE INPUT #FileHandle, Temp$

SELECT CASE Flag%
CASE 0 'Search for Flag
        SELECT CASE Temp$
                CASE "[FREEPLAY]": Flag% = 1: IF Progress = 1 THEN LOCATE CSRLIN, CK%: PRINT "DAT";
                CASE "[COLLECTIONS]": Flag% = 2: IF Progress = 1 THEN LOCATE CSRLIN, CK%: PRINT "COL";
                CASE "[PUZZLES]": Flag% = 3: IF Progress = 1 THEN LOCATE CSRLIN, CK%: PRINT "PUZ"; :
        END SELECT
CASE 1 'FreePlay Section
        SELECT CASE Temp$
                CASE "[/FREEPLAY]": Flag% = 0
                CASE IS <> "[/FREEPLAY]", "": FPCount% = FPCount% + 1: FPList$(FPCount%) = Temp$
        END SELECT
CASE 2 'Collections Section
        SELECT CASE Temp$
        CASE "[/COLLECTIONS]": Flag% = 0'Stop counting collections
        CASE IS <> "[/COLLECTIONS]"
                IF LEN(Temp$) > 2 THEN 'Valid to count
                        SELECT CASE LEFT$(Temp$, 1)
                        CASE "{"
                                IF MID$(Temp$, 2, 1) = "/" THEN
                                CollData$(CollCount%) = LTRIM$(RTRIM$(STR$(Count%)))  'stop counting
                                ELSE
                                CollCount% = CollCount% + 1: CollHead$(CollCount%) = Temp$: Count% = 0'start counting
                                END IF
                        CASE IS <> "{": Count% = Count% + 1 'Count
                        END SELECT
                 END IF 'End of "Valid to count"
        END SELECT

CASE 3 'Puzzles mode
        SELECT CASE Temp$
        CASE "[/PUZZLES]": Flag% = 0'reset when finished
        CASE IS <> "[/PUZZLES]"
                IF LEN(Temp$) > 1 THEN 'Valid to count
                        SELECT CASE LEFT$(Temp$, 1)
                        CASE "#": DatPuzCount% = VAL(MID$(Temp$, 2)) 'checksum puzzle count
                        CASE "{"
                                IF MID$(Temp$, 2, 1) = "/" THEN 'stop counting
                                        'PuzData$(PuzCount%) = LTRIM$(STR$(CountKX%)) + STR$(CountP%) + STR$(CountB%) + STR$(CountC%) + STR$(CountU%)     'write PuzData$

                                ELSE 'Grab
                                        'CountKX% = 0: CountP% = 0: CountB% = 0: CountC% = 0: CountU% = 0
                                        'TempC$ = Original PuzHead$ item
                                        i% = 0: FOR Z% = 1 TO 6: i% = INSTR(i% + 1, Temp$, "|"): NEXT Z%
                                        TempC$ = LEFT$(Temp$, i% - 1): TempD$ = MID$(Temp$, i% + 1)
                                        PuzCount% = PuzCount% + 1: PuzHead$(PuzCount%) = TempC$ + "}"
                                        'TempB$ = Extension piece to PuzData$
                                        PuzData$(PuzCount%) = TempD$
                                        IF Progress = 1 THEN LOCATE CSRLIN, CK% - 10: PRINT "Load puzzles:"; LTRIM$(RTRIM$(STR$(PuzCount%))); "/"; LTRIM$(RTRIM$(STR$(DatPuzCount%)));
                                END IF
                        'CASE IS <> "{", "#"'Extract some analysis out of the value
                        '                L% = LEN(Temp$)
                        '                IF L% > 9 THEN
                        '                        K% = INSTR(L% - 4, Temp$, "|")
                        '                        IF K% > 0 THEN
                        '                                UseEndSt$ = MID$(Temp$, K% + 1): SELECT CASE UseEndSt$
                        '                                        CASE "KVC", "KHC", "XVC", "XHC": CountKX% = CountKX% + 1: CountC% = CountC% + 1
                        '                                        CASE "KVU", "KHU", "XVU", "XHU": CountKX% = CountKX% + 1: CountU% = CountU% + 1
                        '                                        CASE "PVC", "PHC": CountP% = CountP% + 1: CountC% = CountC% + 1
                        '                                        CASE "PVU", "PHU": CountP% = CountP% + 1: CountU% = CountU% + 1
                        '                                        CASE "OC": CountB% = CountB% + 1: CountC% = CountC% + 1
                        '                                        CASE "OU": CountB% = CountB% + 1: CountU% = CountU% + 1
                        '                                END SELECT
                        '                        END IF
                        '                END IF
                       
                        END SELECT
                END IF
        END SELECT
END SELECT 'Of Flag%
        IF EOF(FileHandle) THEN CLOSE #FileHandle: EXIT DO
        'IF EOF(FileHandle) THEN CLOSE #FileHandle: CLS : PRINT "FATAL: XYPUZ.DAT Error: EOF"; Flag%: END
LOOP

END SUB

SUB DecorateTable (inPat$)
'Use a simple pattern to fill the empty background
'Patterns should be 1,2 or 4 bytes in size (not 3)
L% = LEN(inPat$): C% = 0
FullLine$ = FillString$(45, inPat$) 'First Create a whole line of the repeating pattern

'Figure out how big the fill is going to be
FOR Y% = 2 TO 24: FOR X% = 2 TO 45
        IF SCREEN(Y%, X%) = 32 THEN
                IF C% = 0 THEN StartX% = X% 'Capture start position
                C% = C% + 1 'Increment Length
        ELSE
                IF C% > 0 THEN LOCATE Y%, StartX%: PRINT MID$(FullLine$, StartX%, C%); : C% = 0 'Print a section
        END IF
        NEXT X%
        IF C% > 0 THEN LOCATE Y%, StartX%: PRINT MID$(FullLine$, StartX%, C%); : C% = 0 'Print to border
NEXT Y%

END SUB

SUB DrawArt (StartX%, StartY%, OverWriteFlag%)
'FileName$ = File to load
'StartX%/StartY% = The starting co-ordinates
'OverwriteFlag% = Look ahead before pasting (slower)

LineCount% = VAL(Art$(0)): RunCount% = 1'Reset Counter
IF LineCount% = 0 THEN EXIT SUB 'Expects LoadArt has already has been run and val(Art$(0)) is >0

DO
        Temp$ = Art$(RunCount%): L% = LEN(Temp$): C% = 0
        IF OverWriteFlag% = 1 THEN
                LOCATE StartY%, StartX%: PRINT Temp$; 'Blast it over existing text
        ELSE  'Use the flag and read ahead
                FOR TX% = StartX% TO StartX% + L%
                IF SCREEN(StartY%, TX%) = 32 THEN
                        IF C% = 0 THEN BeginX% = TX% 'Capture start position
                        C% = C% + 1 'Increment Length
                ELSE
                        IF C% > 0 THEN LOCATE StartY%, BeginX%: PRINT MID$(Temp$, BeginX%, C%); : C% = 0 'Print a section
                END IF
                NEXT TX%
                IF C% > 0 THEN LOCATE StartY%, BeginX%: PRINT MID$(Temp$, BeginX%, C%); : C% = 0 'Print to border
        END IF

StartY% = StartY% + 1: RunCount% = RunCount% + 1: IF RunCount% > LineCount% THEN EXIT DO'Last one?
LOOP
END SUB

SUB DrawBox (StartX%, StartY%, EndX%, EndY%, BoxStyle%)
'Example Draw a bordered box from 15,15 to 60,18 using Style 1
'DrawBox 15, 15, 60, 18, 1 'StartX%,StartY%,EndX%,EndY%,BoxStyle%

'Establish BoxStyle
SELECT CASE BoxStyle%
        CASE 1: BoxUL$ = "Ú": BoxUR$ = "¿": BoxLL$ = "À": BoxLR$ = "Ù": BoxV$ = "³": BoxH$ = "Ä"
        CASE 2: BoxUL$ = "É": BoxUR$ = "»": BoxLL$ = "È": BoxLR$ = "¼": BoxV$ = "º": BoxH$ = "Í"
END SELECT
'Do Corners first
LOCATE StartY%, StartX%: PRINT BoxUL$; : LOCATE EndY%, StartX%: PRINT BoxLL$;
LOCATE StartY%, EndX%: PRINT BoxUR$; : LOCATE EndY%, EndX%: PRINT BoxLR$;
'H Borders
FOR X% = StartX% + 1 TO EndX% - 1: LOCATE StartY%, X%: PRINT BoxH$; : NEXT X%
FOR X% = StartX% + 1 TO EndX% - 1: LOCATE EndY%, X%: PRINT BoxH$; : NEXT X%
'V Borders
FOR Y% = StartY% + 1 TO EndY% - 1: LOCATE Y%, StartX%: PRINT BoxV$; : NEXT Y%
FOR Y% = StartY% + 1 TO EndY% - 1: LOCATE Y%, EndX%: PRINT BoxV$; : NEXT Y%

END SUB

SUB DrawLine (StartX%, StartY%, EndX%, EndY%, LineChar%)
'Very simple repeating character routine
'Draw a line from 10,10 to 15,10 (Horizontal)
'DrawLine 10, 10, 15, 10, 205 'StartX%,StartY%,EndX%,EndY%,ASCVal%

IF StartX% = EndX% THEN 'Vertical
        FOR Y% = StartY% TO EndY%: LOCATE Y%, StartX%: PRINT CHR$(LineChar%); : NEXT Y%
ELSE 'Horizontal
        FOR X% = StartX% TO EndX%: LOCATE StartY%, X%: PRINT CHR$(LineChar%); : NEXT X%
END IF

END SUB

SUB ExitGame
'Call this to shut the game down gracefully
CLOSE : CLS
PRINT "Thank you for playing xyWords Text-Mode Edition": END
END SUB

FUNCTION FillString$ (useLen%, inText$)
Temp$ = ""
DO
Temp$ = Temp$ + inText$: IF LEN(Temp$) >= useLen% THEN EXIT DO
LOOP
FillString$ = LEFT$(Temp$, useLen%)

END FUNCTION

SUB GameSelect (inPass%)

'Use inPass% to skip to certain sections for call/loop back within the game
'CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen Border [double]

'This will show freeplay, collections (based on unlock), statistics and help
REDIM FPMap%(FPCount%) 'Map FPList to PuzHead/data number
REDIM UIList$(128) 'Dynamic array

FOR X% = 1 TO FPCount%
        FOR Y% = 1 TO PuzCount%
                Temp$ = MID$(StrSplit$(1, "|", PuzHead$(Y%)), 2)
                IF FPList$(X%) = Temp$ THEN : FPMap%(X%) = Y%: EXIT FOR
        NEXT Y%
NEXT X%

GameSelect.MainMenu:
ClearLine 2, 11, 79, 24 'Prepare Viewport
PrevSelect% = 1: vSelect% = 1: MenuD% = 48: MenuC% = 5: COLOR 7, 0: DrawBox MenuD% - 10, 11, MenuD% + 29, 24, 2
COLOR 7: LOCATE 15, MenuD% - 4: PRINT "Use Arrow Keys to navigate"
LOCATE 16, MenuD% - 4: PRINT "Use Enter key to select"
LOCATE 11, MenuC%: COLOR 9: PRINT "Game Select"; : COLOR 7: PRINT ": "; StrSplit$(2, "|", PlayerHead$(FocusPlayer%));

LOCATE 13, MenuC%: PRINT "[ ] Free Play a Puzzle";
LOCATE 14, MenuC%: PRINT "[ ] Play a Puzzle Collection";
LOCATE 15, MenuC%: PRINT "[ ] Back to Player Select";

'Lay out basic FocusPlayer% player statistics and info in Rows 19-24
Temp$ = StrSplit$(1, "|", PlayerHead$(FocusPlayer%))
REDIM PlayerCulmStat&(16): FOR X% = 1 TO 4: PlayerCulmStat&(X%) = VAL(SplitWords$(X% + 4)): NEXT X%
LOCATE 19, MenuC%: COLOR 9: PRINT "Created on"; : COLOR 7: PRINT ": "; DateString$(SplitWords$(3), 2);
LOCATE 20, MenuC%: COLOR 9: PRINT "Last played"; : COLOR 7: PRINT ": "; DateString$(SplitWords$(4), 2);
LOCATE 21, MenuC%: COLOR 9: PRINT "Play time"; : COLOR 7: PRINT ": "; BigTime$(PlayerCulmStat&(1));
LOCATE 22, MenuC%: COLOR 9: PRINT "Found a word"; : COLOR 7: PRINT ":"; PlayerCulmStat&(2);
LOCATE 23, MenuC%: COLOR 9: PRINT "Completed a Puzzle"; : COLOR 7: PRINT ":"; PlayerCulmStat&(3);

GameSelect.vSelect:
IF vSelect% < 1 THEN vSelect% = 3 'Move from top to bottom
IF vSelect% > 3 THEN vSelect% = 1 'Move from bottom to top

COLOR 7: LOCATE 12 + vSelect%, MenuC% + 1: PRINT CHR$(16); 'Higlight the vSelect% selection
IF vSelect% <> PrevSelect% THEN LOCATE 12 + PrevSelect%, MenuC% + 1: PRINT " "; 'Clear the previous selection

GameSelect.MainKeySelect:
SELECT CASE GetKey$
        CASE CHR$(13) 'Select menu item and display sub-menu
                SELECT CASE vSelect%
                        CASE 1: GOTO GameSelect.FreePlay: EXIT SUB
                        CASE 2: GOTO GameSelect.CollPlay: EXIT SUB
                        CASE 3: CollSelect% = 0: PuzSelect% = 0: EXIT SUB 'Quit
                END SELECT
        CASE CHR$(0) + CHR$(72), "W", "w", "8": PrevSelect% = vSelect%: vSelect% = vSelect% - 1: GOTO GameSelect.vSelect 'Up
        CASE CHR$(0) + CHR$(80), "S", "s", "2": PrevSelect% = vSelect%: vSelect% = vSelect% + 1: GOTO GameSelect.vSelect 'Down
        CASE CHR$(27): CollSelect% = 0: PuzSelect% = 0: EXIT SUB  'Quit
END SELECT
GOTO GameSelect.MainKeySelect 'LOOP

GameSelect.FreePlay: 'Display and select a freeplay puzzle
LOCATE 11, MenuD% - 8: PRINT "þSelect a puzzleþ"; : REDIM UIList$(FPCount% * 24): COLOR 7
FOR X% = 1 TO FPCount% 'Generate the UI List
        Temp$ = StrSplit$(1, "|", MID$(PuzHead$(FPMap%(X%)), 2, LEN(PuzHead$(FPMap%(X%))) - 2)): L% = LEN(Temp$): BestTime$ = ""
        FOR Y% = 1 TO VAL(FocusPlayerStat$(0))
                IF LEN(FocusPlayerStat$(Y%)) > L% THEN
                        IF LEFT$(FocusPlayerStat$(Y%), L%) = Temp$ THEN : BestTime$ = "Best: " + TimeString$(VAL(StrSplit$(4, "|", FocusPlayerStat$(Y%)))): EXIT FOR
                END IF
        NEXT Y%
        UIList$(X%) = "[ ] " + Temp$ + " (" + GetDifficulty$(VAL(SplitWords$(2))) + ") " + BestTime$
NEXT X%

'Display UIList with a Start and End view - Use i rather than v for menu as this is a submenu
iMax% = 9: iPages% = IntUP%(FPCount%, iMax%): iPage% = 1'Init

GameSelect.FP.Refresh:
iPrev% = 1: iSelect% = 1: ClearLine MenuD% - 9, 12, MenuD% + 27, 23'Clear a 27x11 viewport and change selection to 1st

GameSelect.FP.ShowUI:
'Figure out selection of UIList$() that will be displayed
C% = 0: iStart% = (iPage% - 1) * iMax%: IF iStart% = 0 THEN iStart% = 1
IF iPage% > 1 THEN iStart% = iStart% + 1 'Increment by 1 if we're on the second page otherwise show overlap
iEnd% = iStart% + iMax% - 1: IF iEnd% > FPCount% THEN iEnd% = FPCount% 'Trim vEnd% if needed
FOR X% = iStart% TO iEnd%: iFocus% = iStart% + C%: C% = C% + 1: LOCATE 12 + C%, MenuD% - 8: PRINT UIList$(iFocus%); : NEXT X%

IF iPages% > 1 THEN LOCATE 23, MenuD% + 2: PRINT "[PGUP/DN] Page:"; iPage%; "/"; iPages%; 'Update page detail
LOCATE 24, MenuD% - 7: PRINT "Í"; CHR$(18); " Select"; "ÍÍÍ"; CHR$(17); "Ù Play!ÍÍÍESC Back"; 'Button UI

GameSelect.FP.Select:
LOCATE 12 + iSelect%, MenuD% - 7: PRINT CHR$(16);
IF iPrev% <> iSelect% THEN LOCATE 12 + iPrev%, MenuD% - 7: PRINT " "; 'clear old value

GameSelect.FP.GetKey:
SELECT CASE GetKey$
        CASE CHR$(27): GOTO GameSelect.MainMenu
        CASE CHR$(0) + CHR$(73): IF iPage% > 1 THEN iPage% = iPage% - 1: GOTO GameSelect.FP.Refresh 'Back a page
        CASE CHR$(0) + CHR$(81): IF iPage% < iPages% THEN iPage% = iPage% + 1: GOTO GameSelect.FP.Refresh 'Forward a page
        CASE CHR$(0) + CHR$(72): IF iSelect% > 1 THEN iPrev% = iSelect%: iSelect% = iSelect% - 1: GOTO GameSelect.FP.Select 'UP
        CASE CHR$(0) + CHR$(80): IF iSelect% < C% THEN iPrev% = iSelect%: iSelect% = iSelect% + 1: GOTO GameSelect.FP.Select 'DOWN
        CASE CHR$(13): CollSelect% = 0: PuzSelect% = FPMap%(iStart% + iSelect% - 1): EXIT SUB 'EXECUTE!
END SELECT
GOTO GameSelect.FP.ShowUI

GameSelect.CollPlay: 'Display and select a collection
'Collection Name, Collection Author, Collection Graphic, Completed X of Y (%)
COLOR 7: LOCATE 11, MenuD% - 8: PRINT STRING$(27, "Í"); 'Redraw box header
COLOR 7: LOCATE 11, MenuD% - 1: PRINT "þChoose a Collectionþ";
LOCATE 24, MenuD% - 7: PRINT "Í"; CHR$(18); " Select"; "ÍÍÍ"; CHR$(17); "Ù ChooseÍÍÍESC Back";
iStart% = 1: iPrev% = 1: iSelect% = 1: iMax% = 9: iPages% = IntUP%(CollCount%, iMax%): iPage% = 1 'Reset flags

FOR X% = 1 TO CollCount% 'Build UIList$()
        UIList$(X%) = MID$(CollHead$(X%), 2, LEN(CollHead$(X%)) - 2) + "|" + CollData$(X%) + "|" + RTRIM$(LTRIM$(STR$(LEN(GetUnlock$(X%)))))
        Temp$ = StrSplit$(1, "|", UIList$(X%)): TempPCT% = INT(VAL(SplitWords$(5)) / VAL(SplitWords$(4)) * 100)
        UIList$(X%) = Temp$ + " - " + LTRIM$(RTRIM$(STR$(TempPCT%))) + "% Complete "'; SplitWords$(5); "/"+ SplitWords$(4)
NEXT X%

GameSelect.ShowUI.Refresh:  'Refresh the viewport
IF iPages% > 1 THEN LOCATE 23, MenuD% + 2: PRINT "[PGUP/DN] Page:"; iPage%; "/"; iPages%;  'Update page detail
ClearLine MenuD% - 9, 12, MenuD% + 27, 22: CX% = 1: iStart% = ((iPage% - 1) * iMax%) + 1
iEnd% = iStart% + (iMax% - 1): IF iEnd% > CollCount% THEN iEnd% = CollCount%
FOR X% = iStart% TO iEnd%: LOCATE 12 + CX%, MenuD% - 8: PRINT "[ ] "; UIList$(X%); STRING$(36 - (LEN(UIList$(X%)) + 3), " "); : CX% = CX% + 1: NEXT X%

GameSelect.ShowUI.Select: 'Position cursor
LOCATE 12 + iSelect%, MenuD% - 7: PRINT CHR$(16);
IF iSelect% <> iPrev% THEN LOCATE 12 + iPrev%, MenuD% - 7: PRINT " ";

GameSelect.ShowUI.GetKey: 'GetKey loop
SELECT CASE GetKey$
        CASE CHR$(27): GOTO GameSelect.MainMenu
        CASE CHR$(0) + CHR$(73): IF iPage% > 1 THEN iPage% = iPage% - 1: iSelect% = 1: GOTO GameSelect.ShowUI.Refresh'Back a page
        CASE CHR$(0) + CHR$(81): IF iPage% < iPages% THEN iPage% = iPage% + 1: iSelect% = 1: GOTO GameSelect.ShowUI.Refresh'Forward a page
        CASE CHR$(0) + CHR$(72): IF iSelect% > 1 THEN iPrev% = iSelect%: iSelect% = iSelect% - 1: GOTO GameSelect.ShowUI.Select 'UP
        CASE CHR$(0) + CHR$(80): IF iSelect% < (iEnd% - iStart% + 1) THEN iPrev% = iSelect%: iSelect% = iSelect% + 1: GOTO GameSelect.ShowUI.Select'DOWN
        CASE CHR$(13): CollSelect% = iStart% + iSelect% - 1: GOTO GameSelect.ColPuz.Menu'Load in the collection and go to collpuz sub menu
END SELECT
GOTO GameSelect.ShowUI.GetKey

GameSelect.ColPuz.Menu:    'Display Collection > Puzzle List
FocusCollProg$ = "": FocusCollPuz% = 0: ClearLine MenuD% - 9, 12, MenuD% + 28, 23: FocusCollName$ = StrSplit$(1, "|", MID$(CollHead$(CollSelect%), 2))
LOCATE 11, MenuD% - 5: PRINT "("; FocusCollName$; "): Select Puzzle"; STRING$(77 - POS(CSRLN), "Í"); : LoadColl CollSelect%
FOR X% = 1 TO VAL(FocusPlayerProg$(0))
        IF StrSplit$(1, "|", FocusPlayerProg$(X%)) = FocusCollName$ THEN FocusCollProg$ = SplitWords$(2): EXIT FOR
NEXT X%

Locker$ = " ": FOR X% = 1 TO FocusCollCount% 'Generate Lock State
        IF X% > LEN(FocusCollProg$) + 1 THEN Locker$ = "X" 'allow play of previously completed puzzles plus next in line
        UIList$(X%) = "[" + Locker$ + "] " + StrSplit$(1, "|", FocusColl$(X%))
        IF Locker$ = "X" THEN UIList$(X%) = UIList$(X%) + " (Lock)"
        IF Locker$ <> "X" AND X% <= LEN(FocusCollProg$) THEN
                UIList$(X%) = UIList$(X%) + " (Done)" 'Default if no prog flag
        END IF
        Tail$ = "": Temp$ = SplitWords$(1): L% = LEN(Temp$)
        FOR Y% = 1 TO VAL(FocusPlayerStat$(0))
                IF LEN(FocusPlayerStat$(Y%)) > L% THEN
                        IF LEFT$(FocusPlayerStat$(Y%), L%) = Temp$ THEN : Tail$ = " Best: " + TimeString$(VAL(StrSplit$(4, "|", FocusPlayerStat$(Y%)))): EXIT FOR
                END IF
        NEXT Y%
        UIList$(X%) = UIList$(X%) + Tail$
NEXT X%

'Display the UIList
OKScroll% = 0: iStart% = 1: iPrev% = 1: iSelect% = 1: iMax% = 9: IF iMax% > FocusCollCount% THEN iMax% = FocusCollCount%:
iPages% = IntUP%(FocusCollCount%, iMax%): iPage% = 1'Reset flags
GameSelect.ColPuz.List:
ClearLine MenuD% - 9, 12, MenuD% + 28, 23: FocusCollName$ = StrSplit$(1, "|", MID$(CollHead$(CollSelect%), 2))
IF iPages% > 1 THEN LOCATE 23, MenuD% + 2: PRINT "[PGUP/DN] Page:"; iPage%; "/"; iPages%;  'Update page detail
CX% = 1: iStart% = (iPage% - 1) * iMax% + 1: IF iPage% > 1 THEN iStart% = iStart% + 1
iEnd% = iStart% + iMax% - 1: IF iEnd% > FocusCollCount% THEN iEnd% = FocusCollCount%
FOR X% = iStart% TO iEnd%
LOCATE 12 + CX%, MenuD% - 8: PRINT UIList$(X%);
CX% = CX% + 1
NEXT X%

GameSelect.ColPuz.Sel:
IF iStart% + iSelect% - 2 <= LEN(FocusCollProg$) THEN
        LOCATE 12 + iSelect%, MenuD% - 7: PRINT CHR$(16);
        IF iPrev% <> iSelect% AND iPrev% - 2 < LEN(FocusCollProg%) THEN LOCATE 12 + iPrev%, MenuD% - 7: PRINT " ";
END IF

SELECT CASE GetKey$
        CASE CHR$(27): GOTO GameSelect.CollPlay
        CASE CHR$(13) 'Check then submit
        IF iStart% + iSelect% - 2 <= LEN(FocusCollProg$) THEN
                'Find the puzzle # for the selected item
                CLS
                FOR X% = 1 TO PuzCount%
                        Temp$ = StrSplit$(1, "|", MID$(PuzHead$(X%), 2))
                        SearchFor$ = StrSplit$(1, "|", FocusColl$(iStart% + iSelect% - 1))
                        IF SearchFor$ = Temp$ THEN
                                FocusCollPuz% = iStart% + iSelect% - 1 'set the increment
                                PuzSelect% = X%: EXIT SUB 'set the puzzle id
                        END IF
                NEXT X%
               
        END IF
        CASE CHR$(0) + CHR$(73) 'PGUP
                IF iPage% > 1 THEN iPage% = iPage% - 1: iSelect% = 1: GOTO GameSelect.ColPuz.List
        CASE CHR$(0) + CHR$(81) 'PGDN
                IF iPage% < iPages% THEN iPage% = iPage% + 1: iSelect% = 1: GOTO GameSelect.ColPuz.List
        CASE CHR$(0) + CHR$(72) 'UP
                IF iSelect% > 1 THEN : iPrev% = iSelect%: iSelect% = iSelect% - 1: LOCATE 12 + iPrev%, MenuD% - 7: PRINT " "; : GOTO GameSelect.ColPuz.Sel
        CASE CHR$(0) + CHR$(80) 'DN
                IF iSelect% + iStart% - 2 < LEN(FocusCollProg$) AND iSelect% + iStart% <= iEnd% THEN
                        iPrev% = iSelect%: LOCATE 12 + iPrev%, MenuD% - 7: PRINT " "; : iSelect% = iSelect% + 1: GOTO GameSelect.ColPuz.Sel
                END IF
END SELECT
GOTO GameSelect.ColPuz.Sel

END SUB

FUNCTION GetDifficulty$ (inVal%)
'Return a difficulty rating based on inVal%
'This needs to be more complex later..

SELECT CASE inVal%
        CASE IS < 25: GetDifficulty$ = "Easy"
        CASE IS > 24 < 66: GetDifficulty$ = "Normal"
        CASE IS > 65: GetDifficulty$ = "Hard"
END SELECT

END FUNCTION

FUNCTION GetKey$
'Waits until a key is pressed and returns the string
        i$ = INKEY$
        DO
        LET i$ = INKEY$
        LOOP WHILE i$ = ""
        GetKey$ = i$
END FUNCTION

FUNCTION GetMem$
        'Return a quick scrape of QBs memory stats
        GetMem$ = "$" + LTRIM$(STR$(FRE(""))) + " S" + LTRIM$(STR$(FRE(-2))) + " A" + LTRIM$(STR$(FRE(-1)))
END FUNCTION

SUB GetScreen (AreaStartX%, AreaStartY%, AreaEndX%, AreaEndY%)
'GetScreen primative, reads text on screen 1x1
'Does not capture color properties as this is useless due to PutScreen primative
'Should replace this with a PEEK routine

V% = ((AreaEndX% - AreaStartX%) + 1) * ((AreaEndY% - AreaStartY%) + 1)
REDIM TextHold$(V%)

FOR Y% = AreaStartY% TO AreaEndY%
        FOR X% = AreaStartX% TO AreaEndX%: TextHold$(Y%) = TextHold$(Y%) + CHR$(SCREEN(Y%, X%)): NEXT X%
NEXT Y%

END SUB

FUNCTION GetTimeStamp$
        GetTimeStamp$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2)
END FUNCTION

FUNCTION GetUnlock$ (inX%)
'Return the unlock code for a collection for the current FocusPlayer
LookupDat$ = MID$(StrSplit$(1, "|", CollHead$(inX%)), 2)
FOR X% = 1 TO VAL(FocusPlayerProg$(0))
        IF LookupDat$ = StrSplit$(1, "|", FocusPlayerProg$(X%)) THEN
                GetUnlock$ = SplitWords$(2)
                EXIT FUNCTION
        END IF
NEXT X%
END FUNCTION

SUB HelpUI (inVal%)
'In-puzzle Help and Info UI
'Show Key Help or a conditional Word Bag view

SELECT CASE inVal%
CASE 0 'Key Help
        ClearLine 64, 12, 79, 13: ERASE UIList$
        ClearLine 64, 15, 79, 24: LOCATE 12, 64: COLOR 0, 7: PRINT "Key Help"; : COLOR 15, 0: IF PuzBagWordsTotal% > 0 THEN LOCATE 13, 64: COLOR 15: PRINT "F2-"; : COLOR 7: PRINT " Word Bag";
        LOCATE 15, 64: PRINT StrSplit$(2, "|", PlayerHead$(FocusPlayer%)); : LOCATE 17, 64: COLOR 15: PRINT "<--"; : COLOR 7: PRINT " Backspace";
        LOCATE 18, 64: COLOR 15: PRINT "<Ù"; : COLOR 7: PRINT " Submit Word"; : LOCATE 19, 64: COLOR 15: PRINT CHR$(24); : COLOR 7: PRINT " Shuffle";
        LOCATE 21, 64: COLOR 15: PRINT CHR$(27); CHR$(26); : COLOR 7: PRINT " Rotate"; : LOCATE 22, 64: COLOR 15: PRINT "SPACE"; : COLOR 7: PRINT " Select";
        SELECT CASE EnableSound%
                CASE 1: LOCATE 23, 64: COLOR 15: PRINT "F4"; : COLOR 7: PRINT " Mute Sound";
                CASE 0: LOCATE 23, 64: COLOR 15: PRINT "F4"; : COLOR 7: PRINT " Enable Sound";
        END SELECT
        HelpUIView% = 0: LOCATE 24, 64: COLOR 15: PRINT "ESC"; : COLOR 7: PRINT " Quit!";
CASE 1 'Word Bag - show last page
        ClearLine 64, 12, 79, 13: HelpUIView% = 1
        REDIM UIList$(64): ClearLine 64, 15, 79, 24: CX% = 1: LOCATE 12, 64: COLOR 15, 0: PRINT "F1-"; : COLOR 7: PRINT " Key Help"; : LOCATE 13, 64: COLOR 0, 7: PRINT "Word Bag"; : COLOR 7, 0
        IF PuzBagWordsCount% = 0 THEN LOCATE 15, 64: PRINT "Nothing found!"; : EXIT SUB
        FOR X% = 1 TO FocusPuzCount%
                IF FocusPuzF%(X%) = 1 THEN 'is it an OWord?
                        IF DoType%(X%) = -1 THEN
                                L% = LEN(FocusPuzWL$(X%))
                                IF LEN(UIList$(CX%)) + L% > 15 THEN
                                        CX% = CX% + 1: UIList$(CX%) = FocusPuzWL$(X%) + " "
                                ELSE
                                        UIList$(CX%) = UIList$(CX%) + FocusPuzWL$(X%) + " "
                                END IF
                        END IF
                END IF
        NEXT X%
        'Paginate the word bag, set HelpUIPage% to HelpUIPages%
        HelpUIPages% = IntUP%(CX%, 8): HelpUIPage% = HelpUIPages%: StartCX% = ((HelpUIPage% - 1) * 8) + 1: EndCX% = StartCX% + 8: IF EndCX% > CX% THEN EndCX% = CX%
        FOR X% = StartCX% TO EndCX%: LOCATE 14 + X%, 64: PRINT UIList$(X%); : NEXT X%
        IF HelpUIPages% > 1 THEN : LOCATE 23, 64: PRINT "PGUP/DN - Page"; : LOCATE 24, 64: PRINT HelpUIPage%; "of"; HelpUIPages%;
END SELECT
END SUB

FUNCTION IntUP% (inVal%, inDiv%)
        'Divide with roundup
        IntUP% = (inVal% + inDiv% - 1) \ inDiv%
END FUNCTION

FUNCTION JumbleString$ (inTxt$)
'Jumble a string based on the Fisher-Yates/Knuth shuffle approach

RANDOMIZE TIMER
FOR i% = 1 TO (LEN(inTxt$) * 3) '3 Shuffle iterations
        j% = INT(RND * LEN(inTxt$) + 1): K% = INT(RND * LEN(inTxt$) + 1)
        Temp$ = MID$(inTxt$, j%, 1): MID$(inTxt$, j%, 1) = MID$(inTxt$, K%, 1)
        MID$(inTxt$, K%, 1) = Temp$: NEXT i%
JumbleString$ = inTxt$
END FUNCTION

SUB LetterWheel
'Paint the letter wheel and plot letters as per PuzWheel$
'Needs optimisation, 7&6 and 5&4 can be combined and steps can be skipped for a repaint call (shuffle/rotate)
LWCol% = 50

SELECT CASE LEN(PuzWheel$) 'Dynamic Sized letter wheel
CASE 7 'Full Size
        COLOR 7, 0: LOCATE 15, LWCol% + 3: PRINT "ÜÜÜ"; '1
        COLOR 0, 7: LOCATE 16, LWCol% + 1: PRINT "ß  "; LEFT$(PuzWheel$, 1); "  ß";  '2
        LOCATE 17, LWCol%: PRINT "  "; RIGHT$(PuzWheel$, 1); "   "; MID$(PuzWheel$, 2, 1); "  ";
        LOCATE 18, LWCol%: PRINT "  "; MID$(PuzWheel$, 6, 1); "   "; MID$(PuzWheel$, 3, 1); "  ";
        LOCATE 19, LWCol% + 1: PRINT "Ü "; MID$(PuzWheel$, 5, 1); " "; MID$(PuzWheel$, 4, 1); " Ü";
        COLOR 7, 0: LOCATE 20, LWCol% + 3: PRINT "ßßß";
CASE 6 'Full Size, Shift
        COLOR 7, 0: LOCATE 15, LWCol% + 3: PRINT "ÜÜÜ"; '1
        COLOR 0, 7: LOCATE 16, LWCol% + 1: PRINT "ß  "; LEFT$(PuzWheel$, 1); "  ß";  '2
        LOCATE 17, LWCol%: PRINT "  "; RIGHT$(PuzWheel$, 1); "   "; MID$(PuzWheel$, 2, 1); "  ";
        LOCATE 18, LWCol%: PRINT "  "; MID$(PuzWheel$, 5, 1); "   "; MID$(PuzWheel$, 3, 1); "  ";
        LOCATE 19, LWCol% + 1: PRINT "Ü  "; MID$(PuzWheel$, 4, 1); "  Ü";
        COLOR 7, 0: LOCATE 20, LWCol% + 3: PRINT "ßßß";
CASE 5 'Small Size
        COLOR 7, 0: LOCATE 15, LWCol% + 3: PRINT "ÜÜÜ"; '1
        COLOR 0, 7: LOCATE 16, LWCol% + 1: PRINT "ß  "; LEFT$(PuzWheel$, 1); "  ß";  '2
        LOCATE 17, LWCol% + 1: PRINT " "; RIGHT$(PuzWheel$, 1); "   "; MID$(PuzWheel$, 2, 1); " ";
        LOCATE 18, LWCol% + 1: PRINT "Ü "; MID$(PuzWheel$, 4, 1); " "; MID$(PuzWheel$, 3, 1); " Ü";
        COLOR 7, 0: LOCATE 19, LWCol% + 3: PRINT "ßßß";
CASE 4 'Small Size, Shift
        COLOR 7, 0: LOCATE 15, LWCol% + 3: PRINT "ÜÜÜ"; '1
        COLOR 0, 7: LOCATE 16, LWCol% + 1: PRINT "ß  "; LEFT$(PuzWheel$, 1); "  ß";  '2
        LOCATE 17, LWCol% + 1: PRINT " "; RIGHT$(PuzWheel$, 1); "   "; MID$(PuzWheel$, 2, 1); " ";
        LOCATE 18, LWCol% + 1: PRINT "Ü  "; MID$(PuzWheel$, 3, 1); "  Ü";
        COLOR 7, 0: LOCATE 19, LWCol% + 3: PRINT "ßßß";
END SELECT

END SUB

SUB LoadArt (Header$)
'Load ASCII art into Art$
REDIM Art$(1024) 'set to 1K buffer
Footer$ = "[/" + RIGHT$(Header$, LEN(Header$) - 1)
F = FREEFILE: LineCount% = 0
OPEN "GRAPHICS.DAT" FOR INPUT AS #F
'Seek to the header$
DO
LINE INPUT #F, Test$
IF Test$ = Header$ THEN EXIT DO
IF EOF(F) THEN : CLOSE #F: EXIT SUB'Error
LOOP


IF ProgFlag% = 0 THEN 'Load whole file in first
DO
LineCount% = LineCount% + 1:  LINE INPUT #F, Art$(LineCount%)
IF EOF(F) THEN : CLOSE #F: EXIT DO
IF Art$(LineCount%) = Footer$ THEN : LineCount% = LineCount% - 1: EXIT DO
LOOP
Art$(0) = LTRIM$(RTRIM$(STR$(LineCount%)))
CLOSE #F
END IF

END SUB

SUB LoadColl (useCol%)
'Load entire contents (Except for the line captured in CollHead$(useCol%))

'Load The puzzle - note: no error checking here..
FileName$ = "xypuz.dat": F = FREEFILE: Flag% = 0: FocusCollCount% = 0
UseTail$ = "{/" + MID$(StrSplit$(1, "|", CollHead$(useCol%)), 2) + "}"
OPEN FileName$ FOR INPUT AS #F
DO
        LINE INPUT #F, Temp$
        SELECT CASE Flag%
                CASE 0: IF Temp$ = CollHead$(useCol%) THEN Flag% = 1 'Search
              
                CASE 1 'Load Entries
                IF Temp$ = UseTail$ THEN
                         EXIT DO 'Finished
                ELSE
                        IF INSTR(1, Temp$, "|") <> 0 THEN : FocusCollCount% = FocusCollCount% + 1: FocusColl$(FocusCollCount%) = Temp$
                END IF
        END SELECT
        IF EOF(F) THEN EXIT DO
LOOP
CLOSE #F

END SUB

SUB LoadPuzzle
'Reset a bunch of game variables
PuzKeyWord$ = "": PuzBuffer$ = "": PuzTimeCount% = 0: PuzInvCount% = 0: PuzBagWordsCount% = 0: PuzBoardWordsCount% = 0: PuzKeyWordTime% = 0: PuzUWordCount% = 0: PuzCWordCount% = 0
REDIM FocusPuz$(100)

SELECT CASE CollSelect% 'Set UseHeader$ and find it in [PUZZLES]
        CASE 0: UseHeader$ = FPList$(PuzSelect%) 'Selected from FreePlay List
        CASE IS >= 1
        LoadColl CollSelect%
        UseHeader$ = StrSplit$(1, "|", MID$(PuzHead$(PuzSelect%), 2))'Collection
END SELECT

'Load The puzzle - note: no error checking here..
FileName$ = "xypuz.dat": F = FREEFILE: Flag% = 0: SLen% = 1 + LEN(UseHeader$): FocusPuzCount% = 0
OPEN FileName$ FOR INPUT AS #F
DO
        LINE INPUT #F, Temp$
        SELECT CASE Flag%
                CASE 0: IF Temp$ = "[PUZZLES]" THEN Flag% = 1 'Not found PUZZLES yet
              
                CASE 1 'Look for the puzzle
                IF LEN(Temp$) > SLen% THEN
                        IF LEFT$(Temp$, SLen%) = "{" + UseHeader$ THEN Flag% = 2: LCount% = 0
                END IF

                CASE 2 'Load Entries
                IF Temp$ = "{/" + UseHeader$ + "}" THEN
                         EXIT DO 'Finished
                ELSE
                        SELECT CASE LCount% 'What line are we in?
                        CASE 2: pMDASettings$ = Temp$ '3rd line in puzzle contains pMDA Setting
                        CASE IS > 2 'We're past the config lines, load the puzzle!
                                 FocusPuzCount% = FocusPuzCount% + 1: FocusPuz$(FocusPuzCount%) = Temp$
                        END SELECT
                        LCount% = LCount% + 1
                END IF
        END SELECT
        IF EOF(F) THEN EXIT DO
LOOP
CLOSE #F
END SUB

SUB PlayerInit (FileHandle)
'Load in the player.dat file - ready for future PlayerSelect function
'Populate PlayerHead$() and set a count in PlayerHead$(0)
REDIM PlayerHead$(16)
PlayerHead$(0) = "0" 'reset count
DO
        LINE INPUT #FileHandle, Temp$
        IF LEN(Temp$) > 1 THEN
                IF LEFT$(Temp$, 1) = "[" AND LEFT$(Temp$, 2) <> "[/" THEN : V% = VAL(PlayerHead$(0)) + 1: PlayerHead$(0) = LTRIM$(RTRIM$((STR$(V%)))): PlayerHead$(V%) = Temp$'Grab and increment
        END IF
IF EOF(FileHandle) THEN EXIT DO
LOOP
CLOSE #FileHandle
END SUB

SUB PuzRecord
'PuzRecord: DDMMYYYYHHMM, PuzzleTime$, PuzzleWPM, KeyTime, Invalid, Bag
PuzRecordChange$ = "" 'reset records
SELECT CASE CollSelect% 'Get Puzzle Name
        CASE 0: inText$ = FPList$(PuzSelect%)'text name of the FP
        CASE IS > 0: inText$ = StrSplit$(1, "|", FocusColl$(FocusCollPuz%))'Text of the puzzle in use

END SELECT
V% = VAL(FocusPlayerStat$(0)): Flag% = V% + 1: IF V% = 0 THEN GOTO PR.SkipSearch 'Default to write a new record

FOR X% = 1 TO V% 'Search for it
        IF inText$ = StrSplit$(1, "|", FocusPlayerStat$(X%)) THEN Flag% = X%: EXIT FOR
NEXT X%

PR.SkipSearch:
UseTimeString$ = GetTimeStamp$ 'Basic 12 byte string for DDMMYYYYhhmm
K% = IntUP((((PuzBagWordsCount% + PuzBoardWordsCount%) / PuzTimeCount%) * 60), 1)

SELECT CASE FocusPlayerStat$(Flag%)

CASE "" 'new record just write it
        FocusPlayerStat$(Flag%) = inText$ + "|1|" + UseTimeString$ + "|" + LTRIM$(RTRIM$(STR$(PuzTimeCount%)))
        FocusPlayerStat$(Flag%) = FocusPlayerStat$(Flag%) + "|" + LTRIM$(RTRIM$(STR$(K%)))
        FocusPlayerStat$(Flag%) = FocusPlayerStat$(Flag%) + "|" + LTRIM$(RTRIM$(STR$(PuzKeyWordTime%))) + "|" + LTRIM$(RTRIM$(STR$(PuzInvCount%))) + "|" + LTRIM$(RTRIM$(STR$(PuzBagWordsCount%)))
        FocusPlayerStat$(0) = LTRIM$(RTRIM$(STR$(Flag%)))

CASE IS <> "" 'rewrite record
        Temp$ = StrSplit$(1, "|", FocusPlayerStat$(Flag%))
        IF Temp$ = inText$ THEN 'Review and Update
                'Always Update
                V% = VAL(SplitWords$(2)) + 1
                SplitWords$(2) = LTRIM$(RTRIM$(STR$(V%))): SplitWords$(3) = UseTimeString$
                'Evaluate then Update
                IF PuzTimeCount% < VAL(SplitWords$(4)) THEN : SplitWords$(4) = LTRIM$(RTRIM$(STR$(PuzTimeCount%))): PuzRecordChange$ = PuzRecordChange$ + "T"
                IF K% > VAL(SplitWords$(5)) THEN : SplitWords$(5) = LTRIM$(RTRIM$(STR$(K%))): PuzRecordChange$ = PuzRecordChange$ + "W"
                IF PuzInvCount% < VAL(SplitWords$(7)) THEN : SplitWords$(7) = LTRIM$(RTRIM$(STR$(PuzInvCount%))): PuzRecordChange$ = PuzRecordChange$ + "I"
                IF PuzBagWordsCount% > VAL(SplitWords$(8)) THEN : SplitWords$(8) = LTRIM$(RTRIM$(STR$(PuzBagWordsCount%))): PuzRecordChange$ = PuzRecordChange$ + "B"
                'Never update: (6) Keyword time does not get overridden. Initial record stands.
                OldStat$ = FocusPlayerStat$(Flag%): FocusPlayerStat$(Flag%) = SplitWords$(1)
                FOR X% = 2 TO 8: FocusPlayerStat$(Flag%) = FocusPlayerStat$(Flag%) + "|" + SplitWords$(X%): NEXT X%
        END IF
END SELECT

SavePlayer 'Update PLAYER.DAT

END SUB

SUB SelectPlayer (UseID%)
'SELECT CASE SkipFlag%
'CASE 1: GOTO SelectPlayer.UI
'END SELECT

CLS : xyBannerDraw 2, 19: DrawBox 1, 1, 80, 25, 2'Draw Screen
REDIM FocusPlayerProg$(32)
REDIM FocusPlayerStat$(64)
SelectPlayer.UI:
'Enumerate FocusPlayerProg$() and FocusPlayerStat$()
PEnd$ = "[/P" + LTRIM$(STR$(UseID%)) + "]": PStr$ = "[P" + LTRIM$(STR$(UseID%)): Flag% = 0: FocusPlayerProg$(0) = "0": FocusPlayerStat$(0) = "0"'Reset counters

'No error checking here, it was done previously
FileHandle = FREEFILE: OPEN "PLAYER.DAT" FOR INPUT AS #FileHandle

DO
LINE INPUT #FileHandle, Temp$: IF Temp$ = "" THEN GOTO playerinit.skipper 'skip blank lines
SELECT CASE Flag%
        CASE 0 'Search for a flag
        IF LEN(Temp$) >= LEN(PStr$) THEN
                IF LEFT$(Temp$, LEN(PStr$)) = PStr$ THEN
                        Flag% = 1 'Found it
                END IF
        END IF
        CASE 1 'In context of the player view
        SELECT CASE Temp$
                CASE "{COLLECTIONS}": Flag% = 2: C% = 0
                CASE "{PUZZLES}": Flag% = 3: C% = 0
        END SELECT
        IF Temp$ = PEnd$ THEN EXIT DO
        CASE 2 'Parse a collection value
        SELECT CASE Temp$
                CASE IS <> "{/COLLECTIONS}": C% = C% + 1: FocusPlayerProg$(C%) = Temp$'Grab and increment counter
                CASE "{/COLLECTIONS}": Flag% = 1: FocusPlayerProg$(0) = LTRIM$(STR$(C%)) 'Set the count in (0)
        END SELECT
        CASE 3 'Parse a puzzle value
        SELECT CASE Temp$
                CASE IS <> "{/PUZZLES}": C% = C% + 1: FocusPlayerStat$(C%) = Temp$   'Grab and increment counter
                CASE "{/PUZZLES}": Flag% = 1: FocusPlayerStat$(0) = LTRIM$(STR$(C%)) 'Set the count in (0)
        END SELECT
END SELECT

playerinit.skipper:
IF EOF(FileHandle) THEN EXIT DO

LOOP
CLOSE #FileHandle
END SUB

SUB ShowPuzzle
'Plots the masked puzzle. Expects a playfield UI is ready.

'Render Word$,Type%,Orient%,X%,Y%,Mask=1
REDIM DoType%(FocusPuzCount% + 2), DoOrient%(FocusPuzCount% + 2), UseX%(FocusPuzCount% + 2), UseY%(FocusPuzCount% + 2), DoCommon%(FocusPuzCount% + 2)
REDIM FocusPuzWL$(FocusPuzCount% * 7)
REDIM FocusPuzF%(FocusPuzCount% + 2)
FOR uX% = 1 TO FocusPuzCount%
        DoWordType$ = StrSplit$(3, "|", FocusPuz$(uX%))
        FocusPuzWL$(uX%) = SplitWords$(1) 'Add to puzzle word list - faster than splitting each time
        FocusPuzF%(uX%) = 0 'Reset
        DoWordLoc$ = SplitWords$(2): UseY%(uX%) = VAL(StrSplit$(1, ",", DoWordLoc$)): UseX%(uX%) = VAL(StrSplit$(2, ",", DoWordLoc$))
        SELECT CASE DoWordType$
                CASE "KVU", "KVC": DoOrient%(uX%) = 1: DoType%(uX%) = 2: PuzKeyWord$ = FocusPuzWL$(uX%)
                CASE "XVU", "XVC": DoOrient%(uX%) = 1: DoType%(uX%) = 1
                CASE "KHU", "KHC": DoOrient%(uX%) = 0: DoType%(uX%) = 2: PuzKeyWord$ = FocusPuzWL$(uX%)
                CASE "XHU", "XHC": DoOrient%(uX%) = 0: DoType%(uX%) = 1
                CASE "PHU", "PHC": DoOrient%(uX%) = 0: DoType%(uX%) = 0
                CASE "PVU", "PVC": DoOrient%(uX%) = 1: DoType%(uX%) = 0
                CASE "OU", "OC": DoOrient%(uX%) = -1: DoType%(uX%) = -1
        END SELECT
        DoCommon%(uX%) = 0: IF INSTR(DoWordType$, "U") > 0 THEN DoCommon%(uX%) = 1 'Commonality flag
IF UseX%(uX%) > 0 AND UseY%(uX%) > 0 THEN ShowWord FocusPuzWL$(uX%), DoType%(uX%), DoOrient%(uX%), UseX%(uX%), UseY%(uX%), 1
NEXT uX%

'Update Border
FOR uX% = 1 TO FocusPuzCount%: IF UseX%(uX%) > 0 AND UseY%(uX%) > 0 THEN UpdateBorder FocusPuzWL$(uX%), DoType%(uX%), DoOrient%(uX%), UseX%(uX%), UseY%(uX%)
NEXT uX%

END SUB

SUB ShowWord (WordStr$, WordType%, WordOrient%, WordX%, WordY%, MaskFlag%)
'Plot the Outline and either a MASK or CHAR
IF WordType% = -1 THEN EXIT SUB 'Fail out

IF pMDA%(3) <> pMDA%(4) THEN COLOR pMDA%(3), pMDA%(4)

'WordType% {0 for PWord, 1 for XWord, 2 for KWord, -1 for OWord}
'WordOrient% {0 for Vertical, 1 for Horizontal}
'WordX% {Grid Position Column/X}
'WordY% {Grid Position Row/Y}
'MaskFlag% {1 '0 for Show word, 1 for show mask
'WordStr$ {String to use}

TileBack$ = CHR$(4)
WordLength% = LEN(WordStr$)'Length of the Word
WordIterative% = 1 'Reset

IF WordOrient% = 1 THEN 'Vertical Plot
        FOR Y = WordY% * 2 TO (WordY% + WordLength% - 1) * 2 STEP 2
                IF MaskFlag% = 1 THEN
                        LOCATE Y + 1, 2 + (2 * WordX%): PRINT TileBack$;
                        LOCATE Y + 1, 1 + (2 * WordX%): PRINT "³"; : LOCATE Y + 1, 3 + (2 * WordX%): PRINT "³";
                        LOCATE Y, 2 + (2 * WordX%): PRINT "Ä"; : LOCATE Y + 2, 2 + (2 * WordX%): PRINT "Ä";
                ELSE
                        LOCATE Y + 1, 2 + (2 * WordX%): PRINT MID$(WordStr$, WordIterative%, 1); : WordIterative% = WordIterative% + 1
                END IF
        NEXT Y
ELSE 'Horizontal
        FOR X = WordX% * 2 TO (WordX% + WordLength% - 1) * 2 STEP 2
        IF MaskFlag% = 1 THEN 'Draw Mask and Border
                LOCATE (WordY% * 2) + 1, X + 2: PRINT TileBack$;
                LOCATE (WordY% * 2) + 1, X + 3: PRINT "³"; : LOCATE (WordY% * 2) + 1, X + 1: PRINT "³";
                LOCATE (WordY% * 2) + 2, X + 2: PRINT "Ä"; : LOCATE (WordY% * 2), X + 2: PRINT "Ä";
        ELSE  'MaskFlag% = 0 Show Char, no border
                LOCATE (WordY% * 2) + 1, X + 2: PRINT MID$(WordStr$, WordIterative%, 1); : WordIterative% = WordIterative% + 1
        END IF
        NEXT X
END IF

END SUB

FUNCTION StrSplit$ (uVal%, uSplitBy$, uText$)
'StrSplit$ v1.0 - As per xyWords 1

'Splits a String
'uVal% =What segment to report back with 1=First
'uSplitBy$ = seperator char to use
'uText$ = entire string
'Note: Set up a Dynamic Shared SplitWords$(128) in your global declares
ERASE SplitWords$: REDIM SplitWords$(LEN(uText$) + 5) 'Add 5 bytes for a buffer

IF uText$ = "" THEN 'Fail
        StrSplit$ = "": EXIT FUNCTION
        END IF

'First one
i = INSTR(1, uText$, uSplitBy$)
IF i = 0 THEN 'FAIL
        StrSplit$ = uText$: EXIT FUNCTION
        END IF
        SplitWords$(1) = MID$(uText$, 1, i - 1)

'Second through to Count-1
A = 2: NextI = INSTR(i + 1, uText$, uSplitBy$)
DO
        IF NextI = 0 THEN 'get last one
                SplitWords$(A) = MID$(uText$, i + 1):  EXIT DO
        END IF

SplitWords$(A) = MID$(uText$, i + 1, NextI - 1 - i): A = A + 1: i = NextI: NextI = INSTR(i + 1, uText$, uSplitBy$)
        IF NextI = 0 THEN 'get last one
                SplitWords$(A) = MID$(uText$, i + 1): EXIT DO
        END IF
LOOP

IF uVal% > A THEN
        StrSplit$ = "" 'Return a blank
ELSE
        StrSplit$ = SplitWords$(uVal%)
END IF

                                                                                                        
END FUNCTION

FUNCTION SubmitTry%
'Check PuzBuffer$ against FocusPuzWL$() and then verify against FocusPuzF%()
'For now, it also handles game triggers
IF HelpUIView% = 1 THEN HelpUI 0 'Flip bag if on show

SubmitTry% = 1 '[Default] Return 0 for OK, 1 for NOT OK
FOR X% = 1 TO FocusPuzCount%
        IF PuzBuffer$ = FocusPuzWL$(X%) THEN
                IF FocusPuzF%(X%) = 0 THEN 'Handle word found
                        FocusPuzF%(X%) = 1 'Flip it to found
                       
                        IF DoCommon%(X%) = 1 THEN 'Word Commonality
                                PuzUWordCount% = PuzUWordCount% + 1
                        ELSE
                                PuzCWordCount% = PuzCWordCount% + 1
                        END IF

                        'Trigger found word
                        SELECT CASE DoType%(X%)
                                CASE -1 'OWord
                                PuzBagWordsCount% = PuzBagWordsCount% + 1: UpdateSUI 4, 0, ""
                                UpdateSUI 4, 0, "": IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(2)
                                PlayerCulmStat&(2) = PlayerCulmStat&(2) + 1

                                CASE 0 'PWord
                                PuzBoardWordsCount% = PuzBoardWordsCount% + 1: UpdateSUI 3, 0, ""
                                PlayerCulmStat&(2) = PlayerCulmStat&(2) + 1
                                IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(3)

                                CASE 1 'XWord
                                PuzBoardWordsCount% = PuzBoardWordsCount% + 1: UpdateSUI 3, 0, ""
                                PlayerCulmStat&(2) = PlayerCulmStat&(2) + 1
                                IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(4)
                               
                                CASE 2 'KWord
                                PuzBoardWordsCount% = PuzBoardWordsCount% + 1: PuzKeyWordTime% = PuzTimeCount%'Capture K Time
                                PlayerCulmStat&(2) = PlayerCulmStat&(2) + 1
                                UpdateSUI 3, 0, "": IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(1)
                              
                        END SELECT
                        IF DoType%(X%) > -1 THEN ShowWord FocusPuzWL$(X%), DoType%(X%), DoOrient%(X%), UseX%(X%), UseY%(X%), 0'Show KPX words
                        SubmitTry% = 0: EXIT FUNCTION

                ELSE    'Handle word duplicate
                        IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(6)
                        PlayerCulmStat&(4) = PlayerCulmStat&(4) + 1
                        PuzInvCount% = PuzInvCount% + 1: SubmitTry% = 1: UpdateSUI 2, 0, "": EXIT FUNCTION
                END IF
        END IF
NEXT X%
'If we're still alive here - trigger a "word not found"
IF EnableSound% = 1 THEN PLAY "MB" + PlayList$(5)
PuzInvCount% = PuzInvCount% + 1: SubmitTry% = 1: UpdateSUI 2, 0, ""
PlayerCulmStat&(4) = PlayerCulmStat&(4) + 1




END FUNCTION

SUB UIBorderDraw (StartC%)
        LOCATE 1, StartC% - 4: PRINT "Ë";
        FOR Y = 2 TO 11: LOCATE Y, StartC% - 4: PRINT "º"; : NEXT Y
        LOCATE 11, StartC% - 4: PRINT "È";
        LOCATE 11, 80: PRINT "¹";
        FOR X = StartC% - 3 TO StartC% + 29: LOCATE 11, X: PRINT "Í"; : NEXT X
        LOCATE 11, 62: PRINT "Ë"; : LOCATE 25, 62: PRINT "Ê";
        FOR Y = 12 TO 24: LOCATE Y, 62: PRINT "º"; : NEXT Y
        LOCATE 14, 62: PRINT "Ì"; : FOR X = 63 TO 79: PRINT "Í"; : NEXT X
        LOCATE 14, 80: PRINT "¹";
END SUB

SUB UpdateBorder (WordStr$, WordType%, WordOrient%, WordX%, WordY%)

'Plot the Outline and either a MASK or CHAR

'WordType% {0 for PWord, 1 for XWord}
'WordOrient% {0 for Vertical, 1 for Horizontal}
'WordX% {Grid Position Column/X}
'WordY% {Grid Position Row/Y}
'WordStr$ {String to use}

'Environment Variables Referenced
'None yet

'Procedure Generated Variables
WordLength% = LEN(WordStr$)'Length of the Word
WordYY% = WordY% * 2: WordXX% = WordX% * 2 'Save repeat calculations later in SUB

IF WordOrient% = 1 THEN 'Vertical Plot
        FOR Y = WordYY% TO (WordY% + WordLength% - 1) * 2 STEP 2
        IF SCREEN(Y, WordXX% + 1) = 32 THEN LOCATE Y, WordXX% + 1: PRINT CHR$(BorderConnect(Y + 1, 2 + WordXX%, 1)); 'UpLeft decision branch
        IF SCREEN(Y + 2, WordXX% + 1) = 32 THEN LOCATE Y + 2, WordXX% + 1: PRINT CHR$(BorderConnect(Y + 1, 2 + WordXX%, 2)); 'DnLeft decision branch
        IF SCREEN(Y, WordXX% + 3) = 32 THEN LOCATE Y, WordXX% + 3: PRINT CHR$(BorderConnect(Y + 1, 2 + WordXX%, 3)); 'UpRight decision branch
        IF SCREEN(Y + 2, WordXX% + 3) = 32 THEN LOCATE Y + 2, WordXX% + 3: PRINT CHR$(BorderConnect(Y + 1, 2 + WordXX%, 4)); 'DnRight decision branch
        NEXT Y
ELSE    'Procedure to update Horizontally orientated words
        FOR X = WordX% * 2 TO (WordX% + WordLength% - 1) * 2 STEP 2
                IF SCREEN(WordYY%, X + 1) = 32 THEN LOCATE WordYY%, X + 1: PRINT CHR$(BorderConnect(WordYY% + 1, X + 2, 1)); 'Upleft decision branch
                IF SCREEN(WordYY% + 2, X + 1) = 32 THEN LOCATE WordYY% + 2, X + 1: PRINT CHR$(BorderConnect(WordYY% + 1, X + 2, 2)); 'Dnleft decision branch
                IF SCREEN(WordYY%, X + 3) = 32 THEN LOCATE WordYY%, X + 3: PRINT CHR$(BorderConnect(WordYY% + 1, X + 2, 3)); 'UpRight decision branch
                IF SCREEN(WordYY% + 2, X + 3) = 32 THEN LOCATE WordYY% + 2, X + 3: PRINT CHR$(BorderConnect(WordYY% + 1, X + 2, 4)); 'DnRight decision branch
        NEXT X
END IF

END SUB

SUB UpdateSUI (Section%, uVal%, uStr$)
'Update StatusUI for various 'scoreboard' items during the game
'Initialise also performs a board configure (time, invalid, bag, puzzle words)
'This pulls from the PuzData$(PuzSelect%)
COLOR 7, 0'reset
SELECT CASE Section%

CASE 0 'Initialise
PuzTimeCount% = 0: PuzInvCount% = 0: PuzBagWordsCount% = 0: PuzKeyWordTime% = 0: PuzUWordCount% = 0: PuzCWordCount% = 0 'reset

'Line 1 - Collection
IF CollSelect% = 0 THEN 'FreePlay
        'redo this to align to FPList
        Temp$ = "Free Play!": TPuzNum$ = "(#" + LTRIM$(RTRIM$(STR$(PuzSelect%))) + ")"
ELSE
        'Fill in the update for the collection and puzzle index
        Temp$ = StrSplit$(1, "|", MID$(CollHead$(CollSelect%), 2))
        Temp$ = "Collection: " + Temp$: TPuzNum$ = "(#" + LTRIM$(RTRIM$(STR$(FocusCollPuz%))) + "/" + StrSplit$(1, "|", CollData$(CollSelect%)) + ")"
END IF
LOCATE 2, SUICol%: COLOR 0, 7: PRINT Temp$; : COLOR 7, 0'Output Collection/Free Play

'Line 2 and 3 - Puzzle Number and Name,  Difficulty
Temp$ = MID$(StrSplit$(1, "|", PuzHead$(PuzSelect%)), 2)
LOCATE 4, SUICol%: COLOR 9: PRINT "Puzzle"; : COLOR 7: PRINT ": "; TPuzNum$; " "; Temp$; : Temp$ = SplitWords$(2)
CAL% = VAL(Temp$): LOCATE 5, SUICol%: COLOR 9: PRINT "Difficulty"; : COLOR 7: PRINT ":"; CAL%; "- "; GetDifficulty$(VAL(Temp$))

PuzInvTotal% = VAL(SplitWords$(5))
PuzTimeTotal% = VAL(SplitWords$(4))'If N - result will be 0

'Line 3 - Timer
SELECT CASE PuzTimeTotal%
        CASE 0: LOCATE 6, SUICol%: COLOR 9: PRINT "Time Elapsed"; : COLOR 7: PRINT ": 0:00";
        CASE IS > 0: LOCATE 6, SUICol%: COLOR 9: PRINT "Time Remain"; : COLOR 7: PRINT ":";
END SELECT

'Line 4 - Invalid Word Capture
SELECT CASE PuzInvTotal%
        CASE 0: LOCATE 7, SUICol%: COLOR 9: PRINT "Invalid Attempts"; : COLOR 7: PRINT ": 0";
        CASE IS > 0: LOCATE 7, SUICol%: COLOR 9: PRINT "Invalid Attempts Remain"; : COLOR 7: PRINT ":"; PuzInvTotal%;
END SELECT

'Line 5 and 6
PuzBoardWordsTotal% = VAL(StrSplit$(1, "|", PuzData$(PuzSelect%))) + VAL(SplitWords$(2))
LOCATE 8, SUICol%: COLOR 9: PRINT "Board Words"; : COLOR 7: PRINT ": 0"; " of"; PuzBoardWordsTotal%;

PuzBagWordsTotal% = VAL(SplitWords$(3))
IF PuzBagWordsTotal% > 0 THEN
        LOCATE 9, SUICol%: COLOR 9: PRINT "Bag Words"; : COLOR 7: PRINT ": 0 of "; PuzBagWordsTotal%
        LOCATE 13, 64: COLOR 15, 0: PRINT "F2-"; : COLOR 7, 0: PRINT " Word Bag";
ELSE
        LOCATE 9, SUICol%: PRINT "No Bag Words";
END IF

CASE 1 'Timer
SELECT CASE PuzTimeTotal%
        CASE 0: LOCATE 6, SUICol% + 14: PRINT TimeString$(PuzTimeCount%);
        CASE IS > 0: LOCATE 6, SUICol% + 13: PRINT TimeString$(PuzTimeTotal% - PuzTimeCount%)
END SELECT

CASE 2 'Invalid Word
        SELECT CASE PuzInvTotal%
                CASE 0: LOCATE 7, SUICol% + 17: PRINT PuzInvCount%;
                CASE IS > 0: LOCATE 7, SUICol% + 24: PRINT PuzInvTotal% - PuzInvCount%
        END SELECT

CASE 3 'Board Words
        LOCATE 8, SUICol% + 12: PRINT ; PuzBoardWordsCount%; "of"; PuzBoardWordsTotal%;
        
CASE 4 'Bag Words
        IF PuzBagWordsTotal% > 0 THEN LOCATE 9, SUICol% + 11: PRINT PuzBagWordsCount%; "of"; PuzBagWordsTotal%
END SELECT 'Section%
END SUB

