' //////////////////////////////////////////////////////////////
'        Biker Mice.. most of the script is from Swamp Thing
' //////////////////////////////////////////////////////////////
'  
' 	
'
' //////////////////////////////////////////////////////////////
'   Rules -- different than Serious Sam or Swamp Thing v1
' //////////////////////////////////////////////////////////////

' - 3 Balls Per Player
' - Multiball on every 3rd battle win
 ' - There are 5 regular battles, then the Limburger Wizard Mode starts
' - Shoot glowing Limburger Plaza to start a battle
' - Shoot Last Chance Garage for a random reward
' CAREFUL.... the targets have been moved around and switched from Serious Sam!!
'
'BASIC SCORING
'.    - Bonus Multiplier... Hit top -or- flipper inlane sets to increase up to 5x
'.    - Targets and Lanes... 5,000 points
'.    - Slingshots... 210 points, and alternate the outlane special if lit
'.    - Outlanes... 50,000 points
'.    - Spinners... 1,000 points/spin +500 points for every 3 Small Target hits. No max value limit
'.    - Ramp Combos... Jackpot value awarded after ramp combo (2 hits in immediate succession)
'.    - Jackpots... start at 100,000 and can increase with other scoring hits
'.    - Captive Ball... hits increase Jackpot value 50,000 when Increase Jackpot is lit
'

'MULTIBALL
'.    - One multiball active at a time.
'.    - Max of 5 balls at a time
'.    - Can score Jackpots and start/play Battles
'.    - Hit both ramps to collect the Jackpots
'.    - Hit captive ball to re-enable the Jackpots
'.    - Battle multiball starts automatically after every 3 won battles
'.    - Regular multiball starts after locking 3 balls
'
'BALL LOCK
'.    - Locking a ball requires both Large Side Targets be lit
'.    - Shoot Limburger Plaza to lock the ball
'.    - Jackpot value increases when Large Side Targets are lit
'
'POWER UP
'.    - 10 Ramp hits lights Power Up for 30 seconds and Jackpot enabled on the right ramp
'
'SMALL TARGETS after all hit
'.    - Ball Save activates on one outlane at a time. Change outlane by hitting slingshots
'.    - Spinner value increases when the outlane is lit
'.    - Defeat one Target to get extra points
'
'MYSTERY AWARD
'.    Enter Last Chance Garage to enable one of these...
'.    - Instant 2-Ball Multiball
'.    - Extra Points Awarded (10 - 100,000)
'.    - Instant win of incomplete battle
'.    - Increase Playfield Scoring to 5x for 30 seconds
'.    - Increase Bonus X
'.    - Light Extra Ball
'.    - Enable 20 second Ball Save
'
'SKILL SHOT
'.    - values starts at 500,000 points and increase 250,000 for each skill shot obtained.
'.    - Method 1: Hit the lit inlane light on above bumper area
'.    - Method 2: Within 6 seconds of launch, hit the captive ball at left of playfield
' Change line 103 to "2" if you want to enable VR
'
' 


'
' ////////////////////////////////////////////////////////////////////////////////////////
Option Explicit
Randomize
'*************************************************************************
'
' USER Config -- OK to edit this section
'
'*************************************************************************
' NONE!
'
'********************************************************************************
'
'********************************************************************************
'
' // User Settings in F12 menu //
Dim LightLevel			:	LightLevel = 50	   		' Day/Night as a %
Dim LUTimage			:	LUTimage = 2			' Table Color Brightness // 0 to 21 : 2 default (22 total : 11 Cool & 11 Warm)
Dim BattleDifficulty	:	BattleDifficulty = 1	' Normal or Hard (2x as challenging) //  1 (default) Normal : 2 Hard
Dim VolumeDial : VolumeDial = 0.5           		' 0.1 to 1.0 :: Overall Mechanical sound effect volume.
Dim BallRollVolume : BallRollVolume = 0.5   		' 0.1 to 1.0 :: Overall ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 			' 0.1 to 1.0 :: Overall ramp rolling volume. Value between 0 and 1
Dim ArchSoundFactor	:ArchSoundFactor = 5.0			' 0.1 to 5.0 :: Ball rolling on Arches, like Loops and Lanes : sound factor / volume multiplier; must not be zero
Dim RollingSoundFactor : RollingSoundFactor = 1.0	' 0.1 to 5.0 :: Playfield Ball rolling sound factor / volume multiplier; must not be zero 5 max
' ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
'  SWAMP THING specific... 
'		changes Fleep RampLoop to fx_BallRoll_WoodRamp
'		changes Fleet WireLoop to fx_BallRoll_WoodLoop
' ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
Dim RampPlasticLoop : RampPlasticLoop = "fx_ballrolling"
Dim RampWireLoop : RampWireLoop = "fx_BallRoll_WoodRamp"

'VRRoom Settings
Dim VRRoom				:   VRRoom = 0 				' 0 = Desktop Mode, 2 = Minimal Room

' for testing only!
const ForceBattle = 0	' force battles to run a set battle (Case number), set to 0 when not in use

' set number of hits for each battle / mission
dim battleSpin			:	battleSpin = 20*BattleDifficulty 		' rotate spinners
dim battleSuperBumper	:	battleSuperBumper = 8*BattleDifficulty	' pop bumper hits
dim battleRamp			:	battleRamp = 3*BattleDifficulty			' full loop on left or right ramps
dim battleLight			:	battleLight = 2*BattleDifficulty		' enter areas with Skull lights to turn off skull lights
dim battleTarget		:	battleTarget = 4*BattleDifficulty		' hit Large side targets
dim battleLoop			:	battleLoop = 2*BattleDifficulty			' full outer orbit loops (both ends at same time)
dim battleOrbit			:	battleOrbit = 2*BattleDifficulty		' hit either side of outer orbits - looping not needed
dim BattleStartMode		:	BattleStartMode = 1					' default = 1 // when = 2, must hit radioactive area's invisible door once before being allowed in on the 2nd time. Maskes missions/battles harder to start

const battleFollowTheLights = 10000	' milliseconds

'*******************************************
'  ZCON: Constants and Global Variables
'*******************************************

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
Const BallMass = 1				  'Ball mass must be 1
Const tnob = 19					  'Total number of balls the table can hold
Const lob = 2					   'Locked balls

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False


' Constants
Const cGameName = "bikermicefrommars"	' table uses DOF from serioussam table. swampthing is alias in DOF
Const TableName = "Biker Mice"
Const myVersion = "2.0"
Const SongVolume = 0.3 ' 1 is full volume. Value is from 0 to 1
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 30 ' in seconds
Const MaxMultiplier = 5  ' limit to 5x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

' -- FlexDMD in high or normal quality
' True for LCD screen (outputs 256x64 for display)
' False when using a real DMD (outputs 128x32 for display)
Const FlexDMDHighQuality = True
Dim UseFlexDMD 
' If Table1.ShowDT = True then UseFlexDMD = False : End If
'
'
' -- BATTLES --
' 1	Super Spinners  //  FLORONIC MAN , HIT SPINNERS
' 2	Pop Bumpers   //  GENERAL SUNDERLAND , HIT POP BUMPERS
' 3	Ramp Loops //  LADY WEEDS , HIT RAMP LOOPS
' 4	Orbits Hits //  MATANGO ,  HIT ORBIT LANES
' 5	Hit Skull Lights  //  SETHE ,  HIT SKULL LIGHTS	
' 6	Hit the Moving Light //  KAREN CLANCY , HIT THE MOVING LIGHT
			' Sequence :
				' Target1 ... Lg Target right - flower Light29
				' Target12 ... CrocMan Cave - skull Light30
				' Target13 ... Lg Target right left - flower Light31
				' sw7 ... Orbit lane light - skull Light32
				' sw8 ... Orbit lane left - skull Light33
				' RightRampDone ... Ramp right - skull Light34
				' JackalHole ... Flying Gator Cave - skull Light35
				' LeftRampDone... Ramp left - skull Light36
				' Lock ... Radioactive Swamp - skull Light37
' 7	Small Targets //  BLACK LANTERN , HIT SMALL TARGETS
' 8	Large Targets //  TOXIC MONSTER ,  HIT LARGE TARGETS 
' 9	Light Chase //  ANUBIS ,  HIT LIT LIGHTS  (light moves every 10 seconds)
' 10 Outer Orbit Loop //   SEEDER ,  OUTER ORBIT LOOPS
' 11 Light Chase part du  //    AVATAR OF ROT ,  LIGHT CHASE part du
' 12 Ramps and Orbits  CONCLAVE ,  HIT RAMPS + ORBITS 
' 13 ARCANE ,  HIT JACKPOTS 
'
'
' -- LIGHTS --
' *left to right numbering
'
' SWAMP THING logo
' 	SWAMP : 8,5,6,2,1
' 	THING : 3,4,10,9,7
'
'	Light38	' mystery light (fade in-out) - radioactive swamp, extra bright glow
'	light52	' multiball - Wing Zero button, red glow
'	light39	' extra ball - Wing Zero button, blue glow
'	light54	' playfield multiplier - Swamp Thing heart, purple glow
'
' Large Lock Target
'	Light29	' right red Light
'	Light20	' right blue light
'	Light31	' left red Light
'	Light19	' left blue light
'
' Trees
'	Light44	' top row 1 - Jackpot
'	Light45 ' top row 2 - Jackpot
'	Light41	' mid row 1 - Jackpot
'	Light40	' mid row 2 - Jackpot
'	Light43 ' mid row 3 - Power Up
'	Light46 ' mid row 4 - Lock
'	Light48 ' mid row 5 - Super Jackpot
'	Light50 ' mid row 6 - Jackpot
'	Light51 ' mid row 7 - Jackpot
'	Light42 ' low row 1 - Jackpot
'
'
' Skulls - Shoot The Lights battle
'	Light36	' top row, Left Ramp Start
'	Light37 ' top row, Center Radioactive Swamp
'	Light33 ' mid row. Left Orbit
'	Light34	' mid row, Right Ramp Start
'	Light35 ' mid row, Flying Aligator cave
'	Light32 ' mid row, Right Orbit
'	Light30	' low row, Croc Cave
'
Sub Table1_OptionEvent(ByVal eventId)
	' 10.8 only : called when options are tweaked by the player. 
	'... 0: game has started, good time to load options and adjust accordingly
	'... 1: an option has changed
	'... 2: options have been reseted
	'... 3: player closed the tweak UI, good time to update staticly prerendered parts
'	Table1.Option arguments... option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional Array for Menu Text fot that Option/Setting (strings)
'
    If eventId = 1 Then DisableStaticPreRendering = True

	' --- Battle Difficulty --- Normal or Hard (2x as challenging)
	BattleDifficulty = Table1.Option("Battle Difficulty", 1, 2, 1, 1, 0, Array("Normal","Hard (2x)"))

	' --- Table LUT --- 
    LUTimage = Table1.Option("Table Brightness (LUT)", 0, 21, 1, 2, 0, Array("0","1","2","3","4","5","6","7","8","9","10"," Warm 0"," Warm 1"," Warm 2"," Warm 3"," Warm 4"," Warm 5"," Warm 6"," Warm 7"," Warm 8"," Warm 9"," Warm 10"))
	Table1.ColorGradeImage = "LUT" & LUTimage

	' --- Volume Controls ---
	VolumeDial= Table1.Option("Mechanical Volume", 0.1, 1.0, 0.1, 0.5,1)		' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
	BallRollVolume = Table1.Option("Playfield Roll Volume", 0, 1.0, 0.1, 0.1,1)	' Level of ramp rolling volume. Value between 0 and 1
	RampRollVolume = Table1.Option("Ramp Roll Volume Boost", 0, 5.0, 1, 5,1)	
	RollingSoundFactor = Table1.Option("General Roll Vol Boost", 0.1, 1.0, 0.1, 0.5,0)		' Fleep code default... 1.1 / 5
	ArchSoundFactor = Table1.Option("Arch Roll Vol Boost", 0.1, 1.0, 0.1, 0.5,0)				' Fleep code default... 0.025 / 5			 
'
'
	' ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	'  SWAMP THING specific... changed Fleep RampLoop and WireLoop sound files to be variables
	' ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	Dim RampPL : RampPL = Table1.Option("Solid Ramp sound", 1, 2, 1, 1, 0, Array("Normal (default)","Wood"))
	If RampPL = 1 Then RampPlasticLoop="fx_ballrolling" : Else  RampPlasticLoop="fx_BallRoll_WoodRamp": End If
		debug.print "Ramp Plastic Loop : " & RampPlasticLoop
	dim RampWL : RampWL = Table1.Option("Rail Ramp sound", 1, 2, 1, 2, 0, Array("Normal","Wood (default)"))
	If RampWL = 1 Then RampWireLoop="fx_ballrolling" : Else  RampWireLoop="fx_BallRoll_WoodRamp": End If
		debug.print "Ramp Wire Loop : " & RampWireLoop

    If eventId = 3 Then DisableStaticPreRendering = False

End Sub
'
'
' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

'*******************************************
'  ZOPT: User Options
'*******************************************

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim SkillShotLane ' tracks which top inlane skillshot is set for

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(4)
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bJackpot
dim bRulesActive

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim cbRight   'captive ball
Dim bsJackal
Dim x
'
' //////////////////////////////////////////////////////
'  FOG - core code from JPSalas, tweaks from LTek
' //////////////////////////////////////////////////////
'
'Dim fr, fh, fhmax, fopac ' rotation, height, fade rate
'
'Sub Start_Fog
'    fr = 0								' starting rotation angle
'    fh = 0.5							' starting height
'	fhmax = 40							' set MAX Layer Height here
'    fopac = 0.001						' intensity amt added to each timer cycle
'    fog_flash.IntensityScale = 0.001	' starting intesity
'    fog_flash.visible = 1
'    fog_flash.Timerenabled = 1
' 'debug.print "Start Fog, Opacity : " & fopac
'End Sub
'
'Sub Stop_Fog
'    fopac = -0.02
' 'debug.print "Stop Fog, Opacity : " & fopac
'End Sub
'
'Sub fog_flash_Timer
'    'rotation
'    fr = fr + 0.125
'    If fr > 360 Then fr = 0
'    fog_flash.RotZ = fr
'    'height
'    If fh < fhmax Then		
'        fh = fh + 0.125
'        fog_flash.height = fh
'    End If
'    ' opacity
'    If fopac > 0 then
' 'debug.print "Fog Timer, Opacity : " & fopac
'        If fog_flash.IntensityScale >0 and fog_flash.IntensityScale < 0.5 then
'            fog_flash.IntensityScale = fog_flash.IntensityScale + fopac
'        End If
'    End If
'    If fopac < 0 Then
' 'debug.print "Fog Timer, Opacity : " & fopac
'        fog_flash.IntensityScale = fog_flash.IntensityScale + fopac
'        If fog_flash.IntensityScale < 0 Then
' 'debug.print "Fog Intensity < 0, Opacity : " & fopac
'            fog_flash.Timerenabled = 0
'        End If
'    End If
'End Sub
'
'
' ////////////////////////////////////////////
'  3D Model Animations
' ////////////////////////////////////////////

'Dim shakeCROCcounter	' counters
'
'Sub shakeCROC()		' sub used to trigger shake
'    shakeCROCcounter = 8
'    timerCROC.Enabled = True
'End Sub
'
'Sub timerCROC_Timer()
'    CrocMan.TransZ = shakeCROCcounter / 2
'    If shakeCROCcounter = 0 Then Me.Enabled = False:Exit Sub
'    If shakeCROCcounter < 0 Then
'        shakeCROCcounter = ABS(shakeCROCcounter)- 0.5
'    Else
'        shakeCROCcounter = - shakeCROCcounter + 0.5
'    End If
'End Sub
'
'Dim shakeANTONcounter	' counters
'
'Sub shakeANTON()		' sub used to trigger shake
'    shakeANTONcounter = 8
'    timerANTON.Enabled = True
'End Sub
'
'Sub timerANTON_Timer()
'    AntonArchane.TransZ = shakeANTONcounter / 2
'    If shakeANTONcounter = 0 Then Me.Enabled = False:Exit Sub
'    If shakeANTONcounter < 0 Then
'        shakeANTONcounter = ABS(shakeANTONcounter)- 0.5
'    Else
'        shakeANTONcounter = - shakeANTONcounter + 0.5
'    End If
'End Sub
'
'
'Dim GATORcounter
'
'Sub flyGATOR
'    GATORcounter = 3
'    timerGATOR.Enabled = 1
'End Sub
'
'Sub timerGATOR_Timer
'    'GATOR.RotX = GATORcounter * -90
'    'GATOR.ObjRotX = GATORcounter * -90
'    GATOR.TransX = GATORcounter * 4
'    GATOR.TransY = GATORcounter * 4
'    If GATORcounter = 0 Then Me.Enabled = 0:Exit Sub
'    If GATORcounter < 0 Then
'        GATORcounter = ABS(GATORcounter)- 1
'    Else
'        GATORcounter = - GATORcounter + 1
'    End If
'End Sub
'
'Dim SWAMPTHINGcounter
'
'Sub shakeSWAMPTHING
'    SWAMPTHINGcounter = 2
'    timerSWAMPTHING.Enabled = 1
'End Sub
'
'Sub timerSWAMPTHING_Timer
'    'SWAMPTHING.ObjRotX = SWAMPTHINGcounter * -5
'    'SWAMPTHING.RotX = SWAMPTHINGcounter * -90
'    SWAMPTHING.TransX = SWAMPTHINGcounter * 4
'    'SWAMPTHING.TransY = SWAMPTHINGcounter * 4
'    SWAMPTHING.TransZ = SWAMPTHINGcounter * 4
'    If SWAMPTHINGcounter = 0 Then Me.Enabled = 0:Exit Sub
'    If SWAMPTHINGcounter < 0 Then
'        SWAMPTHINGcounter = ABS(SWAMPTHINGcounter)- 1
'    Else
'        SWAMPTHINGcounter = - SWAMPTHINGcounter + 1
'    End If
'End Sub

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger1, CapWall1, Array(CapKicker1, CapKicker1a), 0
        .NailedBalls = 1
        .ForceTrans = .9
        .MinForce = 3.5
        '.CreateEvents "cbRight"
        .Start
    End With
    CapKicker1.CreateSizedBallWithMass BallSize / 2, BallMass

    ' Jackal hole
    Set bsJackal = New cvpmTrough
    With bsJackal
        .size = 5
        .Initexit JackalHole, 160, 35
        '.InitExitVariance 2, 2
        .MaxBallsPerKick = 1
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs

    ' Initalise the DMD display
    DMD_Init
	' Turn off the bumper lights
	FlBumperFadeTarget(1) = 0
	FlBumperFadeTarget(2) = 0
	FlBumperFadeTarget(3) = 0
	FlBumperFadeTarget(4) = 0
	FlBumperFadeTarget(5) = 0

    ' freeplay or coins
    bFreePlay = False 'we want coins

    if bFreePlay Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock(1) = 0
    BallsInLock(2) = 0
    BallsInLock(3) = 0
    BallsInLock(4) = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
    bInstantInfo = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

	' Rules Screen setup
	RulesWall.IsDropped = True
	'bRulesActive = False

    ' Start the RealTime timer
    RealTime.Enabled = 1

	' -- Flashing Starfield --
	' uses collection LightSeqStars,
	' Flashers turned 90 degrees so they lay on the backdrop backwall and can be seen.
	' created random set of sizes, duplicated several times, arranges randomly
	LightSeqStars.UpdateInterval = 600    ' time between seq cycle
	LightSeqStars.Play SeqRandom,2,,400000000
	LightSeqStars.Play SeqBlinking

	ResetToys

'Load VR ROOM		
		VRChangeRoom
End Sub

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script. 

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'******************************************************
'  ZANI: Misc Animations
'******************************************************

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	LeftFlipperTop.RotZ = a
	'Add any left flipper related animations here
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	RightFlipperTop.RotZ = a
	'Add any right flipper related animations here
End Sub

'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit:cbRight.TrigHit ActiveBall:End Sub
Sub CapTrigger1_UnHit:cbRight.TrigHit 0:End Sub
Sub CapWall1_Hit:cbRight.BallHit ActiveBall:PlaySoundAtBall "fx_collide"End Sub
Sub CapKicker1a_Hit:cbRight.BallReturn Me:End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

	' Show Rules On Screen
	If NOT bGameInPlay and Keycode = LeftMagnaSave then
		If RulesFlasher.visible = False Then
		RulesFlasher.visible = True
		RulesWall.visible = True
		else
		RulesFlasher.visible = False
		RulesWall.visible = False
		End If
	End If

    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
		Select Case Int(Rnd * 3)
			Case 0
				PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1
				PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2
				PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select

        Credits = Credits + 1

        If bFreePlay = False Then DOF 125, DOFOn

        If(Tilted = False)Then
            DMDFlush
            DMD "_", CL("CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "" : pupevent 818
            If NOT bGameInPlay Then ShowTableInfo
        End If

    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        SoundPlungerPull
    End If
	If keycode = StartGameKey Then
		SoundStartButton
	End If
    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper True:InstantInfoTimer.Enabled = True
        If keycode = RightFlipperKey Then SolRFlipper True:InstantInfoTimer.Enabled = True

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "mu_war0"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "mu_war0"
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "buzz"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
						pupevent 800
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
							pupevent 800
                            Credits = Credits - 1
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "buzz"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
'Change VR-Room with magna-save buttons
	If keycode = rightmagnasave then
		If VRRoom>0 Then
			vrroom = vrroom +1
			if vrroom > 3 then vrroom = 1
			VRChangeRoom
			End If
End If
End Sub

Sub Table1_KeyUp(ByVal keycode)
    ' If keycode = LeftMagnaSave Then bLutActive = False: HideLUT	' retired, using User Options Menu
	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper False
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper False
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
    End If
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
    End If
End Sub

Sub InstantInfo
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 800, False, ""
    DMD CL("JACKPOT VALUE"), CL(Jackpot(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("SPINNER VALUE"), CL(spinnervalue(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("BUMPER VALUE"), CL(bumpervalue(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("BONUS X"), CL(BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("PLAYFIELD X"), CL(PlayfieldMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("LOCKED BALLS"), CL(BallsInLock(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("LANE BONUS"), CL(LaneBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("TARGET BONUS"), CL(TargetBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("RAMP BONUS"), CL(RampBonus), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("TARGETS ELIMINATED"), CL(MonstersKilled(CurrentPlayer)), "", eNone, eNone, eNone, 800, False, ""
    DMD CL("HIGHEST SCORE"), CL(HighScoreName(0) & " " & HighScore(0)), "", eNone, eNone, eNone, 800, False, ""
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire : DOF 101,1 'leftflipper.rotatetoend
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart : DOF 101,0
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire : DOF 102,1 'rightflipper.rotatetoend
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart : DOF 102,0
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt < 15)Then 'show a warning
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", CL("TILT"), "", eNone, eNone, eNone, 200, False, ""
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        'Bumper1.Force = 0

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        'Bumper1.Force = 6
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub PlayBattleSong
    Dim tmp
    tmp = INT(RND * 6)
    Select Case tmp
        Case 0:PlaySong "mu_battle1"
        Case 1:PlaySong "mu_battle2"
        Case 2:PlaySong "mu_battle3"
        Case 3:PlaySong "mu_battle4"
        Case 4:PlaySong "mu_battle5"
        Case 5:PlaySong "mu_battle6"
    End Select
End Sub

Sub PlayMultiballSong
    Dim tmp
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:PlaySong "mu_war1"
        Case 1:PlaySong "mu_war2"
        Case 2:PlaySong "mu_war3"
        Case 3:PlaySong "mu_war4"
    End Select
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        'PlaySong "mu_end"
        Exit Sub
    End If

    If bMultiBallMode Then
        PlayMultiballSong
    Else
        Select Case Battle(CurrentPlayer, 0)
            Case 0
                PlaySong "mu_main"
            Case 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                PlayBattleSong
            Case 13
                PlayMultiballSong
        End Select
    End If
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 3) + 1
    PlaySound "quote_" &tmp
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
    End Select
End Sub

Sub FlashEffect(n)
    Dim ii
    Select case n
        Case 0 ' all off
            LightSeqFlasher.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 5, 10
    End Select
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

    vpmtimer.addtimer 1500, "FirstBall '"

	ResetToys

End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()

    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reduce the playfield multiplier
    SetPlayfieldMultiplier 1

    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallVariables
    ResetNewBallLights()

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

'Change the music ?
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    RandomSoundBallRelease BallRelease
    BallRelease.Kick 90, 4

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        DOF 143, DOFPulse
        bMultiBallMode = True
        bAutoPlunger = True
        ChangeGi 5
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

'add in any bonus points (multipled by the bonus multiplier)
'AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
'AddScore AwardPoints
'debug.print "Bonus Points = " & AwardPoints
'DMD "", CL("BONUS: " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer) ), "", eNone, eBlink, eNone, 1000, True, ""

'Count the bonus. This table uses several bonus
'Lane Bonus
        AwardPoints = LaneBonus * 1000
        TotalBonus = AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("LANE BONUS " & LaneBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Target hits
        AwardPoints = TargetBonus * 2000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Ramps completed
        AwardPoints = RampBonus * 10000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("RAMP BONUS " & RampBonus), "", eBlink, eNone, eNone, 800, False, ""

        'Number of Monsters Killed
        AwardPoints = MonstersKilled(CurrentPlayer) * 25000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(FormatScore(AwardPoints)), CL("TARAGETS ELIMINATED " & MonstersKilled(CurrentPlayer)), "", eBlink, eNone, eNone, 800, False, ""

        ' calculate the totalbonus
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)

        ' handle the bonus held
        ' reset the bonus held value since it has been already added to the bonus
        BonusHeldPoints(CurrentPlayer) = 0

        ' the player has won the bonus held award so do something with it :)
        If bBonusHeld Then
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If
        Else ' this is not the last ball so save the bonus for the next ball
            BonusHeldPoints(CurrentPlayer) = TotalBonus
        End If
        bBonusHeld = False

        ' Add the bonus to the score
        DMD CL(FormatScore(TotalBonus)), CL("TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer)), "", eBlinkFast, eNone, eNone, 1500, True, ""

        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 6000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, ""

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode
		pupevent 804
        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame > 1 Then
            PlaySound "vo_player" &CurrentPlayer
            DMD "_", CL("PLAYER " &CurrentPlayer), "_", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    
	'debug.print "End Of Game"
    bGameInPLay = False
    
	' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper False
    SolRFlipper False

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()

	'Stop_Fog

    ' Destroy the ball
    Drain.DestroyBall
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active,
        If(bBallSaverActive = True)Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 800, True, "" : pupevent 805
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ResetJackpotLights
                    Select Case Battle(CurrentPlayer, 0)
                        Case 13:WinBattle
                    End Select
                    ChangeGi white
                    ChangeSong
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' End Mode and timers
                ChangeSong
                ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" : pupevent 803 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
	BIPL = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 121, DOFPulse
		SoundPlungerReleaseBall
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    Else
        ' show the message to shoot the ball in case the player has fallen sleep
        swPlungerRest.TimerEnabled = 1
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        UpdateSkillshot()
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
	BIPL = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ResetSkillShotTimer.Enabled = 1
    End If
    If bMultiballMode Then
        If BallsOnPlayfield = 2 Then
            ChangeSong
        End If
    Else
        ChangeSong
    End If
' turn off LaunchLight
' LaunchLight.State = 0
End Sub

' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    DMD "_", CL("SHOOT THE BALL"), "_", eNone, eNone, eNone, 800, True, ""
    swPlungerRest.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
' In this table we use SecondRound variable to double the score points in the second round after killing Malthael
Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    End if
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False)Then

        ' If(bMultiBallMode = True) Then
        Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
        DMD "_", CL("INCREASED JACKPOT"), "_", eNone, eNone, eNone, 800, True, ""
    ' you may wish to limit the jackpot to a upper limit, ie..
    '	If (Jackpot >= 6000) Then
    '		Jackpot = 6000
    ' 	End if
    'End if
    End if
End Sub

Sub AddSuperJackpot(points) 'not used in this table
    If(Tilted = False)Then
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL("BONUS X " &NewBonusLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
    Else
        AddScore 50000
        DMD "_", CL("50000"), "_", eNone, eNone, eNone, 800, True, ""
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly

Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level
    UPdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level)
    ' Update the lights
    Select Case Level
        Case 1:light56.State = 0:light57.State = 0:light58.State = 0:light59.State = 0
        Case 2:light56.State = 1:light57.State = 0:light58.State = 0:light59.State = 0
        Case 3:light56.State = 0:light57.State = 1:light58.State = 0:light59.State = 0
        Case 4:light56.State = 0:light57.State = 0:light58.State = 1:light59.State = 0
        Case 5:light56.State = 0:light57.State = 0:light58.State = 0:light59.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    ' if not at the maximum level x
    if(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier)then
        ' then add and set the lights
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
        DMD "_", CL("PLAYFIELD X " &NewPFLevel), "_", eNone, eNone, eNone, 2000, True, "fx_bonus"
    Else 'if the 5x is already lit
        AddScore 50000
        DMD "_", CL("50000"), "_", eNone, eNone, eNone, 2000, True, ""
    End if
'Start the timer to reduce the playfield x every 30 seconds
' pfxtimer.Enabled = 0
' pfxtimer.Enabled = 1
End Sub

' Set the Playfield Multiplier to the specified level AND set any lights accordingly

Sub SetPlayfieldMultiplier(Level)
    ' Set the multiplier to the specified level
    PlayfieldMultiplier(CurrentPlayer) = Level
    UpdatePFXLights(Level)
End Sub

Sub UpdatePFXLights(Level)
' in this table the multiplier is always shown in the score display sub

' Update the lights
'Select Case Level
'    Case 1:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 0
'    Case 2:light3.State = 1:light2.State = 0:light1.State = 0:light4.State = 0
'    Case 3:light3.State = 0:light2.State = 1:light1.State = 0:light4.State = 0
'    Case 4:light3.State = 0:light2.State = 0:light1.State = 1:light4.State = 0
'    Case 5:light3.State = 0:light2.State = 0:light1.State = 0:light4.State = 1
'End Select
' show the multiplier in the DMD
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1 'light the shoot again lamp
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(("EXTRA GAME WON")), "_", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    FlashEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot, double or triple jackpot
    Dim tmp
    DMD CL(FormatScore(Jackpot(CurrentPlayer))), CL("JACKPOT"), "d_border", eBlinkFast, eBlinkFast, eNone, 1000, True, ""
    DOF 126, DOFPulse
    tmp = INT(RND * 2)
    Select Case tmp
        Case 0:PlaySound "vo_Jackpot"
        Case 1:PlaySound "vo_Jackpot2"
        Case 2:PlaySound "vo_Jackpot3"
    End Select
    AddScore Jackpot(CurrentPlayer)
    LightEffect 2
    FlashEffect 2
    'sjekk for superjackpot
    EnableSuperJackpot
End Sub

Sub AwardSuperJackpot() 'this is actually 4 times a jackpot
    SuperJackpot = Jackpot(CurrentPlayer) * 4
    DMD CL(FormatScore(SuperJackpot)), CL("SUPER JACKPOT"), "d_border", eBlinkFast, eBlinkFast, eNone, 1000, True, "vo_superjackpot"
    DOF 126, DOFPulse
    AddScore SuperJackpot
    LightEffect 2
    FlashEffect 2
    'enabled jackpots again
    StartJackpots
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    'show dmd animation
    DMD CL(FormatScore(SkillshotValue(CurrentPlayer))), CL(("SKILLSHOT")), "d_border", eBlinkFast, eBlink, eNone, 1000, True, ""
    DOF 127, DOFPulse
    PlaySound "vo_welldone"
    Addscore SkillShotValue(CurrentPlayer)
    ' increment the skillshot value with 250.000
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 250000
    'do some light show
    GiEffect 2
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 100000
    HighScore(1) = 100000
    HighScore(2) = 100000
    HighScore(3) = 100000
    Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
    tmp = Score(CurrentPlayer)

    If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    'PlaySound "vo_greatscore" &RndNbr(6)
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr)
    DMDUpdate 0

    TempBotStr = "    > "
    if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1)Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text.
' 1st and 2nd lines are 20 characters long
' 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 256
                FlexDMD.Height = 64
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            Else
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 128
                FlexDMD.Height = 32
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            End If
        End If
    End If

    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 10
    deBlinkFastRate = 5
    dCharsPerLine(0) = 20 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i))
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    dLine(2) = " "
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        tmp = RL(FormatScore(Score(Currentplayer)))
        'tmp = CL(FormatScore(Score(Currentplayer) ) )
        'tmp1 = CL("PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        Select Case Battle(CurrentPlayer, 0)
            Case 0:tmp1 = CL("PLAYER " & CurrentPlayer & " BALL " & Balls & " X" & PlayfieldMultiplier(CurrentPlayer))
            Case 1:tmp1 = CL(battleSpin - SpinCount & " MORE SPINNERS")
            Case 2:tmp1 = CL(battleSuperBumper - SuperBumperHits & " MORE BUMPER HITS")
            Case 3:tmp1 = CL(battleRamp - ramphits3 & " MORE RAMP HITS")
            Case 4:tmp1 = CL(battleOrbit - orbithits & " MORE ORBIT LOOPS")
            Case 5:tmp1 = CL("HIT ALL GREEN LIGHTS")
			Case 6:tmp1 = CL("HIT THE MOVING LIGHT")
            Case 7:tmp1 = CL(battleTarget - TargetHits7 & " MORE SMALL TARGETS")
            Case 8:tmp1 = CL(battleTarget - TargetHits8 & " MORE LARGE TARGETS")
            Case 9:tmp1 = CL(battleLight - LightHits9 & " MORE LIGHTS TO CHASE")
            Case 10:tmp1 = CL(battleLoop - loopCount & " MORE ORBIT LOOPS")	' sw7 & sw8
            Case 11:tmp1 = CL(battleLight - LightHits11 & " MORE LIGHTS")
            Case 12:tmp1 = CL(battleRamp - RampHits12 & " MORE RAMPS ORBITS")
            Case 13:tmp1 = CL("LIMBURGER ROYALE")
        End Select
        tmp2 = ""
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize)Then
        if(Text0 = "_")Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1)
        End If

        if(Text2 = "_")Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1)Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "")Then
        PlaySound(dqSound(dqHead))
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False
    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i))Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead))
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i)- 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1)- deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i)- 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i))
                    End If
            End Select

            if(dqText(i, dqHead) <> "_")Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0))and(deCount(1) = deCountEnd(1))and(deCount(2) = deCountEnd(2))Then

        if(dqTimeOn(dqHead) = 0)Then
            DMDFlush()
        Else
            if(BlinkEffect = True)Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr) > Space(20)Then
            TempStr = Left(TempStr, Space(20))
        Else
            if(Len(TempStr) < 20)Then
                TempStr = TempStr & Space(20 - Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString
    NumString = CStr(abs(Num))
    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 128) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    If Len(NumString1) + Len(NumString2) < 20 Then
        Temp = 20 - Len(NumString1)- Len(NumString2)
        TempStr = NumString1 & Space(Temp) & NumString2
        FL = TempStr
    End If
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) > 20 Then NumString = Left(NumString, 20)
    Temp = (20 - Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString) > 20 Then NumString = Left(NumString, 20)
    Temp = 20 - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 20 to 39
                DMDDisplayChar mid(dLine(1), digit -19, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
            Digits(40).ImageA = dLine(2)
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, DigitsBack, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = ""        '!
    Chars(34) = ""        '"
    Chars(35) = ""        '#
    Chars(36) = ""        '$
    Chars(37) = ""        '%
    Chars(38) = ""        '&
    Chars(39) = ""        ''
    Chars(40) = ""        '(
    Chars(41) = ""        ')
    Chars(42) = ""        '*
    Chars(43) = ""        '+
    Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    Chars(47) = ""        '/
    Chars(48) = "d_0"     '0
    Chars(49) = "d_1"     '1
    Chars(50) = "d_2"     '2
    Chars(51) = "d_3"     '3
    Chars(52) = "d_4"     '4
    Chars(53) = "d_5"     '5
    Chars(54) = "d_6"     '6
    Chars(55) = "d_7"     '7
    Chars(56) = "d_8"     '8
    Chars(57) = "d_9"     '9
    Chars(60) = "d_less"  '<
    Chars(61) = ""        '=
    Chars(62) = "d_more"  '>
    Chars(64) = ""        '@
    Chars(65) = "d_a"     'A
    Chars(66) = "d_b"     'B
    Chars(67) = "d_c"     'C
    Chars(68) = "d_d"     'D
    Chars(69) = "d_e"     'E
    Chars(70) = "d_f"     'F
    Chars(71) = "d_g"     'G
    Chars(72) = "d_h"     'H
    Chars(73) = "d_i"     'I
    Chars(74) = "d_j"     'J
    Chars(75) = "d_k"     'K
    Chars(76) = "d_l"     'L
    Chars(77) = "d_m"     'M
    Chars(78) = "d_n"     'N
    Chars(79) = "d_o"     'O
    Chars(80) = "d_p"     'P
    Chars(81) = "d_q"     'Q
    Chars(82) = "d_r"     'R
    Chars(83) = "d_s"     'S
    Chars(84) = "d_t"     'T
    Chars(85) = "d_u"     'U
    Chars(86) = "d_v"     'V
    Chars(87) = "d_w"     'W
    Chars(88) = "d_x"     'X
    Chars(89) = "d_y"     'Y
    Chars(90) = "d_z"     'Z
    Chars(94) = "d_up"    '^
    '    Chars(95) = '_
    Chars(96) = ""
    Chars(97) = ""  'a
    Chars(98) = ""  'b
    Chars(99) = ""  'c
    Chars(100) = "" 'd
    Chars(101) = "" 'e
    Chars(102) = "" 'f
    Chars(103) = "" 'g
    Chars(104) = "" 'h
    Chars(105) = "" 'i
    Chars(106) = "" 'j
    Chars(107) = "" 'k
    Chars(108) = "" 'l
    Chars(109) = "" 'm
    Chars(110) = "" 'n
    Chars(111) = "" 'o
    Chars(112) = "" 'p
    Chars(113) = "" 'q
    Chars(114) = "" 'r
    Chars(115) = "" 's
    Chars(116) = "" 't
    Chars(117) = "" 'u
    Chars(118) = "" 'v
    Chars(119) = "" 'w
    Chars(120) = "" 'x
    Chars(121) = "" 'y
    Chars(122) = "" 'z
    Chars(123) = "" '{
    Chars(124) = "" '|
    Chars(125) = "" '}
    Chars(126) = "" '~
    'used in the FormatScore function
    Chars(176) = "d_0a" '0.
    Chars(177) = "d_1a" '1.
    Chars(178) = "d_2a" '2.
    Chars(179) = "d_3a" '3.
    Chars(180) = "d_4a" '4.
    Chars(181) = "d_5a" '5.
    Chars(182) = "d_6a" '6.
    Chars(183) = "d_7a" '7.
    Chars(184) = "d_8a" '8.
    Chars(185) = "d_9a" '9.
End Sub

'*******************************************
'	ZTIM: Timers
'*******************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	'Add animation stuff here
	RollingUpdate   		'update rolling sounds
	'DoDTAnim				'handle drop target animations
	'DoSTAnim				'handle stand up target animations
	'BSUpdate
	doorp.Roty = - DoorF.CurrentAngle + 90
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 12 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 1
Const orange = 2
Const amber = 3
Const yellow = 4
Const darkgreen = 5
Const green = 6
Const blue = 7
Const darkblue = 8
Const purple = 9
Const white = 10
Const teal = 11
Const ledwhite = 12

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 8, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white 'bulb
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 197, 143)
            n.colorfull = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers, use FlashForMs
    Select Case col
        Case red
            n.color = RGB(255, 0, 0)
        Case orange
            n.color = RGB(255, 64, 0)
        Case amber
            n.color = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 192)
        Case white 'bulb
            n.color = RGB(255, 197, 143)
        Case teal
            n.color = RGB(2, 128, 126)
         Case ledwhite
            n.color = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n)
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    Dim obj
    RainbowTimer.Enabled = 0
    RainbowTimer.Enabled = 0
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen > 255 then
                rGreen = 255
                RGBStep = 1
            End If
        Case 1 'Red
            rRed = rRed - RGBFactor
            If rRed < 0 then
                rRed = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            rBlue = rBlue + RGBFactor
            If rBlue > 255 then
                rBlue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            rGreen = rGreen - RGBFactor
            If rGreen < 0 then
                rGreen = 0
                RGBStep = 4
            End If
        Case 4 'Red
            rRed = rRed + RGBFactor
            If rRed > 255 then
                rRed = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            rBlue = rBlue - RGBFactor
            If rBlue < 0 then
                rBlue = 0
                RGBStep = 0
            End If
    End Select
    For each obj in RainbowLights
        obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
        obj.colorfull = RGB(rRed, rGreen, rBlue)
    Next
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "        MASONOU", "    BIKER MICE", "dmd_credits", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "dmd_logo", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGH SCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGH SCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGH SCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    DMDScoreNow
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqFlasher.UpdateInterval = 150
    LightSeqFlasher.Play SeqRandom, 10, , 50000
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

'************************************
'       LUT - Darkness control	' retired, using User Options Menu
' 10 normal level & 10 warmer levels 
'************************************
'
'Dim bLutActive, LUTImage
'
'Sub LoadLUT
'    bLutActive = False
'    x = LoadValue(cGameName, "LUTImage")
'    If(x <> "")Then LUTImage = x Else LUTImage = 0
'    UpdateLUT
'End Sub
'
'Sub SaveLUT
'    SaveValue cGameName, "LUTImage", LUTImage
'End Sub
'
'Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub
'
'Sub UpdateLUT
'    Select Case LutImage
'        Case 0:table1.ColorGradeImage = "LUT0"
'        Case 1:table1.ColorGradeImage = "LUT1"
'        Case 2:table1.ColorGradeImage = "LUT2"
'        Case 3:table1.ColorGradeImage = "LUT3"
'        Case 4:table1.ColorGradeImage = "LUT4"
'        Case 5:table1.ColorGradeImage = "LUT5"
'        Case 6:table1.ColorGradeImage = "LUT6"
'        Case 7:table1.ColorGradeImage = "LUT7"
'        Case 8:table1.ColorGradeImage = "LUT8"
'        Case 9:table1.ColorGradeImage = "LUT9"
'        Case 10:table1.ColorGradeImage = "LUT10"
'        Case 11:table1.ColorGradeImage = "LUT Warm 0"
'        Case 12:table1.ColorGradeImage = "LUT Warm 1"
'        Case 13:table1.ColorGradeImage = "LUT Warm 2"
'        Case 14:table1.ColorGradeImage = "LUT Warm 3"
'        Case 15:table1.ColorGradeImage = "LUT Warm 4"
'        Case 16:table1.ColorGradeImage = "LUT Warm 5"
'        Case 17:table1.ColorGradeImage = "LUT Warm 6"
'        Case 18:table1.ColorGradeImage = "LUT Warm 7"
'        Case 19:table1.ColorGradeImage = "LUT Warm 8"
'        Case 20:table1.ColorGradeImage = "LUT Warm 9"
'        Case 21:table1.ColorGradeImage = "LUT Warm 10"
'    End Select
'End Sub
'
'Dim GiIntensity
'GiIntensity = 1   'can be used by the LUT changing to increase the GI lights when the table is darker
'
'Sub ChangeGiIntensity(factor) 'changes the intensity scale
'    Dim bulb
'    For each bulb in aGiLights
'        bulb.IntensityScale = GiIntensity * factor
'    Next
'End Sub
'
'' New LUT postit
'Function GetHSChar(String, Index)
'    Dim ThisChar
'    Dim FileName
'    ThisChar = Mid(String, Index, 1)
'    FileName = "PostIt"
'    If ThisChar = " " or ThisChar = "" then
'        FileName = FileName & "BL"
'    ElseIf ThisChar = "<" then
'        FileName = FileName & "LT"
'    ElseIf ThisChar = "_" then
'        FileName = FileName & "SP"
'    Else
'        FileName = FileName & ThisChar
'    End If
'    GetHSChar = FileName
'End Function
'
'Sub SetLUTLine(String)
'    Dim Index
'    Dim xFor
'    Index = 1
'    LUBack.imagea="PostItNote"
'    For xFor = 1 to 40
'        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
'        Index = Index + 1
'    Next
'End Sub
'
'Sub HideLUT
'SetLUTLine ""
'LUBack.imagea="PostitBL"
'End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim LaneBonus
Dim TargetBonus
Dim RampBonus
Dim BumperValue(4)
Dim BumperHits
Dim SuperBumperHits
Dim SpinnerValue(4)
Dim MonstersKilled(4)
Dim SpinCount
Dim RampHits3
Dim RampHits12
Dim OrbitHits
Dim TargetHits7
Dim TargetHits8
Dim CaptiveBallHits
Dim LightHits9
Dim LightHits11
Dim loopCount
Dim BattlesWon(4)
Dim Battle(4, 15) '12 battles, 1 final battle
Dim NewBattle
Dim PowerupHits

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    ResetToys
	ChangeSong 'Play some Music
    'Init Variables
    LaneBonus = 0 'it gets deleted when a new ball is launched
    TargetBonus = 0
    RampBonus = 0
    BumperHits = 0
    For i = 1 to 4
        SkillshotValue(i) = 500000
        Jackpot(i) = 100000
        MonstersKilled(i) = 0
        BallsInLock(i) = 0
        SpinnerValue(i) = 1000
        BumperValue(i) = 210 'start at 210 and every 30 hits its value is increased by 500 points
    Next
    ResetBattles
    SpinCount = 0
    SuperBumperHits = 0
    RampHits3 = 0
    RampHits12 = 0
    OrbitHits = 0
    TargetHits7 = 0
    TargetHits8 = 0
    CaptiveBallHits = 0
    loopCount = 0
    PowerupHits = 0
    LightHits9 = 0
    LightHits11 = 0
    'Init Delays/Timers
    'MainMode Init()
    'Init lights
    TurnOffPlayfieldLights()
    OpenDoor	' opens glowing swamp invisible door, entry starts battles
	CloseDoor	' will only close when BattleMode = 1 (Hard)
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball is drained
    ResetSkillShotTimer_Timer
    StopBattle
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    Dim i
    LaneBonus = 0
    TargetBonus = 0
    RampBonus = 0
    BumperHits = 0
    ' select a battle
    SelectBattle
End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
' UpdatePFXLights(PlayfieldMultiplier(CurrentPlayer)) 'ensure the multiplier is displayed right
Light13.State = 0
Light16.State = 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Setup and updates the skillshot lights
    LightSeqSkillshot.Play SeqAllOff
	Light54.State = 2	' Swamp Thing Heart -- for effect only
    ' set blinking lights to indicate Skillshot areas
	Select Case Int(Rnd*2)+1 	' make light random
        Case 1 :
			Light17.State = 2	' left inlane Face blink
			Light18.State = 0	' right inlane Face off
			Light48.State = 2	' Captive Ball light blink
			SkillShotLane = 1	' sets lane number for award
        Case 2 :
			Light18.State = 2	' right inlane Face blink
			Light17.State = 0	' left inlane Face off
			Light48.State = 2	' Captive Ball light blink
			SkillShotLane = 6	' sets lane number for award
		End Select
    Gate2.Open = 1
    Gate3.Open = 1
    DMD CL("HIT LIT LIGHT"), CL("FOR SKILLSHOT"), "", eNone, eNone, eNone, 1500, True, ""
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
	Light18.State = 0	' right inlane Face
    Light17.State = 0	' left inlane Face 
	Light48.State = 0	' Captive Ball
    Light54.State = 0	' Swamp Thing Heart
    Gate2.Open = 0
    Gate3.Open = 0
	SkillShotLane = 0	' clears lane number for award
    DMDScoreNow
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' All Target_Hit Subs will...
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

' Tree animation
'Dim MyPi, TreeStep, TreeDir
'MyPi = Round(4 * Atn(1), 6) / 90
'TreeStep = 0
'
'Sub Trees_Timer()
'    TreeDir = SIN(TreeStep * MyPi)
'    TreeStep = (TreeStep + 1)MOD 360
'    Tree1.RotY = - TreeDir
'    Tree2.RotY = TreeDir
'    Tree3.RotY = - TreeDir
'    Tree4.RotY = TreeDir
'    Tree5.RotY = - TreeDir
'    Tree6.RotY = TreeDir
'    Tree7.RotY = - TreeDir
'    Tree8.RotY = TreeDir
'    Tree9.RotY = - TreeDir
'    Tree10.RotY = TreeDir
'    Tree11.RotY = - TreeDir
'    Tree12.RotY = TreeDir
'    Tree15.RotY = - TreeDir
'plant001.RotY = TreeDir*10
'plant002.RotY = TreeDir*10
'plant003.RotY = -TreeDir*10
'plant004.RotY = -TreeDir*10
'plant005.RotY = -TreeDir*10
'grass001.RotY = - TreeDir*4
'grass002.RotY = TreeDir*4
'grass003.RotY = - TreeDir*4
'grass004.RotY = TreeDir*4
'grass005.RotY = - TreeDir*4
'grass006.RotY = TreeDir*4
'grass007.RotY = - TreeDir*4
'grass008.RotY = TreeDir*4
'grass009.RotY = TreeDir*4
'grass001t.RotZ = TreeDir
'grass002t.RotZ = TreeDir
'grass003t.RotZ = TreeDir
'
'End Sub

'*********************************************************
' Slingshots has been hit
' In this table the slingshots change the outlanes lights

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Lemk
    DOF 103,2
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 210
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
    ChangeOutlanes
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    DOF 104,2
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 210
    ' add some effect to the table?
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
    ChangeOutlanes
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub ChangeOutlanes
    Dim tmp
    tmp = light13.State
    light13.State = light16.State
    light16.State = tmp
End Sub

'*********
' Bumpers
'*********
' after each 30 hits the bumpers increase their score value by 500 points up to 3210
' and they increase the playfield multiplier.

Sub Bumper1_Hit
    If NOT Tilted Then
        RandomSoundBumperTop Bumper1
		DOF 109,2
        DOF 138, DOFPulse
        ' add some points
        AddScore BumperValue(CurrentPlayer)
		FlBumperFadeTarget(5) = 1 
		Bumper5.timerenabled = True
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper1"
    End If
    CheckBumpers
End Sub
Sub Bumper5_Timer
	FlBumperFadeTarget(5) = 0
End Sub

Sub Bumper2_Hit
    If NOT Tilted Then
        RandomSoundBumperMiddle Bumper2
		DOF 110,2
        DOF 140, DOFPulse
        ' add some points
        AddScore BumperValue(CurrentPlayer)
		FlBumperFadeTarget(4) = 1 
		Bumper4.timerenabled = True
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper2"
    End If
    CheckBumpers
End Sub

Sub Bumper4_Timer
	FlBumperFadeTarget(4) = 0
End Sub

Sub Bumper3_Hit
    If NOT Tilted Then
        RandomSoundBumperBottom Bumper3
		DOF 107,2
        DOF 137, DOFPulse
        ' add some points
        AddScore BumperValue(CurrentPlayer)
		FlBumperFadeTarget(1) = 1
		Bumper1.timerenabled = True
        If Battle(CurrentPlayer, 0) = 2 Then
            SuperBumperHits = SuperBumperHits + 1
            Addscore 5000
            CheckWinBattle
        End If
        ' remember last trigger hit by the ball
        LastSwitchHit = "Bumper3"
    End If
    CheckBumpers
End Sub

Sub Bumper1_Timer
	FlBumperFadeTarget(1) = 0
End Sub
' Check the bumper hits

Sub CheckBumpers()
    ' increase the bumper hit count and increase the bumper value after each 30 hits
    BumperHits = BumperHits + 1
	pupevent 816
    If BumperHits MOD 30 = 0 Then
        If BumperValue(CurrentPlayer) < 3210 Then
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
        End If
        ' lit the playfield multiplier light
        light54.State = 1
    End If
End Sub

'*************************
' Top & Inlanes: Bonus X
'*************************
' lit the 2 top lane lights and the 2 inlane lights to increase the bonus multiplier

Sub sw1_Hit
    DOF 128, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light17.State = 1
    FlashForMs f8, 1000, 50, 0
    If bSkillShotReady Then
        ResetSkillShotTimer_Timer
    Else
        CheckBonusX
    End If
End Sub

Sub sw6_Hit
    DOF 129, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light18.State = 1
    FlashForMs f8, 1000, 50, 0
    If bSkillShotReady Then
		If SkillShotLane = 1 Then Awardskillshot : End If
    Else
        CheckBonusX
    End If
End Sub

Sub sw4_Hit
    DOF 133, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light14.State = 1
    FlashForMs f6, 1000, 50, 0
    AddScore 5000
    If bSkillShotReady Then
		If SkillShotLane = 6 Then Awardskillshot : End If
        Awardskillshot
    Else
        CheckBonusX
    End If
End Sub


Sub sw3_Hit
    DOF 134, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    Light15.State = 1
    FlashForMs f7, 1000, 50, 0
    AddScore 5000
    CheckBonusX
' Do some sound or light effect
End Sub

Sub CheckBonusX
    If Light17.State + Light18.State + Light14.State + Light15.State = 4 Then
        AddBonusMultiplier 1
        GiEffect 1
        FlashForMs Light17, 1000, 50, 0
        FlashForMs Light18, 1000, 50, 0
        FlashForMs Light14, 1000, 50, 0
        FlashForMs Light15, 1000, 50, 0
    End IF
End Sub

'************************************
' Flipper OutLanes: Virtual kickback
'************************************
' if the light is lit then activate the ballsave

Sub sw2_Hit
    DOF 132, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    AddScore 50000
    ' Do some sound or light effect
    ' do some check
    If light13.State = 1 Then
        EnableBallSaver 5
    End If
End Sub

Sub sw5_Hit
    DOF 135, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    AddScore 50000
    ' Do some sound or light effect
    ' do some check
    If Light16.State = 1 Then
        EnableBallSaver 5
    End If
End Sub

'************
'  Spinners
'************
'dim BackWallFile
'BackWallFile = 0  ' set initially backwall image  to 0 (first file in Image Manager)
'Sub BackWallChange
'	debug.print "back wall change :: backwall-" & BackWallFile
'	BackWallFile = BackWallFile + 1
'	If BackWallFile > 4 Then BackWallFile = 0   ' ensure you set greater-than number correctly based on how many images you have.first image should be a zero (0)... backwall-0
'	Wall54.SideImage = "backwall-" & BackWallFile
'End Sub

Sub spinner1_Spin
    If Tilted Then Exit Sub
    Addscore spinnervalue(CurrentPlayer)
    SoundSpinner Spinner1
    DOF 136, DOFPulse
'	BackWallChange ' change backwall image
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
			'debug.print "Spin Count : " & SpinCount
            CheckWinBattle
    End Select
End Sub

Sub spinner2_Spin
    If Tilted Then Exit Sub
    SoundSpinner Spinner2
    DOF 124, DOFPulse
'	BackWallChange ' change backwall image
    Addscore spinnervalue(CurrentPlayer)
    Select Case Battle(CurrentPlayer, 0)
        Case 1
            Addscore 3000
            SpinCount = SpinCount + 1
			'debug.print "Spin Count : " & SpinCount
            CheckWinBattle
    End Select
End Sub

'*********************************
'   Lock Targets (large targets)
'*********************************

Sub Target13_Hit ' Large Target Left-Side
	Dim delay
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), Target10
	'shakeCROC
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light19.State = 1	' on to indicate hit
		' Light31.State = 1	' battles control, do not set for  this light
    FlashForMs f4, 1000, 50, 0
    ' do some check
    Check2BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5	' changed to only Skull Lights, this is a target light
'            If Light31.State = 2 Then
'                Light31.State = 0
'                Addscore 100000
'                CheckWinBattle
'            End If
        Case 6  ' Hit the Moving Light
			'debug.print "Target13_Hit"		
            If Light31.State = 2 Then
                Light31.State = 0
				Light20.State = 0	' off to indicate hit
                Light32.State = 2
				Addscore 100000
            End If
        Case 8
			TargetHits8 = TargetHits8 + 1
			Addscore 25000
			CheckWinBattle
			' turn lights to blink until battle is over
			Light19.State = 2
			Light31.State = 2
        Case 9
            If Light31.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 11
            If Light31.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target13"
End Sub

Sub Target1_Hit	' Large Target Right-Side
	Dim delay
	'debug.print "Target1_Hit"
    PlaySoundAt SoundFXDOF("fx_target", 116, DOFPulse, DOFTargets), Target1
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    ' Do some sound or light effect
    Light20.State = 1	' on to indicate hit
	    'Light29.State = 1	' battles control, do not set for  this light
    FlashForMs f5, 1000, 50, 0
    ' do some check
    Check2BankTargets
    Select Case Battle(CurrentPlayer, 0)
        Case 5	' changed to just Skulls
'            If Light29.State = 2 Then
'                Light29.State = 0
'                Addscore 100000
'                CheckWinBattle
'            End If
        Case 6    ' Hit the Moving Light
			'debug.print "case 6 : Target1_Hit"
            If Light29.State = 2 Then
                Light29.State = 0
				Light20.State = 0	' off to indicate hit
				Light30.State = 2	' turn on the next light
                Addscore 100000
            End If
        Case 8
			TargetHits8 = TargetHits8 + 1
			Addscore 25000
			CheckWinBattle
			' turn lights to blink until battle is over
			Light19.State = 2
			Light31.State = 2
        Case 9
            If Light29.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 11
            If Light29.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target1"
End Sub

Sub Check2BankTargets
    If light19.state + light20.state = 2 Then
        light19.state = 0
        light20.state = 0
        LightEffect 1
        FlashEffect 1
        Addscore 20000
        If(Light46.State = 0)AND(bMultiballMode = FALSE)Then 'lit the lock light if it is off, open the glowing swamp invisible door and activate the lock switch
            Light46.State = 1
            openDoor
            'PlaySound "vo_lockislit"
            DMD "_", CL("LOCK IS LIT"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
        ElseIf light53.State = 0 Then 'lit the increase jackpot light if the lock light is lit
            light53.State = 1
        'PlaySound "vo_IncreaseJakpot"
        Else
            Addscore 30000
        End If
    End If
End Sub

'**************************
' Lock: Main Multiball
'**************************
' Lock is a virtual lock; holds locked ball count

Sub Door_Hit
    ' PlaySoundAt "fx_woodhit", doorf  'retired: not using wood door
    PlaySoundAt "fx_splat", doorf  ' changed door to splash into radioactive swamp
    OpenDoor
End Sub

Sub Lock_Hit
    Dim delay
    delay = 500
    ' PlaySoundAt "fx_hole_enter", lock
	PlaySoundAt "fx_splat", lock  ' changed door to splash into radioactive swamp
	'shakeSWAMPTHING
    bsJackal.AddBall Me
    CloseDoor
    If(bJackpot = True)AND(light45.State = 2)Then
        light45.State = 0
        AwardJackpot
    End If
    If light46.State = 1 Then 'lock the ball
        BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
        delay = 4000
        Select Case BallsInLock(CurrentPlayer)
            Case 1:DMD "_", CL("BALL 1 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_ball1locked"
            Case 2:DMD "_", CL("BALL 2 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_ball2locked"
            Case 3:DMD "_", CL("BALL 3 LOCKED"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_ball3locked"
        End Select
        light46.State = 0
        If BallsInLock(CurrentPlayer) = 3 Then 'start multiball
            vpmtimer.addtimer 2000, "StartMainMultiball '" 
        End If
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light37.State = 2 Then
                Light37.State = 0
                Addscore 100000
                CheckWinBattle
                Delay = 1000
            End If
        Case 6    ' Hit the Moving Light
			'debug.print "Lock_Hit"
            If Light37.State = 2 Then
                Light37.State = 0
                Addscore 100000
                WinBattle  ' last in sequence, hit this and Win the battle
                Delay = 1000
            End If
        Case 9
            If Light37.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 11
            If Light37.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    If(Battle(CurrentPlayer, NewBattle) = 2)AND(Battle(CurrentPlayer, 0) = 0)Then 'the battle is ready, so start it
        vpmtimer.addtimer 2000, "StartBattle '"
        delay = 6000
    End If
    vpmtimer.addtimer delay, "JackalExit '"
End Sub

Sub StartMainMultiball
    AddMultiball 3
    DMD "_", CL("MULTIBALL"), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_multiball" : pupevent 815
    StartJackpots
    ChangeGi 5
    'reset BallsInLock variable
    BallsInLock(CurrentPlayer) = 0
End Sub

Sub OpenDoor
    doorf.RotateToEnd
    door.IsDropped = 1
End Sub

Sub CloseDoor
	If BattleStartMode = 1 then Exit Sub
    doorf.RotateToStart
    door.IsDropped = 0
End Sub

'**********
' Jackpots
'**********
' Jackpots are enabled during the Main Multiball and Battles
' Tree IDs, left to right numbering
''top row
'Light44	' top row 1 - Jackpot
'Light45 	' top row 2 - Jackpot
''mid row
'Light41	' mid row 1 - Jackpot
'Light40 	' mid row 2 - Jackpot
'Light43 	' mid row 3 - Power Up
'Light46 	' mid row 4 - Lock
'Light48 	' mid row 5 - Super Jackpot
'Light50 	' mid row 6 - Jackpot
'Light51 	' mid row 7 - Jackpot
''low row
'Light42 	' low row 1 - Jackpot
'
Sub StartJackpots
    bJackpot = true
    'turn on the jackpot lights

    Select Case Battle(CurrentPlayer, 0)
        Case 9 ' Targets
            light44.State = 2	' tree - left ramp
            light50.State = 2	' tree - right ramp
			' Target13, Target1
        Case 10 'Caves
            light42.State = 2	' tree - CrocMan Cave
            light40.State = 2	' tree - Gator Cave
            light45.State = 2	' tree - radioactive swamp
        Case 11 ' Ramps
            light44.State = 2	' tree - left ramp
            light50.State = 2	' tree - right ramp
        Case 12 ' Ramps and Orbit Loops :: 4 jackpots
            light44.State = 2	' tree - left ramp
            light50.State = 2	' tree - right ramp
            light41.State = 2	' tree - left orbit
            light51.State = 2	' tree - right orbit
                'Light33	' orbit left skull
				'Light32	' orbit right skull
				'Light36	' ramp left skull
                'Light34	' ramp right skull
        Case 13 ' Final Battle - all 7 Jackpots on
            light44.State = 2
            light45.State = 2
            light41.State = 2
            light40.State = 2
            light42.State = 2
            light50.State = 2
            light51.State = 2
        Case Else
            If bMultiballMode Then
                light43.State = 2	' tree, Power Up
                light52.State = 2	' Swamp Thing 'heart' light BLINKS with Multiball
			End If
    End Select
End Sub

Sub ResetJackpotLights 'when multiball is finished, resets jackpot and superjackpot lights
    bJackpot = False
    light42.State = 0
    light41.State = 0
    light40.State = 0
    light44.State = 0
    light45.State = 0
    light50.State = 0
    light51.State = 0
End Sub

Sub EnableSuperJackpot
    If bJackpot = True Then
        If light42.State + light41.State + light40.State + light44.State + light45.State + light50.State + light51.State = 0 Then
            'PlaySound "vo_superjackpotislit"
            light48.State = 2	' mid row 5 - Super Jackpot
            light39.State = 2	' Swamp Thing heart
        End If
    End If
End Sub

'***********************************
' Small Targets - center playfield
'***********************************

Sub Target2_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target2"
    ' Do some sound or light effect
    Light23.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
    End Select
    Check6BankTargets
End Sub

Sub Target4_Hit ' Flaying Gator
    PlaySoundAtBall SoundFXDOF("fx_target", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target4"
    ' Do some sound or light effect
    Light24.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
    End Select
    Check6BankTargets
End Sub

Sub Target5_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target5"
    ' Do some sound or light effect
    Light25.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
    End Select
    Check6BankTargets
End Sub

Sub Target7_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target7"
    ' Do some sound or light effect
    Light26.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
    End Select
    Check6BankTargets
End Sub

Sub Target10_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target10"
    ' Do some sound or light effect
    Light27.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
    End Select
    Check6BankTargets
End Sub

Sub Target8_Hit ' Anton
    PlaySoundAtBall SoundFXDOF("fx_target", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    AddScore 5000
    TargetBonus = TargetBonus + 1
    LastSwitchHit = "Target8"
    ' Do some sound or light effect
    Light28.State = 1
    ' do some check
    Select Case Battle(CurrentPlayer, 0)
        Case 7:TargetHits7 = TargetHits7 + 1:Addscore 10000:CheckWinBattle
    End Select
    Check6BankTargets
End Sub

Sub Check6BankTargets
    Dim tmp
    FlashForMs f1, 1000, 50, 0
    FlashForMs f3, 1000, 50, 0
    FlashForMs f4, 1000, 50, 0
    FlashForMs f4, 1000, 50, 0
    tmp = INT(RND * 2) + 1   ' sound files enemy_ 1 through 24
    PlaySoundAtBall "enemy_" &tmp
    ' if all 6 targets are hit then kill a monster & activate the mystery light
    If light23.state + light24.state + light25.state + light26.state + light27.state + light28.state = 6 Then
        ' kill a monster
        MonstersKilled(CurrentPlayer) = MonstersKilled(CurrentPlayer) + 1
        DMD " TARGET", "  ELIMINATED", "monster_" &tmp, eNone, eNone, eNone, 2500, True, ""
        LightEffect 1
        FlashEffect 1
        ' Lit the Mystery light if it is off
        If Light38.State = 2 Then
            AddScore 50000
        Else
            Light38.State = 2
            AddScore 25000
        End If
        ' reset the lights
        light23.state = 0
        light24.state = 0
        light25.state = 0
        light26.state = 0
        light27.state = 0
        light28.state = 0
    End If
End Sub

' Playfiel Multiplier timer: reduces the multiplier after 30 seconds

Sub pfxtimer_Timer
    If PlayfieldMultiplier(CurrentPlayer) > 1 Then
        PlayfieldMultiplier(CurrentPlayer) = PlayfieldMultiplier(CurrentPlayer)-1
        SetPlayfieldMultiplier PlayfieldMultiplier(CurrentPlayer)
    Else
        pfxtimer.Enabled = 0
    End If
End Sub

'**************************************************
'  Hidden Target - under Super Jackpot wall
'**************************************************

Sub Target9_Hit
    PlaySoundAtBall SoundFXDOF("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    If bSkillShotReady Then
        Awardskillshot
        Exit Sub
    End If
    AddScore 5000 'all targets score 5000
    ' Do some sound or light effect
    ' do some check
    If(bJackpot = True)AND(light39.State = 2)Then
        AwardSuperJackpot
        light39.State = 0	' turn off Swamp Thing Heart light
        light48.State = 0
        StartJackpots
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 0:SelectBattle 'no battle is active then change to another battle
    End Select

    ' increase the playfield multiplier for 30 seconds
    If light54.State = 1 Then
        AddPlayfieldMultiplier 1
        light54.State = 0
    End If

    ' increase Jackpot
    If light53.State = 1 Then
        AddJackpot 50000
        light53.State = 0
    End If
End Sub

'******************************************
'  Flying Aligator - Hole Hit & Awards
'******************************************

Sub JackalHole_Hit
    Dim Delay
    Delay = 200
    PlaySoundAt "fx_hole_enter", JackalHole
	'flyGATOR
    bsJackal.AddBall Me
    If NOT Tilted Then
        ' do something
        If(bJackpot = True)AND(light40.State = 2)Then
            light40.State = 0
            AwardJackpot
            Delay = 2000
        End If
        If light38.State = 2 Then ' mystery light is lit
            light38.State = 0
            GiveRandomAward
            Delay = 3500
        End If
        If light39.State = 2 Then ' extra ball is lit
            light39.State = 0
            AwardExtraBall
            Delay = 2000
        End If
        Select Case Battle(CurrentPlayer, 0)
            Case 5
                If Light35.State = 2 Then
                    Light35.State = 0
                    Addscore 100000
                    CheckWinBattle
                    Delay = 1000
                End If
            Case 6	  ' Hit the Moving Light
				'debug.print "JackalHole_Hit"
				If Light35.State = 2 Then
                    Light35.State = 0
                    Light36.State = 2
                    Addscore 100000
                End If
            Case 9
                If Light35.State = 2 Then
                    AddScore 100000
                    FlashEffect 3
                    DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
                End If
            Case 11
                If Light35.State = 2 Then
                    AddScore 120000
                    FlashEffect 3
                    DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
                End If
        End Select
    End If
    vpmtimer.addtimer Delay, "JackalExit '"
End Sub

Sub JackalExit()
    If bsJackal.Balls > 0 Then
        FlashForMs f1, 1000, 50, 0
        PlaySoundAt SoundFXDOF("fx_kicker", 119, DOFPulse, DOFContactors), JackalHole
        DOF 121, DOFPulse	' strobe
		DOF 139, DOFPulse	' shaker
        PlaySoundAt "fx_Explosion02", JackalHole
        'add a small delay before actually kicking the ball
        vpmtimer.addtimer 500, "bsJackal.ExitSol_On '"
    End If
    'kick out all the balls
    If bsJackal.Balls > 0 Then
        vpmtimer.Addtimer 500, "JackalExit '"
    End If
End Sub

Sub GiveRandomAward()
    Dim tmp, tmp2

    ' show some random values on the dmd
    DMD CL("BIKER AWARD"), "", "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("PLAYFIELD X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA BALL"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BONUS X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("SPINNER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("PLAYFIELD X"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("BUMPER VALUE"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA POINTS"), "", eNone, eNone, eNone, 50, False, "fx_spinner"
    DMD "_", CL("EXTRA BALL"), "", eNone, eNone, eNone, 50, False, "fx_spinner"

    tmp = INT(RND(1) * 80)
    Select Case tmp
        Case 1, 2, 3, 4, 5, 6 'Lit Extra Ball
            DMD "", CL("EXTRA BALL IS LIT"), "", eNone, eBlink, eNone, 1500, True, "vo_extraball"
            light39.State = 2
        Case 7, 8, 13, 14, 15 '100,000 points
            DMD CL("BIG POINTS"), CL("100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            AddScore 100000
        Case 9, 10, 11, 12 'Hold Bonus
            DMD CL("BONUS HELD"), CL("ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            bBonusHeld = True
        Case 16, 17, 18 'Increase Bonus Multiplier
            DMD CL("INCREASED"), CL("BONUS X"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            AddBonusMultiplier 1
        Case 19, 20, 21 'Complete Battle
            If Battle(CurrentPlayer, 0) > 0 AND Battle(CurrentPlayer, 0) < 13 Then
                DMD CL("BATTLE"), CL("COMPLETED"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
                WinBattle
            Else
                DMD CL("BIG POINTS"), CL("100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
                AddScore 100000
            End If
        Case 22, 23, 36, 37, 38 'PlayField multiplier
            DMD CL("INCREASED"), CL("PLAYFIELD X"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            AddPlayfieldMultiplier 1
        Case 24, 25, 26, 27, 28 '100,000 points
            DMD CL("BIG POINTS"), CL("100000"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            AddScore 100000
        Case 29, 30, 31, 32, 33, 34, 35 'Increase Bumper value
            BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
            DMD CL("BUMPER VALUE"), CL(BumperValue(CurrentPlayer)), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
        Case 39, 40, 43, 44 'extra multiball
            DMD CL("EXTRA"), CL("MULTIBALL"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            AddMultiball 1
        Case 45, 46, 47, 48 ' Ball Save
            DMD CL("BALL SAVE"), CL("ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            EnableBallSaver 20
        Case 49, 50, 51, 52
            DMD CL("OUTLANE SAVER"), CL("ACTIVATED"), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            Light13.State = 1
        Case ELSE 'Add a Random score from 10.000 to 100,000 points
            tmp2 = INT((RND) * 9) * 10000 + 10000
            DMD CL("EXTRA POINTS"), CL(tmp2), "", eBlink, eBlink, eNone, 1500, True, "fx_thunder7"
            AddScore tmp2
    End Select
End Sub

'***************************
' Outer Orbit Loop lanes
'***************************

Sub sw8_Hit	' Left Orbit top corner
    DOF 130, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If(bJackpot = True)AND(light41.State = 2)Then
        light41.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 4:OrbitHits = OrbitHits + 1:Addscore 70000:CheckWinBattle
        Case 5
            If Light33.State = 2 Then
                Light33.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6   ' Hit the Moving Light
			'debug.print "sw8_Hit"
            If Light33.State = 2 Then
                Light33.State = 0
                Light34.State = 2
                Addscore 100000
            End If
        Case 9
            If Light33.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 10
            If LastSwitchHit = "sw7" Then
                LastSwitchHit = ""
                loopCount = loopCount + 1
                Addscore 140000
                CheckWinBattle
            End If
        Case 11
            If Light33.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 12	' Ramps & Orbits
            If Light33.State = 2 Then
                Light33.State = 0	' skull - orbit left
                RampHits12 = RampHits12 + 1
                Addscore 100000
                CheckWinBattle
            End If
    End Select
    LastSwitchHit = "sw8"
End Sub

Sub sw7_Hit	' Right Orbit upper corner switch
    DOF 131, DOFPulse
    If Tilted Then Exit Sub
    LaneBonus = LaneBonus + 1
    If(bJackpot = True)AND(light51.State = 2)Then
        light51.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 4:OrbitHits = OrbitHits + 1:Addscore 70000:CheckWinBattle
        Case 5
            If Light32.State = 2 Then
                Light32.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6   ' Hit the Moving Light
			'debug.print "sw7_Hit"
			If Light32.State = 2 Then
                Light32.State = 0
                Light33.State = 2
                Addscore 100000
            End If
        Case 9
            If Light32.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 10
            If LastSwitchHit = "sw8" Then
                LastSwitchHit = ""
                loopCount = loopCount + 1
                Addscore 140000
                CheckWinBattle
            End If
        Case 11
            If Light32.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 12
            If Light32.State = 2 Then
				Light32.State = 0' skull - orbit right
                RampHits12 = RampHits12 + 1
                Light32.State = 0
                Addscore 100000
                CheckWinBattle
            End If
    End Select
    LastSwitchHit = "sw7"
End Sub

'************************
'  Raised Ramp Loops
'************************

Sub LeftRampStart_hit()
	debug.print "Left Ramp Start"
	WireRampOn True	 ' Start Loop Ramp Sound (Fleep Plastic Ramps)
End Sub

Sub LeftRampDone_Hit
    Dim tmp
	' Ramp BallRolling FX
	WireRampOff	 ' Stop Loop Ramp Sound (Fleep Plastic Ramps)
	debug.print "Left Ramp Done"
	'debug.print "Battle " & Battle(CurrentPlayer, 0)
	'debug.print "ForceBattle | NewBattle : " & ForceBattle & " | " & NewBattle 
    If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
    If(bJackpot = True)AND(light44.State = 2)Then
        light44.State = 0
        AwardJackpot
    End If
    'PowerUp - left ramp only counts the variable
    PowerupHits = PowerupHits + 1
	pupevent 817
    CheckPowerup
    'Battles
    Select Case Battle(CurrentPlayer, 0)
        Case 3
			'debug.print "LeftRampDone_Hit case 3"
			RampHits3 = RampHits3 + 1
			Addscore 100000
			CheckWinBattle
        Case 5
            If Light36.State = 2 Then
                Light36.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6   ' Hit the Moving Light
			'debug.print "LeftRampDone_Hit"		
				If Light36.State = 2 Then
                Light36.State = 0
                Light37.State = 2
                Addscore 100000
            End If
        Case 9
            If Light36.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 11
			'debug.print "LeftRampDone_Hit case 11"
            If Light36.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 12
            If Light36.State = 2 Then
                Light36.State = 0	' left ramp entry
				RampHits12 = RampHits12 + 1
                Addscore 100000
                CheckWinBattle
            End If
        Case else
            ' play ss quote
            PlayQuote
    End Select
    'check for combos
    if LastSwitchHit = "RightRampDone" OR LastSwitchHit = "LeftRampDone" Then
        Addscore jackpot(CurrentPlayer)
        DMD CL("COMBO"), CL(jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End If
    LastSwitchHit = "LeftRampDone"
End Sub

Sub LeftRampDone_Unhit
	WireRampOn False	' Start Wire Ramp Sound
End Sub

Sub LeftRampEnd_hit()
	debug.print "Left Ramp End"
	WireRampOff	 ' Stop Wire Ramp Sound
End Sub

Sub LeftRampEnd_unhit()
	RandomSoundRampStop LeftRampEnd
End Sub

Sub RightRampStart_hit()
	debug.print "Right Ramp Start"
	WireRampOn True	 ' Start Loop Ramp Sound  (Fleep Plastic Ramps)
End Sub

Sub RightRampDone_Hit
    Dim tmp
	WireRampOff	 ' Stop Loop Ramp Sound  (Fleep Plastic Ramps)
	debug.print "Right Ramp Done"
	'debug.print "Battle " & Battle(CurrentPlayer, 0)
    'debug.print "ForceBattle | NewBattle : " & ForceBattle & " | " & NewBattle 
	If Tilted Then Exit Sub
    'increase the ramp bonus
    RampBonus = RampBonus + 1
	pupevent 817
    If(bJackpot = True)AND(light54.State = 2)Then
        light54.State = 0
        AwardJackpot
    End If
    'Powerup - right ramp counts the variable and give the jackpot if Light50 lit
    If light50.State = 2 Then
        DMD CL("POWERUP AWARD"), CL(jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, "vo_Jackpot"
        AddScore Jackpot(CurrentPlayer)
        LightEffect 2
        FlashEffect 2
    Else
        PowerupHits = PowerupHits + 1
        CheckPowerup
    End If
    'Battles
    Select Case Battle(CurrentPlayer, 0)
        Case 3
			'debug.print "RightRampDone_Hit case 3"
			RampHits3 = RampHits3 + 1
			Addscore 100000
			CheckWinBattle
        Case 5	' all Skull Lights
            If Light34.State = 2 Then
                Light34.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6   ' Hit the Moving Light
			'debug.print "RightRampDone_Hit case 6"		
			If Light34.State = 2 Then
                Light34.State = 0
                Light35.State = 2	
                Addscore 100000
            End If
        Case 9
            If Light34.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 11
			'debug.print "RightRampDone_Hit case 11"
            If Light34.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 12
            If Light34.State = 2 Then
                Light34.State = 0	' skull - ramp right
                RampHits12 = RampHits12 + 1
                Addscore 100000
                CheckWinBattle
            End If
        Case else
            ' play ss quote
            PlayQuote
    End Select

    'check for combos
    if LastSwitchHit = "RightRampDone" OR LastSwitchHit = "LeftRampDone" Then
        Addscore jackpot(CurrentPlayer)
        DMD CL("COMBO"), CL(jackpot(CurrentPlayer)), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End If
    LastSwitchHit = "RightRampDone"
End Sub

Sub RightRampDone_Unhit
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub RightRampEnd_hit()
	debug.print "Right Ramp End"
	WireRampOff	 'Exiting Wire Ramp Stop Playing Sound
End Sub

Sub RightRampEnd_unhit()
	RandomSoundRampStop RightRampEnd
End Sub

'********************************
' Croc Man - Hidden Cave Target
'********************************

Sub Target12_Hit ' Croc Man Cave
    If Tilted Then Exit Sub
    PlaySoundAtBall "fx_target"
	'shakeCROC
    If(bJackpot = True)AND(light42.State = 2)Then
        light42.State = 0
        AwardJackpot
    End If
    Select Case Battle(CurrentPlayer, 0)
        Case 5
            If Light30.State = 2 Then
                Light30.State = 0
                Addscore 100000
                CheckWinBattle
            End If
        Case 6   ' Hit the Moving Light
			'debug.print "case 6 : Target12_Hit"				
            If Light30.State = 2 Then
                Light30.State = 0
                Light31.State = 2
				Addscore 100000
            End If
        Case 9
            If Light30.State = 2 Then
                AddScore 100000
                FlashEffect 3
                LightHits9 = LightHits9 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("100000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
        Case 11
            If Light30.State = 2 Then
                AddScore 120000
                FlashEffect 3
                LightHits11 = LightHits11 + 1
                CheckWinBattle
                DMD "_", CL(FormatScore("120000")), "_", eNone, eBlinkFast, eNone, 500, True, ""
            End If
    End Select
    LastSwitchHit = "Target12"
End Sub

'************************
'       Battles
'************************

' Table has 12 main battles plus 1 final battle
' During play, players selects any of the 12 main battles
' After completing all 12 battles, the final battle is auto selected

' current active battle number is stored in Battle(CurrentPlayer,0)

Sub SelectBattle 'select a new random battle if none is active
    Dim i
    If Battle(CurrentPlayer, 0) = 0 Then
        ' reset the battles that are not finished
        For i = 1 to 5
            If Battle(CurrentPlayer, i) = 2 Then Battle(CurrentPlayer, i) = 0
        Next
        If BattlesWon(CurrentPlayer) = 5 Then
            NewBattle = 13:Battle(CurrentPlayer, NewBattle) = 2:UpdateBattleLights:StartBattle '13 battle is the wizard
        Else
            NewBattle = INT(RND * 5 + 1)
            do while Battle(CurrentPlayer, NewBattle) <> 0
                NewBattle = INT(RND * 4 + 1)
            loop
            Battle(CurrentPlayer, NewBattle) = 2
            Light52.State = 1	' Swamp Thing 'heart' light SOLID when selecting battle
            UpdateBattleLights
        End iF
    'debug.print "SelectBattle " & newbattle
    End If
End Sub

' Update SWAMP THING PF lights based on battles won
' first 10 won battles light up all letters
Sub UpdateBattleLights
	' SWAMP
	If Battle(CurrentPlayer, 3) + Battle(CurrentPlayer, 1)>0 Then Light8.State=1 : End If ' if either battle was won, light letter
    Light5.State = Battle(CurrentPlayer, 1)
    Light6.State = Battle(CurrentPlayer, 2)
    Light2.State = Battle(CurrentPlayer, 3)
    Light1.State = Battle(CurrentPlayer, 4)
	' THING
	If Battle(CurrentPlayer, 6) + Battle(CurrentPlayer, 2)>0 Then Light3.State=1 : End If ' if either battle was won, light letter
    Light4.State = Battle(CurrentPlayer, 5)
    Light10.State = Battle(CurrentPlayer, 13)
    Light9.State = Battle(CurrentPlayer, 10)
    Light7.State = Battle(CurrentPlayer, 11)
End Sub

' Starting a battle means to setup some lights and variables, maybe timers
' Battle lights will always blink during an active battle
Sub StartBattle
	debug.print "StartBattle, NewBattle : " & NewBattle
	'Start_Fog
	If ForceBattle <> 0 Then NewBattle = ForceBattle
 	debug.print "ForceBattle | NewBattle : " & ForceBattle & " | " & NewBattle 
    Battle(CurrentPlayer, 0) = NewBattle
    Light52.State = 2	' Swamp Thing 'heart' light BLINKS when starting battle
    DOF 139, DOFPulse	' shaker
    PlayQuote  ' using random quote from Swamp Thing // retired... PlaySound "fx_growl"
    ChangeSong
	EnableBallSaver 15 'start a 15 seconds ball save
    Select Case NewBattle
        Case 1 'Super Spinners
            DMD CL("VINNIE"), CL("HIT SPINNERS"), "", eNone, eNone, eNone, 1500, True, "" : pupevent 809
            Light33.State = 2	' left spinner skull (orbit lane)
            Light32.State = 2	' right spinner skull (orbit lane)
            SpinCount = 0
        Case 2 ' Pop Bumpers
            DMD CL("MODO"), CL("HIT POP BUMPERS"), "", eNone, eNone, eNone, 1500, True, "" : pupevent 810
            Light55.State = 2
            LightSeqBumpers.Play SeqRandom, 10, , 1000
            SuperBumperHits = 0
        Case 3 ' Ramp Loops
            DMD CL("THROTTLE"), CL("HIT RAMP LOOPS"), "", eNone, eNone, eNone, 1500, True, "" : pupevent 811
            Light36.State = 2	' left skull
            Light34.State = 2	' right skull
            RampHits3 = 0
        Case 4 ' Orbits
            DMD CL("KARBUNKLE"), CL("HIT ORBIT LANES"), "", eNone, eNone, eNone, 1500, True, "" : pupevent 812
            Light33.State = 2
            Light32.State = 2
            OrbitHits = 0
        Case 5 ' Shoot Skull Lights
            DMD CL("GREASEPIT"), CL("HIT GREEN LIGHTS"), "", eNone, eNone, eNone, 1500, True, "" : pupevent 813
            Light30.State = 2
            Light32.State = 2
            Light33.State = 2
            Light34.State = 2
            Light35.State = 2
            Light36.State = 2
            Light37.State = 2
			'	Light36	' top row, Left Ramp Start
			'	Light37 	' top row, Center Radioactive Swamp
			'	Light33 	' mid row. Left Orbit
			'	Light34	' mid row, Right Ramp Start
			'	Light35 	' mid row, Flying Aligator cave
			'	Light32 	' mid row, Right Orbit
			'	Light30	' low row, Croc Cave			
        Case 6   ' Hit the Moving Light
            DMD CL("TALLGEESE"), CL("HIT THE MOVING LIGHT"), "", eNone, eNone, eNone, 1500, True, "" : pupevent 814
			'debug.print "Case 6 started, Light29 = 2"		
            Light29.State = 2
			' Sequence :
				' Target1 ... Lg Target right - flower Light29
				' Target12 ... CrocMan Cave - skull Light30
				' Target13 ... Lg Target right left - flower Light31
				' sw7 ... Orbit lane light - skull Light32
				' sw8 ... Orbit lane left - skull Light33
				' RightRampDone ... Ramp right - skull Light34
				' JackalHole ... Flying Gator Cave - skull Light35
				' LeftRampDone... Ramp left - skull Light36
				' Lock ... Radioactive Swamp - skull Light37
        Case 7 'Small Targets (4 in center)
			' HIT: Target2, Target4, Target5, Target7, Target8, Target10
            DMD CL("BLACK LANTERN"), CL("HIT SMALL TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            LightSeqBlueTargets.Play SeqRandom, 10, , 1000
            TargetHits7 = 0
        Case 8 ' Large Targets, 1 Left, 1 Right
			' HIT: Target1, Target13
            DMD CL("TOXIC MONSTER"), CL("HIT LARGE TARGETS"), "", eNone, eNone, eNone, 1500, True, ""
            Light31.State = 2
            Light29.State = 2
            TargetHits8 = 0
        Case 9 ' Light Chase (moves every 10 seconds)
            DMD CL("ANUBIS"), CL("LIGHT CHASE"), "", eNone, eNone, eNone, 1500, True, ""
            FollowTheLights.Enabled = 1
            LightHits9 = 0
			'	Light37 	' top row 2 - skull
			'	Light32 	' mid row 3 - skull
			'	Light33 	' mid row 1 - skull
			'	Light35 	' mid row 2 - skull
			'	Light36		' top row 1 - skull
			'	Light30		' low row 1 - skull
			'	Light31		' large target left
			'	Light29		' large target right
			'	Light44		' top row 1 - jackpot
			'	Light40 	' mid row 2 - jackpot
        Case 10 ' Outer Orbit Loop - sw 7 & sw8
            DMD CL("SEEDER"), CL("OUTER ORBIT LOOPS"), "", eNone, eNone, eNone, 1500, True, ""
            Light33.State = 2
            Light32.State = 2
            Gate2.Open = 1
            Gate3.Open = 1
            loopCount = 0
        Case 11 ' Light Chase part du  (moves every 10 seconds)
            DMD CL("AVATAR OF ROT"), CL("LIGHT CHASE"), "", eNone, eNone, eNone, 1500, True, ""
            FollowTheLights.Enabled = 1
            LightHits11 = 0
			'	Light31		' large target left
			'	Light29		' large target right
			'	Light37 	' top row 2 - skull
			'	Light32 	' mid row 3 - skull
			'	Light33 	' mid row 1 - skull
			'	Light35 	' mid row 2 - skull
			'	Light44		' top row 1 - Jackpot
			'	Light40 	' mid row 2 - Jackpot
        Case 12 ' Ramps and Orbits
            'uses the ramphits12 to count the hits
            DMD CL("CONCLAVE"), CL("HIT RAMP ORBIT"), "", eNone, eNone, eNone, 1500, True, ""
			Light33.State = 2	' skull - orbit left
			Light32.State = 2	' skull - orbit right
			Light36.State = 2	' skull - ramp left
			Light34.State = 2	' skull - ramp right
            RampHits12 = 0
        Case 13 'Mordekai the Summoner - the final battle
            DMD CL("LIMBURGER"), CL("HIT JACKPOTS"), "", eNone, eNone, eNone, 1500, True, ""
            AddMultiball 4
            StartJackpots
            ChangeGi 5
    End Select
End Sub

' check if the battle is completed
Sub CheckWinBattle
	debug.print "CheckWinBattle 1 - SpinCount " & SpinCount
	debug.print "CheckWinBattle 2 - SuperBumperHits " & SuperBumperHits
	debug.print "CheckWinBattle 3 - RampHits3 " & RampHits3
	debug.print "CheckWinBattle 4 - OrbitHits " & OrbitHits
	debug.print "CheckWinBattle 5 - " & Light30.State + Light32.State + Light33.State + Light34.State + Light35.State + Light36.State + Light37.State
	debug.print "CheckWinBattle 6 - n/a"
	debug.print "CheckWinBattle 7 - TargetHits7 " & TargetHits7
	debug.print "CheckWinBattle 8 - TargetHits8 " & TargetHits8
	debug.print "CheckWinBattle 9 - LightHits9 " & LightHits9
	debug.print "CheckWinBattle 10 - loopCount " & loopCount
    debug.print "CheckWinBattle 11 - LightHits11 " & LightHits11
    debug.print "CheckWinBattle 12 - RampHits12 " & RampHits12
'
'
	dim tmp
    tmp = INT(RND * 7) + 1
    PlaySound "fx_thunder" & tmp
    DOF 126, DOFPulse
    LightSeqInserts.StopPlay 'stop the light effects before starting again so they don't play too long.
    LightEffect 3
    FlashEffect 3
    Select Case NewBattle
        Case 1
            If SpinCount = battleSpin Then WinBattle:End if
        Case 2
            If SuperBumperHits = battleSuperBumper Then WinBattle:End if
        Case 3
            If RampHits3 = battleRamp Then WinBattle:End if
        Case 4
            If OrbitHits = battleOrbit Then WinBattle:End if
        Case 5	' Hit Skull Lights
            If Light30.State + Light32.State + Light33.State + Light34.State + Light35.State + Light36.State + Light37.State = 0 Then WinBattle:End if
        Case 6 ' Hit the Moving Light // must Hit All lights in sequence, no count is kept since WinBattle triggers with the last light in sequence	
        Case 7
            If TargetHits7 = battleTarget Then WinBattle:End if
        Case 8
            If TargetHits8 = battleTarget Then WinBattle:End if
        Case 9
            If LightHits9 = battleLight Then WinBattle:End if
        Case 10:
            If loopCount = battleLoop Then WinBattle
        Case 11
            If LightHits11 = battleLight Then WinBattle:End if
        Case 12
            If RampHits12 = battleRamp Then WinBattle:End if
    End Select
End Sub

Sub ResetToys
	debug.print "CurrentPlayer | BattlesWon : " & CurrentPlayer & " | " & BattlesWon(CurrentPlayer)
	Select Case BattlesWon(CurrentPlayer)
		Case 0
			'CrocMan.visible = True
			f4.state = true
			f4r.state = false
			'Gator.visible = True
			f1.state = true
			f1r.state = false
			'AntonArchane.visible = True	
			f3.state = true
			f3r.state = false
		Case 1 						' Crock Man hurt
			'CrocMan.visible = True
			f4.state = true
			f4r.state = true
			'Gator.visible = True
			f1.state = true
			f1r.state = false
			'AntonArchane.visible = True	
			f3.state = true
			f3r.state = false
        Case 2						' Crock Man dead
			'CrocMan.visible = False
			f4.state = false
			f4r.state = true
			'Gator.visible = True
			f1.state = true
			f1r.state = false
			'AntonArchane.visible = True	
			f3.state = true
			f3r.state = false
        Case 4						' Flying Gator hurt
			'CrocMan.visible = False
			f4.state = false
			f4r.state = true
			'Gator.visible = True
			f1.state = true
			f1r.state = true
			'AntonArchane.visible = True	
			f3.state = true
			f3r.state = false
        Case 5						' Flying Gator dead
			'CrocMan.visible = False
			f4.state = false
			f4r.state = true
			'Gator.visible = False
			f1.state = false
			f1r.state = true
			'AntonArchane.visible = True	
			f3.state = true
			f3r.state = false
        Case 7 						' Anton Archane hurt
			'CrocMan.visible = False
			f4.state = false
			f4r.state = true
			'Gator.visible = False
			f1.state = false
			f1r.state = true
			'AntonArchane.visible = True	
			f3.state = true
			f3r.state = true
		Case 8						' Anton Archane dead
			'CrocMan.visible = False
			f4.state = false
			f4r.state = true
		'	Gator.visible = False
			f1.state = false
			f1r.state = true
			'AntonArchane.visible = False	
			f3.state = false
			f3r.state = true
	End Select
End Sub

Sub StopBattle 'called at the end of a battle
    Dim i
    Battle(CurrentPlayer, 0) = 0
    For i = 0 to 15
        If Battle(CurrentPlayer, i) = 2 Then Battle(CurrentPlayer, i) = 0
    Next
    UpdateBattleLights
    StopBattle2
    NewBattle = 0	' reset battle #

End Sub

'called after completing a battle
Sub WinBattle
    Dim tmp
	debug.print "BattlesWon : " & BattlesWon(CurrentPlayer)
    BattlesWon(CurrentPlayer) = BattlesWon(CurrentPlayer) + 1
    Battle(CurrentPlayer, 0) = 0
    Battle(CurrentPlayer, NewBattle) = 1
    UpdateBattleLights
    FlashEffect 2
    LightEffect 2
    GiEffect 2
    DMD "", CL("BATTLE COMPLETED"), "_", eNone, eBlinkFast, eNone, 1000, True, "fx_Explosion01"
    DOF 139, DOFPulse	' shaker
    tmp = INT(RND * 4)
    Select Case tmp
        Case 0:vpmtimer.addtimer 1500, "PlaySound ""vo_excelent"" '"
        Case 1:vpmtimer.addtimer 1500, "PlaySound ""vo_impressive"" '"
        Case 2:vpmtimer.addtimer 1500, "PlaySound ""vo_welldone"" '"
        Case 3:vpmtimer.addtimer 1500, "PlaySound ""vo_YouWon"" '"
    End Select

    StopBattle2
    NewBattle = 0	' reset battle #
    SelectBattle	' auto select next battle

    Select Case BattlesWon(CurrentPlayer)     ' multiball on every 3rd battle win
        Case 3, 6, 9:AddMultiball 2
    End Select

    ChangeSong
	ResetToys  ' sets 3D Model and lighting based on battle wins

End Sub

sub Gate1_hit  	' ball entered shooter lane	
	debug.print "Gate1 hit"
end sub


Sub StopBattle2
  ' Turn Off Lights
    'Skull
    Light30.State = 0
    Light32.State = 0
    Light33.State = 0
    Light34.State = 0
    Light35.State = 0
    Light36.State = 0
    Light37.State = 0

	' Large Targets
    Light19.State = 0
    Light20.State = 0
    Light29.State = 0
    Light31.State = 0

  ' stop timers or reset battle variables

	'stop_fog  ' turn off fog
    
	Select Case NewBattle
        Case 1:SpinCount = 0
        Case 2:Light55.State = 0:LightSeqBumpers.StopPlay:SuperBumperHits = 0
        Case 3, 12:
        Case 4:OrbitHits = 0
        Case 7:LightSeqBlueTargets.StopPlay
        Case 8:
        Case 9, 11:FollowTheLights.Enabled = 0
        Case 10:LoopCount = 0:Gate2.Open = 0:Gate3.Open = 0
        Case 13:ResetBattles:SelectBattle
    End Select
End Sub

Sub ResetBattles
    Dim i, j
    For j = 0 to 4
        BattlesWon(j) = 0
        For i = 0 to 6
            Battle(CurrentPlayer, i) = 0
        Next
    Next
    NewBattle = 0
End Sub

'Extra subs for the battles

Sub LightSeqAllTargets_PlayDone()
    LightSeqAllTargets.Play SeqRandom, 10, , 1000
End Sub

Sub LightSeqBumpers_PlayDone()
    LightSeqBumpers.Play SeqRandom, 10, , 1000
End Sub

Sub LightSeqBlueTargets_PlayDone()
    LightSeqBlueTargets.Play SeqRandom, 10, , 1000
End Sub

' Wizards modes timer
Dim FTLstep:FTLstep = 0

Sub FollowTheLights_Timer	' timer is variable with user setting

FollowTheLights.Interval = battleFollowTheLights

    Light30.State = 0
    Light32.State = 0
    Light33.State = 0
    Light34.State = 0
	Light35.State = 0
    Light36.State = 0
    Light37.State = 0
    Select Case Battle(CurrentPlayer, 0)
        Case 9
            Select case FTLstep
                Case 0:FTLstep = 1:Light30.State = 2
                Case 1:FTLstep = 2:Light32.State = 2
                Case 2:FTLstep = 3:Light33.State = 2
                Case 3:FTLstep = 3:Light34.State = 2
                Case 4:FTLstep = 4:Light35.State = 2
                Case 5:FTLstep = 5:Light36.State = 2
                Case 6:FTLstep = 6:Light37.State = 2
            End Select
        Case 11
            FTLstep = INT(RND * 7)
            Select case FTLstep
                Case 0:Light30.State = 2
                Case 1:Light32.State = 2
                Case 2:Light33.State = 2
                Case 3:Light34.State = 2
                Case 4:Light35.State = 2
                Case 5:Light36.State = 2
                Case 6:Light37.State = 2
            End Select
    End Select
End Sub

'**********************
' Power up Jackpot
'**********************
' 30 seconds hurry up with jackpots on the right ramp
' uses variable PowerupHits and the light50

Sub CheckPowerup
    If light50.State = 0 Then
        If PowerupHits MOD 10 = 0 Then
            EnablePowerup
        End If
    End If
End Sub

Sub EnablePowerup
    ' start the timers
    PowerupTimerExpired.Enabled = True
    PowerupSpeedUpTimer.Enabled = True
    ' turn on the light
    Light50.BlinkInterval = 160
    Light50.State = 2
End Sub

Sub PowerupTimerExpired_Timer()
    PowerupTimerExpired.Enabled = False
    ' turn off the light
    Light50.State = 0
End Sub

Sub PowerupSpeedUpTimer_Timer()
    PowerupSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    Light50.BlinkInterval = 80
    Light50.State = 2
End Sub

'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM?si=Xqd-rcaqTlgEd_wx
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE?si=i8ne8Ao2co8rt7fy
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs?si=hvgMOk-ej1BEYjJv
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25)
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 12-15    |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 3-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |





'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub
	
	Public Property Get Pos 'returns % position a ball. For debug stuff.
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'   Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If DebugOn Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'debug.print " "
		End If
	End Sub
End Class

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	Dim BOT
	BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************
'
'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If

		If x = UBound(RampBalls) Then	 'debug
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId,RampPlasticLoop,RampWireLoop)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound(RampPlasticLoop & x)
			StopSound(RampWireLoop & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound(RampPlasticLoop & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound(RampWireLoop & x)
					debug.print "ramp sound on : " & RampPlasticLoop
				Else
					StopSound(RampPlasticLoop & x)
					PlaySound(RampWireLoop & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				debug.print "ramp sound on : " & RampWireLoop
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound(RampPlasticLoop & x)
				StopSound(RampWireLoop & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound(RampPlasticLoop & x)
				StopSound(RampWireLoop & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound(RampPlasticLoop & x)
			StopSound(RampWireLoop & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function




Sub RandomSoundRampStop(obj)
	' /// added for Swamp Thing ///
	'   changed wireramp_stop to fx_woodhit to sound like Wood
	' ///
 Const RampHitSound = "fx_woodhit"  ' added for Swamp Thing
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol RampHitSound, obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol RampHitSound, obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol RampHitSound, obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
' Dim RollingSoundFactor
' RollingSoundFactor = 1.1 / 5  ' ** moved to User Setting Menu

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
' Dim ArchSoundFactor
' ArchSoundFactor = 0.025 / 5		'volume multiplier; must not be zero   ** moved to user settings

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	FlipperCradleCollision ball1, ball2, velocity

	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'****************************** Script for VR Rooms  ********************

Dim Object

Sub Desktopmode()
	If table1.ShowDT = True Then
	UseFlexDMD = False
	Flasher2.visible = 1
	Flasher3.visible = 1	
	FlasherSkull.visible = 1
	rrail.visible=1
	lrail.visible=1
	wall005.sidevisible = 1
	wall006.sidevisible = 1
	for each Object in rails : object.sidevisible = 0 : next	
	for each Object in aDMD : object.visible = 1 : next
	for each Object in VR_Stuff : object.visible = 0 : next
    for each Object in Sphere : object.visible = 0 : next
	for each Object in VR_Table : object.visible = 0 : next
	for each Object in VR_Minimal : object.visible = 0 : next
	Else
	UseFlexDMD = True
	for each Object in rails : object.sidevisible = 0 : next	
	for each Object in aDMD : object.visible = 0 : next
	for each Object in VR_Stuff : object.visible = 0 : next
    for each Object in Sphere : object.visible = 0 : next
	for each Object in VR_Table : object.visible = 0 : next
	for each Object in VR_Minimal : object.visible = 0 : next
	Flasher2.visible = 0
	Flasher3.visible = 0
	rrail.visible=0
	lrail.visible=0
	FlasherSkull.visible = 0
	wall005.sidevisible = 1
	wall006.sidevisible = 1
End If
End Sub

Sub Room1()
	for each Object in rails : object.sidevisible = 1 : next		
	for each Object in aDMD : object.visible = 0 : next
	for each Object in VR_Stuff : object.visible = 1 : next
    for each Object in Sphere : object.visible = 0 : next
	for each Object in VR_Table : object.visible = 1 : next
	for each Object in VR_Minimal : object.visible = 0 : next
	Flasher2.visible = 0
	Flasher3.visible = 0
	rrail.visible=0
	lrail.visible=0
	FlasherSkull.visible = 0
	wall005.sidevisible = 0
	wall006.sidevisible = 0
	End Sub
Sub Roomminimal()
	for each Object in aDMD : object.visible = 0 : next
	for each Object in VR_Stuff : object.visible = 0 : next
    for each Object in Sphere : object.visible = 0 : next
	for each Object in VR_Table : object.visible = 1 : next
	for each Object in VR_Minimal : object.visible = 1 : next
	for each Object in rails : object.sidevisible = 1 : next
	wall005.sidevisible = 0
	wall006.sidevisible = 0
	Flasher2.visible = 0
	Flasher3.visible = 0
	rrail.visible=0
	lrail.visible=0
	FlasherSkull.visible = 0
End Sub
Sub Roomchroma()
	for each Object in aDMD : object.visible = 0 : next
	for each Object in VR_Stuff : object.visible = 0 : next
    for each Object in Sphere : object.visible = 1 : next
	for each Object in VR_Table : object.visible = 1 : next
	for each Object in VR_Minimal : object.visible = 0 : next
	for each Object in rails : object.sidevisible = 1 : next
	wall005.sidevisible = 0
	wall006.sidevisible = 0
	FlasherSkull.visible = 0
	Flasher2.visible = 0
	Flasher3.visible = 0
	rrail.visible=0
	lrail.visible=0
End Sub

Sub VRChangeRoom()
	If VRRoom = 0 Then
		DesktopMode
	End If
	If VRRoom = 1 Then
		UseFlexDMD = True
		Room1
	End If
	If VRRoom = 2 Then
		UseFlexDMD = True
		Roomminimal
	End If
	If VRRoom = 3 Then
		UseFlexDMD = True
		Roomchroma
	End If
End Sub

'*****************************************************************************************************
' VR PLUNGER ANIMATION
'
' Code needed to animate the plunger. If you pull the plunger it will move in VR.
' IMPORTANT: there are two numeric values in the code that define the postion of the plunger and the 
' range in which it can move. The fists numeric value is the actual y position of the plunger primitive
' and the second is the actual y position + 135 to determine the range in which it can move.
'
' You need to to select the Primary_plunger primitive you copied from the
' template you need to select the Primary_plunger primitive and copy the value of the Y position 
' (e.g. 1269.286) into the code. The value that determines the range of the plunger is always the y 
' position + 135 (e.g. 1404).
'
'*****************************************************************************************************

Sub TimerPlunger_Timer
  If PinCab_Shooter.Y < 40.20 then
  		PinCab_Shooter.Y = PinCab_Shooter.Y + 15
  End If
End Sub

Sub TimerPlunger2_Timer
 'debug.print plunger.position
  PinCab_Shooter.Y = -94.80 + (5* Plunger.Position) -20
End Sub

'******************************************************
' 	ZFLB:  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

' Explanation of how these bumpers work:
' There are 10 elements involved per bumper:
' - the shadow of the bumper ( a vpx flasher object)
' - the bumper skirt (primitive)
' - the bumperbase (primitive)
' - a vpx light which colors everything you can see through the bumpertop
' - the bulb (primitive)
' - another vpx light which lights up everything around the bumper
' - the bumpertop (primitive)
' - the VPX bumper object
' - the bumper screws (primitive)
' - the bulb highlight VPX flasher object
' All elements have a special name with the number of the bumper at the end, this is necessary for the fading routine and the initialisation.
' For the bulb and the bumpertop there is a unique material as well per bumpertop.
' To use these bumpers you have to first copy all 10 elements to your table.
' Also export the textures (images) with names that start with "Flbumper" and "Flhighlight" and materials with names that start with "bumper".
' Make sure that all the ten objects are aligned on center, if possible with the exact same x,y coordinates
' After that copy the script (below); also copy the BumperTimer vpx object to your table
' Every bumper needs to be initialised with the FlInitBumper command, see example below;
' Colors available are red, white, blue, orange, yellow, green, purple and blacklight.
' In a GI subroutine you can then call set the bumperlight intensity with the "FlBumperFadeTarget(nr) = value" command
' where nr is the number of the bumper, value is between 0 (off) and 1 (full on) (so you can also use 0.3 0.4 etc).

' Notes:
' - There is only one color for the disk; you can photoshop it to a different color
' - The bumpertops are angle independent up to a degree; my estimate is -45 to + 45 degrees horizontally, 0 (topview) to 70-80 degrees (frontview)
' - I built in correction for the day-night slider; this might not work perfectly, depending on your table lighting
' - These elements, textures and materials do NOT integrate with any of the lighting routines I have seen in use in many VPX tables
'   (just find the GI handling routine and insert the FlBumperFadeTarget statement)
' - If you want to use VPX native bumperdisks just copy my bumperdisk but make it invisible

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0
	DNA45 = (NightDay - 10) / 20
	DNA90 = 0
	DayNightAdjust = 0.4
Else
	DNA30 = (NightDay - 10) / 30
	DNA45 = (NightDay - 10) / 45
	DNA90 = (NightDay - 10) / 90
	DayNightAdjust = NightDay / 25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt
For cnt = 1 To 6
	FlBumperActive(cnt) = False
Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight
FlInitBumper 1, "red"
FlInitBumper 2, "white"
FlInitBumper 3, "blue"
FlInitBumper 4, "orange"
FlInitBumper 5, "yellow"

' ### uncomment the statement below to change the color for all bumpers ###
   Dim ind
   For ind = 1 To 5
	   FlInitBumper ind, "yellow"
   Next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1
	FlBumperFadeTarget(nr) = 0.7
	FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr)
	FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr)
	Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr)
	Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr)
	FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr)
	FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	
	' set the color for the two VPX lights
	Select Case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0)
			FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0)
			FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255)
			FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255)
			FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8)
			FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32)
			FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255)
			MaterialColor "bumpertopmat" & nr, RGB(16,255,16)
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0)
			FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8)
			FlBumperBigLight(nr).colorfull = RGB(255,190,8)
			
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190)
			FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
			
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255)
			FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4)
			FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50)
			FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
			FlBumperSmallLight(nr).TransmissionScale = 0
			
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255)
			FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255)
			FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	End Select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
	'   UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
	'			   OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
	'			   float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 ) * DayNightAdjust
	
	Select Case FlBumperColor(nr)
		Case "blue"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(38 - 24 * Z,130 - 98 * Z,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z ^ 3) / (0.5 + DNA90)
			
		Case "green"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(16 + 16 * Sin(Z * 3.14),255,16 + 16 * Sin(Z * 3.14)), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z ^ 3) / (1 + DNA90)
			
		Case "red"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 16 - 11 * Z + 16 * Sin(Z * 3.14),0), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z * 4,8 - Z * 8)
			
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 100 - 22 * z + 16 * Sin(Z * 3.14),Z * 32), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z * 50, 0)
			
		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 8 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20 * Z,255 - 65 * Z)
			FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20 * Z,255 - 65 * Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z * 36,220 - Z * 90)
			
		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 1, RGB(30 - 27 * Z ^ 0.03,30 - 28 * Z ^ 0.01, 255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z ^ 3
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z ^ 3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255 - 240 * (Z ^ 0.1),255 - 240 * (Z ^ 0.1),255)
			FlBumperSmallLight(nr).colorfull = RGB(255 - 200 * z,255 - 200 * Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255 - 190 * Z,235 - z * 180,220 + 35 * Z)
			
		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(255, 180 + 40 * z, 48 * Z), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30 ^ 2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z + 0.97 * Z ^ 10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z ^ 3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)
			
		Case "purple"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1 - Z, 1 - Z, 1 - Z, 0.9999, RGB(128 - 118 * Z - 32 * Sin(Z * 3.14), 32 - 26 * Z ,255), RGB(255,255,255), RGB(32,32,32), False, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15 + 200 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z + 0.97 * Z ^ 3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z ^ 3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128 - 60 * Z,32,255)
	End Select
End Sub

Sub BumperTimer_Timer
	Dim nr
	For nr = 1 To 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) And FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	Next
End Sub

'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************
'*************************************************************Team Tuga********************************************************


' COPY EVERYTHING BELOW TO THE TOP OF YOUR TABLE SCRIPT UNDER OPTION EXPLICIT                             Start Pup Pack

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "Biker Mice"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cGameName 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage: pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP

'******************************************************
' 	Z3DI:   3D INSERTS
'******************************************************
'
'
' Before you get started adding the inserts to your playfield in VPX, there are a few things you need to have done to prepare:
'	 1. Cut out all the inserts on the playfield image so there is alpha transparency where they should be.
'	  Make sure the playfield material has Opacity Active checkbox checked.
'	2. All the  insert text and/or images that lie over the insert plastic needs to be in its own file with
'	   alpha transparency. Many playfields may require finding the original font and remaking the insert text.
'
' To add the inserts:
'	1. Import all the textures (images) and materials from this file that start with the word "Insert" into your Table
'   2. Copy and past the two primitves that make up the insert you want to use. One primitive is for the on state, the other for the off state.
'   3. Align the primitives with the associated insert light. Name the on and off primitives correctly.
'   5. You will need to manually tweak the disable lighting value and material parameters to achielve the effect you want.
'
'
' Quick Reference:  Laying the Inserts ( Tutorial From Iaakki)
' - Each insert consists of two primitives. On and Off primitive. Suggested naming convention is to use lamp number in the name. For example
'   is lamp number is 57, the On primitive is "p57" and the Off primitive is "p57off". This makes it easier to work on script side.
' - When starting from a new table, I'd first select to make few inserts that look quite similar. Lets say there is total of 6 small triangle
'   inserts, 4 yellow and 2 blue ones.
' - Import the insert on/off images from the image manager and the vpx materials used from the sample project first, and those should appear
'   selected properly in the primitive settings when you paste your actual insert trays in your target table . Then open up your target project
'   at same time as the sample project and use copy&paste to copy desired inserts to target project.
' - There are quite many parameters in primitive that affect a lot how they will look. I wouldn't mess too much with them. Use Size options to
'   scale the insert properly into PF hole. Some insert primitives may have incorrect pivot point, which means that changing the depth, you may
'   also need to alter the Z-position too.
' - Once you have the first insert in place, wire it up in the script (detailed in part 3 below). Then set the light bulb's intensity to zero,
'   so it won't harass the adjustment.
' - Start up the game with F6 and visually see if the On-primitive blinks properly. If it is too dim, hit D and open editor. Write:
' - p57.BlendDisableLighting = 300 and hit enter
' - -> The insert should appear differently. Find good looking brightness level. Not too bright as the light bulb is still missing. Just generic good light.
'	 - If you cannot find proper light color or "mood", you can also fiddle with primitive material values. Provided material should be
'	   quite ok for most of the cases.
'	 - Now when you have found proper DL value (165), but that into script:
' - That one insert is now adjusted and you should be able to copy&paste rest of the triangle inserts in place and name them correctly. And add them
'   into script. And fine tune their brightness and color.
'
' Light bulbs and ball reflection:
'
' - This kind of lighted primitives are not giving you ball reflections. Also some more glow vould be needed to make the insert to bloom correctly.
' - Take the original lamp (l57), set the bulb mode enabled, set Halo Height to -3 (something that is inside the 2 insert primitives). I'd start with
'   falloff 100, falloff Power 2-2.5, Intensity 10, scale mesh 10, Transmit 5.
' - Start the game with F6, throw a ball on it and move the ball near the blinking insert. Visually see how the reflection looks.
' - Hit D once the reflection is the highest. Open light editor and start fine tuning the bulb values to achieve realistic look for the reflection.
' - Falloff Power value is the one that will affect reflection creatly. The higher the power value is, the brighter the reflection on the ball is.
'   This is the reason why falloff is rather large and falloff power is quite low. Change scale mesh if reflection is too small or large.
' - Transmit value can bring nice bloom for the insert, but it may also affect to other primitives nearby. Sometimes one need to set transmit to
'   zero to avoid affecting surrounding plastics. If you really need to have higher transmit value, you may set Disable Lighting From Below to 1
'   in surrounding primitive. This may remove the problem, but can make the primitive look worse too.

' ---- FOR EXAMPLE PUROSE ONLY. DELETE BELOW WHEN USING FOR REAL -----
' NOTE: The below timer is for flashing the inserts as a demonstration. Should be replaced by actual lamp states.
'	   In other words, delete this sub (InsertFlicker_timer) and associated timer if you are going to use with a ROM.
Dim flickerX, FlickerState
FlickerState = 0
'Sub InsertFlicker_timer
'	If FlickerState = 0 Then
'		For flickerX = 0 To 19
'			If flickerX < 6 Or flickerX > 9 Then
'				AllLamps(flickerX).state = False
'			ElseIf BonusX(flickerX - 6) = False Then
'				AllLamps(flickerX).state = False
'			End If
'		Next
'		FlickerState = 1
'	Else
'		For flickerX = 0 To 19
'			If flickerX < 6 Or flickerX > 9 Then
'				AllLamps(flickerX).state = True
'			ElseIf BonusX(flickerX - 6) = False Then
'				AllLamps(flickerX).state = True
'			End If
'		Next
'		FlickerState = 0
'	End If
'End Sub



Sub l1_animate: p1.BlendDisableLighting = 200 * (Light40.GetInPlayIntensity / Light40.Intensity): End Sub
Sub l2_animate: p2.BlendDisableLighting = 200 * (Light44.GetInPlayIntensity / Light44.Intensity): End Sub
Sub l3_animate: p3.BlendDisableLighting = 200 * (Light48.GetInPlayIntensity / Light48.Intensity): End Sub
 Sub l4_animate: p4.BlendDisableLighting = 200 * (Light46.GetInPlayIntensity / Light46.Intensity): End Sub
 Sub l5_animate: p5.BlendDisableLighting = 200 * (Light51.GetInPlayIntensity / Light51.Intensity): End Sub
Sub l6_animate: p6.BlendDisableLighting = 200 * (Light50.GetInPlayIntensity / Light50.Intensity): End Sub
Sub l7_animate: p7.BlendDisableLighting = 200 * (Light42.GetInPlayIntensity / Light42.Intensity): End Sub
Sub l8_animate: p8.BlendDisableLighting = 200 * (l8.GetInPlayIntensity / l8.Intensity): End Sub
Sub l9_animate: p9.BlendDisableLighting = 200 * (Light41.GetInPlayIntensity / Light41.Intensity): End Sub

' Sub l10_animate: p10.BlendDisableLighting = 200 * (l10.GetInPlayIntensity / l10.Intensity): End Sub
Sub l11_animate: p11.BlendDisableLighting = 200 * (Light35.GetInPlayIntensity / Light35.Intensity): End Sub
Sub l12_animate: p12.BlendDisableLighting = 200 * (Light37.GetInPlayIntensity / Light37.Intensity): End Sub
Sub l13_animate: p13.BlendDisableLighting = 200 * (l13.GetInPlayIntensity / l13.Intensity): End Sub
Sub l14_animate: p14.BlendDisableLighting = 200 * (Light43.GetInPlayIntensity / Light43.Intensity): End Sub
Sub l15_animate: p15.BlendDisableLighting = 200 * (Light33.GetInPlayIntensity / Light33.Intensity): End Sub
Sub l16_animate: p16.BlendDisableLighting = 200 * (Light36.GetInPlayIntensity / Light36.Intensity): End Sub
Sub l17_animate: p17.BlendDisableLighting = 200 * (Light34.GetInPlayIntensity / Light34.Intensity): End Sub
Sub l18_animate: p18.BlendDisableLighting = 200 * (Light5.GetInPlayIntensity / Light5.Intensity): End Sub
Sub l19_animate: p19.BlendDisableLighting = 200 * (Light30.GetInPlayIntensity / Light30.Intensity): End Sub

Sub l20_animate: p20.BlendDisableLighting = 200 * (Light32.GetInPlayIntensity / Light32.Intensity): End Sub
Sub l21_animate: p21.BlendDisableLighting = 200 * (Light16.GetInPlayIntensity / Light16.Intensity): End Sub
Sub l22_animate: p22.BlendDisableLighting = 200 * (Light14.GetInPlayIntensity / Light14.Intensity): End Sub
Sub l23_animate: p23.BlendDisableLighting = 200 * (LightShootAgain.GetInPlayIntensity / LightShootAgain.Intensity): End Sub
Sub l24_animate: p24.BlendDisableLighting = 200 * (Light13.GetInPlayIntensity / Light13.Intensity): End Sub
Sub l25_animate: p25.BlendDisableLighting = 200 * (Light56.GetInPlayIntensity / Light56.Intensity): End Sub
Sub l26_animate: p26.BlendDisableLighting = 200 * (Light57.GetInPlayIntensity / Light57.Intensity): End Sub
Sub l27_animate: p27.BlendDisableLighting = 200 * (Light58.GetInPlayIntensity / Light58.Intensity): End Sub
Sub l28_animate: p28.BlendDisableLighting = 200 * (Light59.GetInPlayIntensity / Light59.Intensity): End Sub
Sub l29_animate: p29.BlendDisableLighting = 200 * (Light55.GetInPlayIntensity / Light55.Intensity): End Sub
Sub l30_animate: p30.BlendDisableLighting = 200 * (Light52.GetInPlayIntensity / Light52.Intensity): End Sub
Sub l31_animate: p31.BlendDisableLighting = 200 * (Light6.GetInPlayIntensity / Light6.Intensity): End Sub
Sub l32_animate: p32.BlendDisableLighting = 200 * (Light1.GetInPlayIntensity / Light1.Intensity): End Sub
Sub l35_animate: p35.BlendDisableLighting = 200 * (Light4.GetInPlayIntensity / Light4.Intensity): End Sub
Sub l34_animate: p34.BlendDisableLighting = 200 * (Light2.GetInPlayIntensity / Light2.Intensity): End Sub
Sub l36_animate: p35.BlendDisableLighting = 200 * (Light10.GetInPlayIntensity / Light10.Intensity): End Sub

'******************************************************
'*****   END 3D INSERTS
'******************************************************
