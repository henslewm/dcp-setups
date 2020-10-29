'----- SCHED_MsgFiler ----------------------------------------------------
'
'      This subroutine is scheduled by the user to capture the RECENT.DAT
'      files and save them to a larger file family that preserved them
'      grouped by days.  That is, each file is one day of messages.
'
'      When run with the Iridium GOES message capture code, it should be
'      scheduled to run a minute or so after the transmission to avoid
'      conflicts
'
'----- GLOBAL VARIABLES  -------------------------------------------------
'
STATIC PreviousDay = 0      'Day used to reference file names
STATIC PreviousFile = "xxxx"    'Place to hold open file name

Public SUB SCHED_MsgFiler

     'Look for Recent.dat
     iDatFile = FREEFILE
     strMsg = "No recent data"
     strDatFileName = "\Flash Disk\RECENT.DAT"
     OPEN strDatFileName FOR INPUT AS iDatFile
     ON ERROR GOTO 100
         LINE INPUT IDatFile, strMsg
     ON ERROR RESUME NEXT
100  CLOSE iDatFile

     'Get a file handle for the message data
     iMsgFile = FREEFILE

     'Get the time, and figure out what the day is
     DateNow = Date
     DayNow = Day(DateNow)

     'Compare the day with the previous run's day
     '  -- if different, close current file, build new file name, and open new file
     IF (DayNow - PreviousDay) > 0 THEN
          MonthNow = Month(DateNow)
          YearNow = Year(DateNow)
          'strMsgFileName = Format("\Flash Disk\Msg%02d.%02d.%4d.txt", MonthNow,DayNow,YearNow)
          strMsgFileName = Format("\Storage Card\Msg%02d.%02d.%4d.txt", MonthNow,DayNow,YearNow)
          PreviousFile = strMsgFileName
     END IF
     'Open the file
     OPEN PreviousFile FOR APPEND AS iMsgFile

     'Save the message
     PRINT iMsgFile, strMsg
     'Close the file
     CLOSE iMsgFile

END SUB
