'----- Iridium Message Formatter -----------------------------------------------
'
'     This basic code is the self timed formatter for Iridium SBD messages
'     We make use of the XPert's ability to handle custom GOES formatters
'     written by the user in BASIC.  A fundamental part of this capability is 
'     appending data to a message built by the C++ XPert code (e.g. binary interleaved).
'     In this case, we don't append anything, we just capture the message.  The capture
'     is done by writing the message to RECENT.DAT.  That way, the user can define a
'     special log in name and password and retrieve the message that way.  
'
'     The function SELFTIMED_STFormatter handles the message capture.  Most of this
'     routine is straight from the XPert BASIC manual.
'
'------------------------------------------------------------------------------
'
Public Function SELFTIMED_STFormatter
   'Note here that Selftime_STFormatter is the current message built by the C code
   iDatFile = FreeFile
   strDatFileName = "\Flash Disk\RECENT.DAT"
   Open strDatFileName for Output as iDatFile
   Print iDatFile,  Selftimed_STFormatter
   Close iDatFile
End Function


