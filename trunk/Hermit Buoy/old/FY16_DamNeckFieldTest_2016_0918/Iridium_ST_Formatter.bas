' To set up an Xpert/9210 to output the current satlink message over Iridium:
'  1. Use Xterm to copy this file to Xpert/9210 \Flash Disk
'  2. Fully configure system for Satlink transmission as you normally would,
'     but then disable both self-timed and random transmissions
'  3. On Setup tab, navigate to Basic - Custom Formatting - Iridium, and
'     select "STFORMATTER" as custom formatter for Iridium transmitter
 
' Declare external function used to get current satlink self-timed message
' extern "C" _declspec(dllexport) LPCTSTR GetSTCurrentMessage()
Declare Function GetSTCurrentMessage Lib "\Windows\Satlink.sll" As String
 
' This function is called by Iridium when time to format. Simply return the
' current satlink self-timed message.
Public Function SELFTIMED_STFormatter
   Selftimed_STFormatter = GetSTCurrentMessage
End Function