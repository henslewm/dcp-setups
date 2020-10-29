Public Sub BLOCK_STRIP_FLUFF
   R = GetInput(3)
   Idx = Instr(1, R.Data, "= ")
   If (Idx > 0) Then
      R.Data = Mid(R.Data, Idx + 2)
   End If
   SetOutput 3, R
End Sub