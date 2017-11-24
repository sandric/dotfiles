<^c::
	Send {tab}
return

#IfWinActive ahk_class SDL_app

^s::
	send, p21
return

^t::
	send, p12
return


state:=0

$c::
  state:= !state
   if state
  Send {TAB down}
  else
  Send {TAB Up}
return
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																										

#IfWinActive

$Shift::
	start_tick := A_TickCount

	While, GetKeyState("Shift","P")
		Send, {Shift down}																																																																																
	Send, {Shift up}		
																																																													
	If (A_TickCount - start_tick < 200)
		Send, {Space}
Return																																																																																																																																																																																																																																																																																																													