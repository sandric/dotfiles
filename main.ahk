SetTitleMatchMode, Regex
#SingleInstance force

Pause::Reload


MoveActiveLeftHalf()
{
	SysGet, area, MonitorWorkArea
	w:=((areaRight-areaLeft)/2)
	h:=(areaBottom-areaTop)

	WinRestore, A	
	WinMove, A, , 0, 0,%w%,%h%		
}

MoveActiveRightHalf()
{
	SysGet, area, MonitorWorkArea
	w:=((areaRight-areaLeft)/2)
	h:=(areaBottom-areaTop)

	WinRestore, A
	WinMove, A, , w, 0, w, h
}

ToggleActive()
{
	static isFullScreen := 1
	
	if(isFullScreen) {
		MoveActiveRightHalf()
		isFullScreen = 0
	} else {
		WinMaximize, A
		isFullScreen = 1
	}
}

SelectTMUXLeftPane()
{
  WinActivate, ahk_class mintty
  Send !+N
}

SelectTMUXRightPane()
{
  WinActivate, ahk_class mintty
  Send !+I
}



<!+u::
	WinActivate, ahk_class Chrome_WidgetWin_1
return

<!+e::
	WinActivate, ahk_class mintty
return


Numpad0::
	WinExist("ahk_class Shell_TrayWnd")

	t := !t

	If (t = "1") {
		WinHide, ahk_class Shell_TrayWnd
		WinHide, Start ahk_class Button
	} Else {
		WinShow, ahk_class Shell_TrayWnd
		WinShow, Start ahk_class Button
	}
return

Numpad5::
	Run, C:\msys64\mingw64.exe,, UseErrorLevel, first_term_pid
	if ErrorLevel = ERROR
		Msgbox Error occured
	sleep 500
	WinSet, Style,  -0xC40000 , A
	WinMaximize, A
return

Numpad7::
	if WinExist("ahk_class Chrome_WidgetWin_1") {
		WinActivate, ahk_class Chrome_WidgetWin_1
		Sleep 100
		ToggleActive()
	}
return

Numpad8::
	Run, vivaldi.exe
	sleep 10000
	WinMaximize, A
return

Numpad9::
	Winset, Alwaysontop, , A
return



$Shift::
	start_tick := A_TickCount

	While, GetKeyState("Shift","P")
		Send, {Shift down}
	Send, {Shift up}
	
	If (A_TickCount - start_tick < 200)
		Send, {Space}
Return


<^,::
	Send {-}
return

>^.::
	Send {+}
return

<!,::
	Send {/}
return

>!.::
	Send {*}
return



+<^n::
	Send +{Left}          
	Send {LShift DownTemp}
return

+<^i::
	Send +{Right}          
	Send {LShift DownTemp}
return

+<^u::
	Send +{Up}          
	Send {LShift DownTemp}
return

+<^e::
	Send +{Down}          
	Send {LShift DownTemp}
return

+<^l::
	Send +{Home}
	Send {LShift DownTemp}
return

+<^y::
	Send +{End}
	Send {LShift DownTemp}
return

+<^h::
	Send +^{Left}
	Send {LShift DownTemp}
return

+<^o::
	Send +^{Right}
	Send {LShift DownTemp}
return



+<^j::
	Send +{PgUp}
	Send {LShift DownTemp}
return

+<^b::
	Send +{PgDn}
	Send {LShift DownTemp}
return



<^n::
	Send {left}
return

<^i::
	Send {right}
return

<^u::
	Send {up}
return

<^e::
	Send {down}
return

<^l::
	Send {Home}
return

<^y::
	Send {End}
return

<^r::
	Send ^{c}
return

<^s::
	Send ^{x}
return

<^t::
	Send ^{v}
return

<^w::
	Send ^{z}
return

<^p::
	Send ^{y}
return

<^h::
	Send ^{Left}
return

<^o::
	Send ^{Right}
return



>^n::
	Send {enter}
return

>^e::
	Send {tab}
return

>^i::
	Send +{tab}
return

>^m::
	Send {backspace}
return

>^r::
	Send ^{s}
return

>^q::
	Send {escape}
return



<^j::
	Send {PgUp}
return

<^b::
	Send {PgDn}
return



<!n::
	Send (
return

<!o::
	Send )
return

<!e::
	Send [
return

<!i::
	Send ]
return

<!l::
	Send {|}
return

<!b::
	Send {&}
return

<!u::
	Send {{}
return

<!y::
	Send {}}
return

<!h::
	Send {@}
return

>!r::
	Send {:}
return

>!s::
	Send {;}
return

>!q::
	Send {?}
return

>!p::
	Send {!}
return

>!w::
	Send {_}
return

>!f::
	Send {=}
return

>!d::
	Send {#}
return



#IfWinActive emacs

>^q::
	Send ^g
return

<^t::
	Send +{insert}
return

<^+w::
	Send ^Z
return

>^w::
	Send ^{f2}w
return

>^f::
	Send ^{f2}f
return

>^p::
	Send ^{f2}p
return

>^g::
	Send ^{f2}g
return

>^a::
	Send ^{f2}a
return

>^r::
	Send ^{f2}r
return

>^s::
	Send ^{f2}s
return

>^t::
	Send ^{f2}t
return

>^d::
	Send ^{f2}d
return

>^c::
	Send ^{f2}c
return

>^v::
	Send ^{f2}v
return


<^+f::
	Send ^{f3}f
return

<^+t::
	Send ^{f3}t
return

<^+s::
	Send ^{f3}s
return


>^+q::
	Send ^{f4}q
return

>^+w::
	Send ^{f4}w
return

>^+f::
	Send ^{f4}f
return

>^+p::
	Send ^{f4}p
return

>^+g::
	Send ^{f4}g
return

>^+a::
	Send ^{f4}a
return

>^+r::
	Send ^{f4}r
return

>^+s::
	Send ^{f4}s
return

>^+t::
	Send ^{f4}t
return

>^+d::
	Send ^{f4}d
return

>^+c::
	Send ^{f4}c
return

>^+v::
	Send ^{f4}v
return

>^+n::
	Send ^{f4}n
return

>^+e::
	Send ^{f4}e
return

>^+i::
	Send ^{f4}i
return

>^+l::
	Send ^{f4}l
return

>^+u::
	Send ^{f4}u
return

>^+y::
	Send ^{f4}y
return


<!+l::
	Send ^{f5}l
return

<!+y::
	Send ^{f5}y
return

#IfWinActive


#IfWinActive ahk_class Chrome_WidgetWin_1

>^w::
	Send ^{F2}
return

>^f::
	Send ^{F3}
return

>^p::
	Send ^{F4}
return

>^g::
	Send ^{F5}
return

>^a::
	Send ^{F6}
return

>^r::
	Send ^{F7}
return

>^s::
	Send ^{F8}
return

>^t::
	Send ^{F9}
return

>^d::
	Send ^{F10}
return

>^c::
	Send ^{F11}
return

>^v::
	Send ^{F12}
return


<!+n::
	Send ^{F20}
return

<!+i::
	Send ^{F21}
return

#IfWinActive



>!a::
	Send {'}
return

>!t::
	Send {"}
return

>^+r::
  FileDelete, C:\msys64\home\sandric\page.html
	Send ^{F24}
  Sleep 200
  SendInput C:\msys64\home\sandric\page.html
  Send {enter}
  SelectTMUXRightPane()
return

>^+f::
  FileDelete, C:\msys64\home\sandric\page.html
	Send ^v
  Send ^c
  SelectTMUXLeftPane()
  Send ^{F4}q
  SendInput indium-connect-to-chrome{enter}
return


#IfWinActive ahk_class SDL_app

<^s::
	Send p12
return

<^t::
	Send p21
return

y::
	SendInput ^{Space}
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