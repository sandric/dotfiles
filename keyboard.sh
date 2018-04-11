#!/bin/bash

#xmodmap -e "remove mod4 = Hyper_L"
#xmodmap -e "clear mod1"
#xmodmap -e "add mod1 = Alt_L Meta_L"
#xmodmap -e "keycode 23 = Super_L"
#xmodmap -e "keycode 34 = Super_R"
#xmodmap -e "keycode 48 = Hyper_R"
#xmodmap -e "keycode 133 = Alt_R"
#xmodmap -e "remove Control = Hyper_R"
#xmodmap -e "add mod3 = Hyper_R"

xkbcomp -I$HOME/sandric-keyboard $HOME/sandric-keyboard/sandric.xkb $DISPLAY

killall -9 xcape
xcape -e "Shift_R=space" > /dev/null
