# map capslock to Control_R
xmodmap -e 'keycode 66 = Control_R'
xmodmap -e 'clear Lock'
xmodmap -e 'add Control = Control_R'

# map left control to left super
xmodmap -e "remove control = Control_L"
xmodmap -e "add mod4 = Control_L"
