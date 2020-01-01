setxkbmap -option caps:ctrl_modifier
feh --bg-scale $HOME/.config/wall.png
exec "pulseaudio --start" &
exec slstatus &
exec dwm
