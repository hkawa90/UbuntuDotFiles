# ~/.xinitrc

sysXresources=/etc/X11/xinit/.Xresources
userXresources=$HOME/.Xresources

if [ -f "$sysXresources" ]; then
    xrdb -merge "$sysXresources"
fi
if [ -f "$userXresources" ]; then
    xrdb -merge "$userXresources"
fi

