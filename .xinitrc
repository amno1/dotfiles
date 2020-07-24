#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

xset s off -dpms
numlockx &
feh --randomize --bg-fill /home/arthur/wallpapers/* &
compiz ccp --replace &
#emerald --replace &
conky -c ~/.config/conky-vision/conkyrc &
st &
xbindkeys &
firefox-developer-edition &
#exec compiz ccp --replace
# we exec an empty shell script and put it to infinity sleep
# that way if we accidentally close our app or wm crashes it won't crash X
# to restart X either pkill X or loginscript.sh
exec /home/arthur/.config/loginscript.sh

#exec compiz ccp --replace
