xrdb -merge ~/.Xresources

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

emacs --daemon &

python .randomize_background.py &

xcompmgr -c &

exec xmonad
