# ~/.bash_logout: executed by bash(1) when login shell exits.

# when leaving the console clear the screen to increase privacy
echo $SHLVL > $HOME/.sh_level
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
    rsync -av $HOME/Documents/org $HOME/GoogleDrive/org &
    echo "Do rsync" > $HOME/.rsync_done
fi
