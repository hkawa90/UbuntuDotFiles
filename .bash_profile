# Mount google drive
if [ -f /usr/bin/google-drive-ocamlfuse ]; then
        mount 2>&1 | grep /home/kawa90/GoogleDrive >/dev/null 2>&1
        if [ ! $? = 0 ]; then
                google-drive-ocamlfuse $HOME/GoogleDrive
        fi
fi

export PATH=$HOME/.cargo/bin:$PATH
test -r ~/.bashrc && . ~/.bashrc
source "$HOME/.cargo/env"
