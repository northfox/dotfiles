export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# postgre sql
export PGDATA=/usr/local/var/postgres
export DATABASE_URL=postgres:///$(whoami)

# alias
if [ "$(uname)" == 'Darwin' ]; then
    # for mac
    alias E='emacs -nw'
    alias ll='ls -alG'
    alias ls='ls -G'
    alias C='pbcopy'
    alias L='less'
    alias G='grep'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    # for linux
    alias C='cbcopy'
    alias E='emacs -nw'
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
    alias aptitude-list='aptitude search "~i"'
    alias cbcopy='xsel --clipboard --input'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias grep='grep --color=auto'
    alias l='ls -CF'
    alias la='ls -A'
    alias ll='ls -alF'
    alias ls='ls --color=auto'
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then
    # for cygwin
    echo "Please, set settings for cygwin."
else
    echo "Your platform ($(uname -a)) is not supported."
    exit 1
fi


# display title on display when cd
function chpwd() {
    ls; echo -ne "\033]0;$(pwd)\007"
}

# share command history each tab
function share_history() {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
