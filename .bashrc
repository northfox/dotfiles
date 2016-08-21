# Path
#export PATH="$PATH:$HOME/.rvm/bin" # RVM to PATH for scripting
export PATH="$PATH:/usr/local/heroku/bin" # Heroku Toolbelt

# postgre sql
export PGDATA=/usr/local/var/postgres
# export DATABASE_URL=postgres:///$(whoami)

# color
export TERM=xterm-256color

# alias
if [ "$(uname)" == 'Darwin' ]; then
    # for mac
    ## export PATH="$PATH:$HOME/software/adt-bundle-mac-x86_64-20140702/sdk/tools" # SDK tools
    ## export PATH="$PATH:$HOME/software/adt-bundle-mac-x86_64-20140702/sdk/platform-tools" # SDK tools
    export PATH="$HOME/.rbenv/bin:$PATH:~/.composer/vendor/bin"
    eval "$(rbenv init -)"
    export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home"
    export M2_HOME="$HOME/software/apache-maven-3.3.3"
    export PATH="$M2_HOME/bin:$PATH"
    export JENKINS_URL="http://localhost:8080"
    [[ -e ~/.phpbrew/bashrc ]] && source ~/.phpbrew/bashrc
    [[ -L /usr/local/bin/node ]] || ln -s /usr/local/Cellar/node/6.2.0/bin/node /usr/local/bin/node

    alias E='emacs -nw'
    alias E-init='emacs -nw ~/dotfiles/.emacs.d/init.el'
    alias ll='ls -alG'
    alias ls='ls -G'
    alias C='pbcopy'
    alias L='less'
    alias G='grep'
    alias cd-central='cd ~/Dropbox/central_repo/'
    alias cd-dotfiles='cd ~/dotfiles'
    alias cd-practice='cd ~/study/technology/'
    alias cd-repo='cd ~/repo'
    alias ctags="`brew --prefix`/bin/ctags"
    alias exec-rbenv-bundle='rbenv exec bundle exec'
    alias googleupdate='open /Library/Google/GoogleSoftwareUpdate/GoogleSoftwareUpdate.bundle/Contents/Resources/CheckForUpdatesNow.command'
    alias brew="env PATH=${PATH/`echo ~`\/.phpbrew\/php\/php-5.6.18\/bin:/} brew"
    alias simpledate='date -u +"%Y%m%d"'

elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    # for linux
    if [ "$(type rbenv > /dev/null 2>&1)" == 0 ]; then
        export PATH="$HOME/.rbenv/bin:$PATH"
        eval "$(rbenv init -)"
    fi

    alias C='cbcopy'
    alias emacs='emacs -nw'
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

# change title by manual
function title() {
    echo -ne "\033]0;"$*"\007"
}

# share command history each tab
function share_history() {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend
export HISTSIZE=9999
export HISTCONTROL=ignoredups
