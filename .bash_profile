if [[ -s ~/.nvm/nvm.sh ]];
  then source ~/.nvm/nvm.sh
fi

#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# load bashrc
if [ -f ~/.bashrc ] ; then
    . ~/.bashrc
fi
