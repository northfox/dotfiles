if [[ -s ~/.nvm/nvm.sh ]];
  then source ~/.nvm/nvm.sh
fi

if [[ -d ~/.nodebrew ]];
  then export PATH=$PATH:~/.nodebrew/current/bin
fi


#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# load bashrc
if [ -f ~/.bashrc ] ; then
    . ~/.bashrc
fi

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH="/usr/local/sbin:$PATH"
alias brew="env PATH=${PATH/\/Library\/Frameworks\/Python\.framework\/Versions/3\.6/bin:/} brew"
