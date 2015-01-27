
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# postgre sql
export PGDATA=/usr/local/var/postgres
export DATABASE_URL=postgres:///$(whoami)

# alias
alias E='emacs -nw'
alias ll='ls -alG'
alias ls='ls -G'
alias C='pbcopy'
alias L='| less'
alias G='| grep'

