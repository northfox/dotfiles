
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# postgre sql
export PGDATA=/usr/local/var/postgress
export DATABASE_URL=postgres:///$(whoami)

# alias
alias E='emacs -nw'
alias ll='ls -al'
alias C='pbcopy'
