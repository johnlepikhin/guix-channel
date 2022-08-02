(define-module (johnlepikhin home bash)
  #:export (bashrc-history
            bashrc-ssh-agent
            bashrc-bash-prompt
            bashrc-helpers))

(define-public bashrc-history
  "\
shopt -s histappend
shopt -s cmdhist
export HISTIGNORE='ls:ls -l:ls -a:pwd:clear:history'
export HISTFILE=$XDG_CACHE_HOME/.bash_history
export HISTSIZE=10000
export HISTTIMEFORMAT='%F %T '
export HISTCONTROL=ignorespace:ignoredups")

(define-public bashrc-ssh-agent
  "\
SSH_ENV=\"$HOME/.ssh/agent-environment\"

function start_agent {
    echo \"Initialising new SSH agent...\"
    ssh-agent | sed 's/^echo/#echo/' > \"${SSH_ENV}\"
    echo succeeded
    chmod 600 \"${SSH_ENV}\"
    . \"${SSH_ENV}\" > /dev/null
    ssh-add;
}

# Source SSH settings, if applicable

if [ -f \"${SSH_ENV}\" ]; then
    . \"${SSH_ENV}\" > /dev/null
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi")

(define-public bashrc-bash-prompt
  "\
function setup_bash_prompt () {
    local __user_and_host=\"\\u@\\h\"
    local __cur_location=\"\\w\"
    local __git_branch_color=\"\\[\\033[31m\\]\"
    local __guix_env_color=\"\\[\\033[32m\\]\"
    local __reset_color=\"\\[\\033[00m\\]\"
    local __git_branch='$(git branch 2> /dev/null | grep -e ^* | sed -E \"s/.* ([^ ]+)$/(\\1) /\")'
    local __guix_env=''
    if [ -n \"$GUIX_ENVIRONMENT\" ]; then
        __guix_env=\"$__guix_env_color[env]$__reset_color \"
    fi
    local __prompt_tail='$'
    local __xwindow_title=\"\\033]0;$__cur_location   — shell\\007\\r\"
    export PS1=\"${__xwindow_title}${__user_and_host} ${__cur_location} ${__git_branch_color}${__git_branch}${__reset_color}${__guix_env}${__prompt_tail} \"
}
setup_bash_prompt")

(define-public bashrc-helpers
  "\
function whereis_guix {
  if [[ -z \"$1\" ]]; then
    echo \"Argument is required\" >2
    return
  fi
  ls -l `which $1` | sed -r 's/.* (.+)\\/bin\\/[^\\/]+/\\1/'
}

export PATH=\"$HOME/bin:$HOME/.cargo/bin:$HOME/go/bin:$HOME/.local/bin:$PATH\"

# Именно такое значение выставляется в emacs (надо разобраться)
export LIBCLANG_PATH=\"$HOME/.guix-profile/lib/\"
export RUST_SRC_PATH=\"$HOME/.guix-profile/lib/rustlib/src/rust/library\"
export XDG_DATA_DIRS=\"/home/evgenii/.local/share/flatpak/exports/share:$XDG_DATA_DIRS\"

export LESSHISTFILE=\"$XDG_CACHE_HOME/.lesshst\"
")
