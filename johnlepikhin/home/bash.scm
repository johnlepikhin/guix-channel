;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (johnlepikhin home bash)
  #:export (bashrc-history
            bashrc-ssh-agent
            bashrc-bash-prompt
            bashrc-helpers
            bash_profile-helpers))

(define-public bashrc-history
  "\
shopt -s histappend
shopt -s cmdhist
export HISTIGNORE='pwd:clear:history'
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
    local __prompt_commands='$(history -a)'
    local __guix_env=''
    if [ -n \"$GUIX_ENVIRONMENT\" ]; then
        __guix_env=\"$__guix_env_color[env]$__reset_color \"
    fi
    local __prompt_tail='$'
    export PS1=\"${__prompt_commands}${__user_and_host} ${__cur_location} ${__git_branch_color}${__git_branch}${__reset_color}${__guix_env}${__prompt_tail} \"
}
setup_bash_prompt")

(define-public bash-helpers
"\
function whereis_guix {
  if [[ -z \"$1\" ]]; then
    echo \"Argument is required\" >2
    return
  fi
  ls -l `which $1` | sed -r 's/.* (.+)\\/bin\\/[^\\/]+/\\1/'
}

export PATH=\"$HOME/.cargo/bin:$HOME/go/bin:$HOME/.local/bin:$PATH\"

# temporary fix for https://issues.guix.gnu.org/63238
export LIBRARY_PATH=\"$HOME/lib:$LIBRARY_PATH\"

# OpenSSL configuration for Rust builds
export OPENSSL_DIR=\"$HOME/.guix-home/profile\"
export OPENSSL_LIB_DIR=\"$HOME/.guix-home/profile/lib\"
export OPENSSL_INCLUDE_DIR=\"$HOME/.guix-home/profile/include\"

# Именно такое значение выставляется в emacs (надо разобраться)
export XDG_DATA_DIRS=\"/home/evgenii/.local/share/flatpak/exports/share:$XDG_DATA_DIRS\"

export LESSHISTFILE=\"$XDG_CACHE_HOME/.lesshst\"
export EDITOR='emacsclient -c'

# Useless in exotic WMs like XMonad
export _JAVA_AWT_WM_NONREPARENTING=1

# Normalize PKG_CONFIG_PATH to prevent cargo rebuilds due to duplicates
# (Guix profile sourcing can add the same paths multiple times)
export PKG_CONFIG_PATH=$(echo \"$PKG_CONFIG_PATH\" | tr ':' '\\n' | awk '!seen[\$0]++' | tr '\\n' ':' | sed 's/:$//')
")
