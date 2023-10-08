#!/usr/bin/env bash

set -o errexit # exit if a command exits
set -o pipefail # a pipe fails if any part fails
set -o noglob
#set -o nounset # no use of unset variables allowed


WORK_DIR=$(dirname "$0")
SESSION="rust-roguelike"

tmux kill-session -t $SESSION 2> /dev/null || true # cleanup old session

# create new detached session, with the terminals actual size know before attaching
# (for correct pane size percentages later)
tmux new-session -d -s $SESSION -x "$(tput cols)" -y "$(tput lines)" -c "$WORK_DIR" 

# tmux send-keys -t $SESSION:0 "cd $WORK_DIR" Enter

tmux split-window -v -t $SESSION:0 -l 30% -c "$WORK_DIR" # initial vertical split
tmux split-window -h -t $SESSION:0.1 -l 20% -c "$WORK_DIR" # horizontally split bottom pane

tmux send-keys -t $SESSION:0.0 "nix-shell --run $SHELL" Enter
#tmux send-keys -t $SESSION:0.0 "hx" Enter

tmux send-keys -t $SESSION:0.1 "nix-shell --run $SHELL" Enter

tmux send-keys -t $SESSION:0.2 "cpugraph" Enter

# tmux resize-pane -t $SESSION:0.1 -y 30%
# tmux resize-pane -t $SESSION:0.2 -x 20%

# if already in tmux
if [ -n "${TMUX}" ]; then
  tmux switch-client -t $SESSION:0.0
else
  tmux attach-session -t $SESSION:0.0
fi


