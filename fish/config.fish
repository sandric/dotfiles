source ~/.config/fish/fzf.fish

alias l 'ls -lah'

set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --follow --glob "!.git/*"'

set -x FZF_DEFAULT_OPTS '--color fg:0,hl:9,fg+:0,bg+:#add8e6,hl+:9,spinner:0'

set -x EDITOR "emacsclient -t"

nvm use 9.0.0
status --is-interactive; and source (rbenv init -|psub)

function emd
    emacs --daemon
end

function e
    emacsclient -c -n -a ""
end

function t
    emacsclient -c -n -a "" --eval '(progn (set-frame-name "rightframe"))'
end

function n
    emacsclient -c -n -a "" --eval '(progn (set-frame-name "leftframe"))'
end


function f
    emacsclient -c -n -a "" --eval "(progn (set-frame-name \"otherframe\") 
(sandric/tmux-rg \""$PWD"\"))"
end

function m
    emacsclient -c -n -a "" --eval "(progn (set-frame-name \"otherframe\") 
(sandric/tmux-man \""$argv[1]"\"))"
end

function h
    tmux capture-pane -S -32768
    tmux save-buffer /home/sandric/.tmux_history
    tmux delete-buffer

    emacsclient -c -n -a "" --eval "(progn (set-frame-name \"otherframe\") 
(sandric/tmux-history))"
end

function d
    emacsclient -c -n -a "" --eval "(progn (set-frame-name \"otherframe\") (sandric/tmux-dired \""$PWD"\"))"
end



function set_lastprocess --on-event fish_preexec
    printf '\ek%s\e\\' eval $argv[1] | sed -e 's/\s.*$//'
end

function unset_lastprocess --on-event fish_postexec
    printf '\ek%s\e\\' fish
end


function tmux_save_history
     tmux capture-pane -S -32768
     tmux save-buffer ~/tmux.history
     tmux delete-buffer
end

function tt
     tmux new-session -d
     tmux split-window -h
     tmux new-window 'fish'
     tmux new-window 'fish'
     tmux new-window 'fish'
     tmux new-window 'fish'
     htop
     tmux attach-session -d
end

set fish_color_search_match --background='add8e6'
set fish_pager_color_prefix red
