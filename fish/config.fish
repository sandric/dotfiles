source ~/.config/fish/fzf.fish

alias l 'ls -lah'

set -gx PATH $PATH '/usr/include/ImageMagick-6/'
set -gx PATH $PATH '/home/sandric/.yarn/bin'

set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --follow --glob "!.git/*"'
set -x FZF_DEFAULT_OPTS '--color fg:0,hl:9,fg+:0,bg+:#add8e6,hl+:9,spinner:0'

set -gx BROWSER "vivaldi"
set -x EDITOR "emacsclient -a \"\" -n"

set fish_pager_color_description black
set fish_color_search_match --background='add8e6'
set fish_pager_color_prefix red
set fish_color_selection --background=purple


# status --is-interactive; and source (rbenv init -|psub)
nvm use 9.3.0

function fish_prompt
  set_color blue
  echo -n (prompt_pwd)
  set_color purple
  echo -n ' ^ '
  set_color black
end

if [ -n "$INSIDE_EMACS" ]
  source /home/sandric/.emacs.d/eterm.fish
end

alias e="emacsclient -a \"\" -n"
abbr b="bundle exec rails"

function c
  xclip -sel clip < $argv[1]
end

function h
  tmux capture-pane -e -S -64000
  tmux save-buffer /home/sandric/.tmux_history
  tmux delete-buffer
		
  emacsclient -a "" --no-wait --eval "(sandric/tmux-show-history)" > /dev/null
  awesome-client 'switch_to_emacs()'
end

function m
  emacsclient -a "" --no-wait --eval "(man \""$argv[1]"\"))" > /dev/null
  awesome-client 'switch_to_emacs()'
end

function d
  emacsclient -a "" --no-wait --eval "(dired \""$argv[1]"\")" > /dev/null
  awesome-client 'switch_to_emacs()'
end

function f
  emacsclient -a "" --no-wait --eval "(counsel-rg \"\" \""$argv[1]"\")" > /dev/null
  awesome-client 'switch_to_emacs()'
end

function w
  emacsclient -a "" --no-wait --eval "(magit-status \""$argv[1]"\")"
  awesome-client 'switch_to_emacs()'
end
rvm default




setenv SSH_ENV $HOME/.ssh/environment

function start_agent
    echo "Initializing new SSH agent ..."
    ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
    echo "succeeded"
    chmod 600 $SSH_ENV 
    . $SSH_ENV > /dev/null
    ssh-add
end

function test_identities
    ssh-add -l | grep "The agent has no identities" > /dev/null
    if [ $status -eq 0 ]
        ssh-add
        if [ $status -eq 2 ]
            start_agent
        end
    end
end

if [ -n "$SSH_AGENT_PID" ] 
    ps -ef | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
    if [ $status -eq 0 ]
        test_identities
    end  
else
    if [ -f $SSH_ENV ]
        . $SSH_ENV > /dev/null
    end  
    ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep ssh-agent > /dev/null
    if [ $status -eq 0 ]
        test_identities
    else 
        start_agent
    end  
end
