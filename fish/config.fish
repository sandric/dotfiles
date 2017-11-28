source ~/.config/fish/fzf.fish

alias l 'ls -lah'

set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --follow --glob "!.git/*"'

set -x FZF_DEFAULT_OPTS '--color fg:0,hl:9,fg+:0,bg+:#add8e6,hl+:9,spinner:0'

nvm use 9.0.0
status --is-interactive; and source (rbenv init -|psub)

function fish_title
 true
end

function fish_greeting
  true
end

function fish_prompt
  set_color green
  echo -n (prompt_pwd)
  set_color purple
  echo -n ' ^ '
end

set fish_color_search_match --background='add8e6'
set fish_pager_color_prefix red


if [ -n "$INSIDE_EMACS" ]
  source /home/sandric/.emacs.d/eterm.fish
end
