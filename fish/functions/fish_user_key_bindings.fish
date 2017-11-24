function fish_user_key_bindings
  bind -k f12 'fg; commandline -f repaint'

  bind -k f5 fzf-file-widget
  bind -k f6 fzf-history-widget
  bind -k f7 fzf-cd-widget

  if bind -M insert > /dev/null 2>&1
    bind -M insert \ct fzf-file-widget
    bind -M insert \cr fzf-history-widget
    bind -M insert \ec fzf-cd-widget
  end
end
