set -e (echo $EMACS_TERM_NAME)_PWD
set -xU (echo $EMACS_TERM_NAME)_PWD (echo $PWD)

function clear_to_end
  commandline (commandline --cut-at-cursor)
end
bind \cu clear_to_end

function set_eterm_pwd --on-variable PWD
  set -e (echo $EMACS_TERM_NAME)_PWD
  set -xU (echo $EMACS_TERM_NAME)_PWD (echo $PWD)
end
