if status is-interactive
    # Commands to run in interactive sessions can go here
end
starship init fish | source
set fish_greeting
source ~/.config/shell/aliasrc
fish_add_path ~/.local/bin/
fish_add_path ~/.cargo/bin/
fish_add_path ~/.cabal/bin/
fish_add_path ~/.ghcup/bin/
set EDITOR "/usr/bin/nvim"
fish_vi_key_bindings
