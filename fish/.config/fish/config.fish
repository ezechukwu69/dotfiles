if status is-interactive
    # Commands to run in interactive sessions can go here
end

fzf --fish | source
starship init fish | source
zoxide init fish | source
export PATH="$PATH:/home/ezechukwu69/.dotnet/tools"
export PATH="$PATH:/home/ezechukwu69/installations/flutter/bin"
export PATH="$PATH:/home/ezechukwu69/.config/emacs/bin"
export JAVA_HOME="/opt/android-studio/jbr"
export CHROME_EXECUTABLE="google-chrome-stable"
export PATH="$PATH":"$HOME/.pub-cache/bin"
export PATH="$PATH":"$HOME/.local/bin"
export PATH="$HOME/.cargo/bin":"$PATH"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH":"/usr/lib/wlroots0.17/pkgconfig"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH":"/usr/lib64/pkgconfig"
export OPEN_API_KEY="key"
export GOOGLE_APPLICATION_CREDENTIALS="/home/ezechukwu69//firebase/pdetect.json"

#eval (ssh-agent -c)
# function fzf_complete
#     set -l cmdline (commandline)
#     # HACK: Color descriptions manually.
#     complete -C | string replace -r \t'(.*)$' \t(set_color $fish_pager_color_description)'$1'(set_color normal) \
#         | fzf -d \t -1 -0 --ansi --header="$cmdline" --height="80%" --tabstop=4 \
#         | read -l token
#     # Remove description
#     set token (string replace -r \t'.*' '' -- $token)
#     commandline -rt "$token"
# end

function clia
    read -l line
    commandline -a $line
    # commandline -a 
end

bind -M insert \et fuzzy_complete

function fuzzy_complete
    complete -C | sort -u | fzf --height 40% --multi --reverse -q (commandline -t) | cut --output-delimiter ' ' -f1 | sed s/-//g | clia
    commandline -f end-of-line
end

alias mysudo='sudo -E env "PATH=$PATH"'

# Added by LM Studio CLI tool (lms)
set -gx PATH $PATH /home/ezechukwu69/.cache/lm-studio/bin
