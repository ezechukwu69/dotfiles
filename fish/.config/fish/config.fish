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
#eval (ssh-agent -c)
