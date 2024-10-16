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
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH":"/usr/lib/wlroots0.17/pkgconfig"
export OPEN_API_KEY="key"
export GOOGLE_APPLICATION_CREDENTIALS="/home/ezechukwu69//firebase/pdetect.json"

#eval (ssh-agent -c)
