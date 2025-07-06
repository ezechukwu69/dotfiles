{ config, pkgs, ... }:
let 
  unstable = import <nixpkgs> {};
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "ezechukwu69";
  home.homeDirectory = "/home/ezechukwu69";
  nix = {
      package = pkgs.nix; # Or nixUnstable if needed
      settings = {
        experimental-features = [ "nix-command" "flakes" ];
      };
    };

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with unstable; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.neovide
    nettools
    devbox
    asdf
    gthumb
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/ezechukwu69/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "nvim";
  };

  programs.fastfetch = {
    enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    dotDir = ".config/zsh";
    # enableBashCompletion = true;
    syntaxHighlighting.enable = true;
    autocd = true;

    # Define custom aliases
    shellAliases = {
      dba = "devbox add";
      db = "devbox";
      dbi = "devbox init";
      dbr = "devbox rm";
      dbs = "devbox services";
      dbS = "devbox shell";
      gd="git diff";
      gc="git c";
      ga="git a";
      gl="git l";
      gs="git s";
      gp="git push";
      gu="git pull";
      gC="git clone";
      gi="git init";
      gap="git add -p";
      v="nvim";
      e="emacs";
      geminiai="aider --model gemini/gemini-2.0-flash --vim --no-attribute-author --no-attribute-committer --no-attribute-commit-message-author --no-attribute-commit-message-committer --watch-files --no-auto-commits --no-dirty-commits --edit-format diff-fenced --no-auto-lint --architect";
      jjw="watch -c -t -n 1 jj";
      nt="launch-new-zellij-tab";
      ls="eza";
      emacsn="emacs -nw";
      nv="NVIM_APPNAME=nvim-custom nvim";
      man="tldr";
      # zed="zeditor";
    };

    initExtra = ''
# export OLLAMA_NUM_GPU=999
# export SYCL_CACHE_PERSISTENT=1
# export SYCL_PI_LEVEL_ZERO_USE_IMMEDIATE_COMMANDLISTS=1
# export ONEAPI_DEVICE_SELECTOR=level_zero:0
export MANPAGER='nvim +Man!'
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/installations/ollama-ipex:$PATH"
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.npm-global:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/installations/flutter/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.config/nvim/bin:$PATH"
export PATH="$HOME/.cache/lm-studio/bin:$PATH"
# export PATH="$HOME/.local/share/gem/ruby/3.3.0/bin:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"
export PATH="$PATH":"$HOME/.local/share/gem/ruby/3.4.0/bin"
eval $(ssh-agent) 2> /dev/null
ssh-add ~/.ssh/id_ed25519 2> /dev/null
alias v="lvim"
alias et="emacs -nw"
export MOZ_ENABLE_WAYLAND=1
alias e="emacs"
eval "$(devbox global shellenv --init-hook)"
# alias zed="zeditor"
export BUNDLE_PATH=~/.gems
. /opt/asdf-vm/asdf.sh &> /dev/null
source ~/.zshrc_local &> /dev/null
if [ -f ~/.zshrc_local ]; then
  source ~/.zshrc_local
else
  touch ~/.zshrc_local
  source ~/.zshrc_local
fi
bindkey -v
function launch-new-zellij-tab {
  zellij action new-tab --layout default --cwd "$PWD" -n "$(basename "$PWD")"
}
export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense' # optional
zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
source <(carapace _carapace)
zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'
# fastfetch
    '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
