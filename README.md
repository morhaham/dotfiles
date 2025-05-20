## Setup
1. install stow: 
    - `brew install stow`
2. create a `dotfiles` directory in `$HOME`
3. clone this repo into `$HOME/dotfiles`
4. inside the cloned repo run:
    - `stow .`

5. if there are conflicts due to existing files in `$HOME` you may run:
    - `stow --adopt .`
    * please read about this flag before use.

# Most Important Tools
    - Neovim
    - AeroSpace - tiling manager
    - Lazgit
    - Clipy - clipboard history manager - use `<CMD-SHIFT-v>` for pasting
    - Fzf - fuzzy finder used by Telescope

# Useful commands:
    - Dump packages to Brewfile
    - `brew bundle dump --force --file=~/dotfiles/mac/Brewfile`

# Install from Brewfile
    - `brew bundle install --file=~/dotfiles/mac/Brewfile`

