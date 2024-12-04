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

## Additional tools
* Emacs Plus - https://github.com/d12frosted/homebrew-emacs-plus
* Clipy - clipboard history manager - use `<CMD-SHIFT-v>` for pasting
* Fzf - fuzzy finder used by Telescope
