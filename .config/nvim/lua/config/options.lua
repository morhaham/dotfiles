vim.g.mapleader = " "
vim.g.maplocalleader = " "

local options = {
  termguicolors = true, -- Enable 24 bits color in terminal
  -- guicursor = "n-v-c-i:block", -- Set the gui cursor to nothing for each mode
  inccommand = "nosplit", -- Show effect of command incrementally while writing it (substitution)
  showmode = false, -- Don't need to show mode
  mouse = "", -- Disable mouse in all mode
  tabstop = 4, -- A tab is 2 spaces (display)
  shiftwidth = 4, -- Indent to 2 spaces
  softtabstop = 4, -- A tab is 2 spaces (insert mode)
  expandtab = true, -- Use spaces instead of tab character
  smartindent = true, -- Smart auto indent on new line
  cursorline = true, -- Highlight the current line the cursor is on
  hidden = true, -- Make sure hidden buffer are not unloaded
  backup = false, -- Disable file backups
  writebackup = false, -- Disable file backup on save
  swapfile = false, -- Disable swap files
  laststatus = 3, -- Single status line for the whole frame
  signcolumn = "yes", -- Show the sign colum (for gitsigns)
  number = true, -- Show line numbers
  relativenumber = true, -- Use relative line numbers
  scrolloff = 12, -- Keep at least 12 lines above the cursor.
  wrap = false, -- Disable line wrap
  -- colorcolumn = "120", -- Color the 120 character as a limit for line length
  completeopt = "menu,menuone,noselect", -- Completion options
  clipboard = "unnamedplus", -- Sync to system clipboard
  confirm = true, -- Confirm to save changes before exiting modified buffer
  conceallevel = 3, -- Hide * markup for bold and italic
  formatoptions = "jcroqlnt", -- tcqj
  grepformat = "%f:%l:%c:%m",
  grepprg = "rg --vimgrep",
  ignorecase = true, -- Ignore case
  smartcase = true, -- Don't ignore case when search includes capitals
  sessionoptions = "buffers,curdir,tabpages,winsize,help,globals,skiprtp,folds",
  shiftround = true, -- Round indent
  shortmess = vim.o.shortmess .. "WcCI", -- Don't show some warnings
  sidescrolloff = 8, -- Columns of context
  splitbelow = true, -- Put new windows below current
  splitright = true, -- Put new windows right of current
  splitkeep = "screen",
  virtualedit = "block", -- Allow cursor to move where there is no text in visual block mode
  wildmode = "longest:full,full", -- Command-line completion mode
  winminwidth = 5, -- Minimum window width
  allowrevins = true, -- Allow reverse insert mode with <C-/> (for rtl text)
  showtabline = 0, -- Hide tabline
  belloff = "all",
  -- winblend = 20,
  -- pumblend = 20,
  -- guifont = "FiraCode Nerd Font,VictorMono Nerd Font:h14"
  -- timeoutlen = 300, -- Time to wait for mapped sequence to complete
  -- winborder = "rounded",
    linespace = 6
}

if vim.g.neovide then
  vim.g.neovide_input_use_logo = false
  vim.g.neovide_input_macos_option_key_is_meta = "only_left"

  vim.g.neovide_window_blurred = false
  vim.g.neovide_floating_blur_amount_x = 0.0
  vim.g.neovide_floating_blur_amount_y = 0.0
  vim.g.neovide_normal_opacity = 1.0
  vim.g.neovide_opacity = 1.0
end

for opt, val in pairs(options) do
  vim.o[opt] = val
end
