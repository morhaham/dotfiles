require("config.options")
require("config.keymaps")
require("config.autocmds")

vim.deprecate = function() end

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = {
    import = "plugins",
  },
  ui = {
    border = "rounded",
    backdrop = 100
  },
})
