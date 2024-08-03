return {
  "tpope/vim-fugitive",
  config = function()
    vim.api.nvim_create_autocmd({ "User" }, {
      pattern = {"FugitiveCommit"},
      command = "set foldmethod=syntax",
    })
  end,
}
