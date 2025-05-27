return {
  "kkoomen/vim-doge",
  build = "doge#install()",
  config = function()
    vim.keymap.set("n", "<leader>cd", "<Plug>(doge-generate)", { desc = "Create Documentation" })
  end,
}
