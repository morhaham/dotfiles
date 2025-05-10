return {
  "kkoomen/vim-doge",
  config = function()
    vim.keymap.set("n", "<leader>d", "<Plug>(doge-generate)")
  end,
}
