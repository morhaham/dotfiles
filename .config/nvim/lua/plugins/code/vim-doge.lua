return {
  "kkoomen/vim-doge",
  build = "doge#install()",
  init = function()
    -- disable the default keymap
    vim.g.doge_mapping = ""
  end,
  config = function()
    vim.keymap.set("n", "<leader>cd", "<Plug>(doge-generate)", { desc = "Generate Documentation" })
  end,
}
