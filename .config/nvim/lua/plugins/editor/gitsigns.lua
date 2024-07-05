return {
  "lewis6991/gitsigns.nvim",
  config = function()
    require("gitsigns").setup({
      signs = {
        add = { text = "+" },
      },
      signs_staged = {
        add = { text = "+" },
      },
    })
  end,
}
