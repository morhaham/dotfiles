return {
  "miversen33/sunglasses.nvim",
  config = function()
    require("sunglasses").setup({
      filter_type = "NOSYNTAX",
      filter_percent = 0.4
    })
  end,
}
