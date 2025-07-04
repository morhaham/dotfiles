return {
  "nmac427/guess-indent.nvim",
  event = "BufReadPre", -- ensures it runs early
  config = function()
    require("guess-indent").setup {
      auto_cmd = true,               -- enables automatic guessing
      override_editorconfig = false -- optional: don’t override editorconfig
    }
  end,
}
